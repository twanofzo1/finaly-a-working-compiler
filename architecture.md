# Compiler Architecture & Control Flow

> Developer reference for the internal workings of this compiler.
> The compiler reads a custom language (Zig-inspired syntax) and produces x86-64 Linux assembly (AT&T syntax).

---

## High-Level Pipeline

```
Source file (.txt)
       │
       ▼
  ┌─────────┐
  │  Lexer   │  ── tokenises raw text into Token[]
  └────┬─────┘
       │  vector<Token>
       ▼
  ┌─────────┐
  │  Parser  │  ── builds an Abstract Syntax Tree (AST)
  └────┬─────┘
       │  AST
       ▼
  ┌──────────────────┐
  │ Semantic Analyser │  ── validates types, scopes, signatures
  └────┬─────────────┘
       │  (same AST, now verified)
       ▼
  ┌──────────────┐
  │ IR Generator │  ── lowers AST into a flat IR (SSA-like)
  └────┬─────────┘
       │  IR_Program
       ▼
  ┌──────────────┐
  │ IR Optimiser │  ── runs optimisation passes until fixpoint
  └────┬─────────┘
       │  IR_Program (optimised)
       ▼
  ┌─────────┐
  │ Codegen │  ── emits x86-64 AT&T assembly
  └────┬────┘
       │  string → output.s
       ▼
   Assembly file
```

The **main.cpp** drives this pipeline in exact order:

1. Read source file into a `std::string`
2. Strip `\r` for consistent line handling
3. Lex → parse → analyse → IR generate → optimise → codegen
4. Write assembly to `output.s`

Each stage bails with an error message if it fails; later stages are only reached if all previous stages succeeded.

---

## Utility Headers

### `modern_types.h`
Defines short typedefs for fixed-width types (`u8`, `u16`, `u32`, `u64`, `i8`–`i64`, `f32`, `f64`) and a `byte` union with bit-access. Used everywhere for consistent sizing.

### `log.hpp`
Debug-only macros behind `#ifndef NDEBUG`:
- `LOG(msg)` — blue info message
- `WARNING(msg)` — yellow warning
- `ERROR(msg)` — red error
- `ASSERT(cond, msg)` — terminates with file/line info on failure

All macros compile to nothing in release builds.

---

## 1 — Lexer (`lexer.hpp` / `lexer.cpp`)

### Purpose
Converts a raw source string into a flat `vector<Token>`.

### How it works
- Uses a **state machine** with four states: `Start`, `Number`, `Identifier`, `Other`.
- Walks the input character by character (`m_pos`, `m_current`).
- `Start` examines the current char to move into the appropriate state.
- `Number` handles integer and float literals (detects `.` for floats).
- `Identifier` reads alphanumeric + `_` runs, then checks against a keyword map (`fn`, `if`, `else`, `for`, `return`, `true`, `false`, `var`, `const`). If matched → keyword token, else → `Identifier` token.
- `Other` matches operators & punctuation. Multi-char operators (`==`, `!=`, `<=`, `>=`, `&&`, `||`, `<<`, `>>`, `->`, compound-assign operators) use `peek()` to look ahead.

### Key members
| Member | Role |
|---|---|
| `m_pos` | Current index in source string |
| `m_current` | Character at `m_pos` |
| `m_state` | Which lexer state we're in |
| `m_tokens` | Output token list |
| `m_has_error` | Error flag |

### Token
Each `Token` stores a `Token_type` enum plus a `std::string_view` into the original source. This allows error messages to point back to exact source positions without copying strings.

Tokens support `get_line()`, `get_pos()`, and `print_line()` for error reporting — they compute their location by scanning back through the source string.

### Token types
Covers: literals (`Integer`, `Float_literal`, `String_literal`), identifiers, keywords (`If`, `Else`, `For`, `Return`, `True`, `False`, `Function`, `Var`, `Const`), all arithmetic/comparison/logical/bitwise operators, compound-assignment operators, brackets, braces, semicolons, and `End_of_file`.

---

## 2 — Parser (`parser.hpp` / `parser.cpp`)

### Purpose
Consumes the token stream and builds an **AST** (Abstract Syntax Tree).

### How it works
- **Recursive descent** parser.
- Initialises with a root `Block_statement` at index 0 in the AST.
- `parse()` loops over tokens, calling `parse_statement()` for each top-level statement (currently only `Function_declaration` at top level).

### Parsing functions
| Function | Handles |
|---|---|
| `parse_primary()` | Integers, floats, identifiers, parenthesised exprs, unary prefix |
| `parse_expression()` | Entry for expression parsing — calls `parse_primary()` then `parse_binary_expression()` |
| `parse_binary_expression(lhs, min_prec)` | **Pratt parser** — operator-precedence climbing. Loops while the next operator has higher precedence than `min_prec`, recursing for right-associativity |
| `parse_identifier()` | Plain identifier or call expression (if followed by `(`) |
| `parse_call_expression()` | `name(arg1, arg2, ...)` |
| `parse_datatype()` | Zig-style types: `i32`, `u8`, `f64`, `bool`, `void`, arbitrary widths |
| `parse_function_declaration()` | `fn name(type param, ...) : return_type { body }` |
| `parse_variable_declaration(is_const)` | `var x : type = expr;` or `var x := expr;` (inferred) |
| `parse_if_statement()` | `if (cond) { ... } else { ... }` |
| `parse_for_statement()` | `for (init; cond; post) { body }` |
| `parse_return_statement()` | `return expr;` or `return;` |
| `parse_block_statement()` | `{ stmt; stmt; ... }` |

### Operator precedence
Follows C-style precedence (high → low):
`*` `/` `%` → `+` `-` → `<<` `>>` → `<` `>` `<=` `>=` → `==` `!=` → `&` → `^` → `|` → `&&` → `||`

### Assignment handling
After parsing a full expression, the parser checks if the next token is an assignment operator (`=`, `+=`, etc.). If so, it wraps the expression in an `Assignment_expression` node.

---

## 3 — AST (`ast.hpp` / `ast.cpp`)

### Purpose
The data structure that represents the parsed program as a tree.

### Design: Index-based node arena
Instead of heap-allocated tree nodes with pointers, the AST uses **parallel vectors** — one per node type:

```
AST
├── integers[]              + integer_tokens[] (parallel)
├── floats[]                + float_tokens[]   (parallel)
├── identifiers[]           + identifier_tokens[] (parallel)
├── datatypes[]
├── binary_expressions[]
├── unary_expressions[]
├── assignment_expressions[]
├── block_statements[]
├── if_statements[]
├── for_statements[]
├── function_declarations[]
├── return_statements[]
├── variable_declarations[]
└── call_expressions[]
```

Nodes reference each other via **`AST_index`** — a tagged struct containing:
- `AST_index_type type` — which vector to look in
- `u32 index` — position within that vector

This is efficient, cache-friendly, and avoids pointer-chasing.

### Node types
| Node | Children |
|---|---|
| `Binary_expression` | `lhs`, `opp` (operator), `rhs` |
| `Unary_expression` | `opp`, `operand` |
| `Assignment_expression` | `target`, `opp`, `value` |
| `Block_statement` | `statements[]` |
| `If_statement` | `condition`, `true_condition`, `false_condition` |
| `For_statement` | `init`, `condition`, `post`, `block` |
| `Function_declaration` | `identifier`, `datatypes[]`, `names[]`, `block`, `return_type` |
| `Return_statement` | `value` |
| `Variable_declaration` | `name`, `datatype`, `value`, `is_const` |
| `Call_expression` | `callee`, `arguments[]` |
| `Datatype` | `kind` (enum), `bit_width` (allows arbitrary widths like `u3`, `i24`) |

### Debug printing
All nodes have a `print()` method (behind `#ifndef NDEBUG`) that recursively prints the tree with indentation.

---

## 4 — Semantic Analyser (`semantics.hpp` / `semantics.cpp`)

### Purpose
Validates the AST before lowering to IR: checks types, scopes, redeclarations, and function signatures.

### How it works — two passes

**Pass 1: `pass1_collect_functions()`**
- Scans top-level block for `Function_declaration` nodes.
- Registers each function's name, parameter types, and return type into the global scope.
- This enables **forward references** — functions can call other functions declared later in the file.

**Pass 2: `analyse_block(0)` (recursive)**
- Walks every node in the AST recursively.
- For each node type, calls the appropriate `analyse_*` method.

### Scope management
- Uses a `vector<Scope>` as a scope stack.
- `push_scope()` / `pop_scope()` on block entry/exit.
- `declare()` adds a symbol to the current scope (errors on redeclaration).
- `lookup()` walks from innermost to outermost scope to find a symbol.

### What it checks
| Check | Detail |
|---|---|
| Undeclared variables | `lookup()` returns `nullptr` → error |
| Redeclaration | Same name declared twice in the same scope |
| Type matching | Binary ops, assignments, return types must be compatible |
| Function calls | Argument count and types must match the declaration |
| Return type | The returned expression's type must match the function's declared return type |
| Const assignment | Cannot assign to a `const` variable |

### Symbol table
```cpp
struct Symbol {
    Symbol_type type;       // Variable or Function
    AST_index datatype;     // resolved Datatype for variables
    bool is_const;
    // Function-specific:
    vector<AST_index> param_types;
    AST_index return_type;
    u32 param_count;
};
```

### Error reporting
Errors include ANSI colour codes (red) and, when a token is available, print the offending source line with a marker.

---

## 5 — IR Generator (`ir.hpp` / `ir.cpp`)

### Purpose
Lowers the validated AST into a flat, linear **Intermediate Representation** — a list of instructions per function.

### IR design

**`IR_Type`** — mirrors `Datatype` (`kind` + `bit_width`), with a `to_string()` helper.

**`IR_Instruction`** — a single operation:
| Field | Usage |
|---|---|
| `op` | Opcode (see below) |
| `dst` | Destination virtual register |
| `src1`, `src2` | Source registers |
| `imm` / `fimm` | Immediate values |
| `label_id` | Label/jump target |
| `true_label`, `false_label` | Branch targets |
| `func_name` | Call target |
| `args` | Call arguments (list of registers) |
| `type` | Result type |

**`IR_Function`** — `name`, `return_type`, `params[]`, `instructions[]`

**`IR_Program`** — `functions[]`

### Opcodes
| Category | Ops |
|---|---|
| Constants | `Const_int`, `Const_float` |
| Memory | `Alloca`, `Load`, `Store` |
| Arithmetic | `Add`, `Sub`, `Mul`, `Div`, `Mod` |
| Unary | `Neg`, `Bitwise_not` |
| Comparison | `Eq`, `Neq`, `Lt`, `Gt`, `Le`, `Ge` |
| Bitwise | `And`, `Or`, `Xor`, `Shl`, `Shr` |
| Logical | `Logical_and`, `Logical_or`, `Logical_not` |
| Control flow | `Label`, `Jump`, `Branch` |
| Functions | `Ret`, `Call` |

### Generation strategy
- One `IR_Function` per function declaration.
- **Variables** → `Alloca` + `Store`/`Load` (stack-slot model, like LLVM's `-O0`).
- **Expressions** → evaluated into virtual registers (`%0`, `%1`, …).
- **If statements** → `Branch` on condition, `Label` for true/false branches, `Jump` to merge.
- **For loops** → `Label` for header, `Branch` for condition check, `Label` for body, `Jump` back, `Label` for exit.
- **Function calls** → `Call` instruction with argument registers.
- Virtual register counter (`m_next_reg`) and label counter (`m_next_label`) are monotonically increasing.

### Variable scopes in IR
Uses `vector<unordered_map<string, IR_Reg>> m_var_scopes` — mirrors the semantic analyser's scope stack but maps names → alloca register.

---

## 6 — IR Optimiser (`optimiser.hpp` / `optimiser.cpp`)

### Purpose
Runs multiple optimisation passes over the IR to eliminate redundancy and simplify code.

### Strategy: iterate until fixpoint
```
for each function:
    while (any pass makes changes):
        run all passes
```
Capped at 100 iterations as a safety limit.

### Passes (in execution order)

| Pass | What it does |
|---|---|
| **Constant folding** | If both operands of an arithmetic/comparison op are constants, evaluate at compile time and replace with a single `Const_int` |
| **Constant propagation** | Track which registers hold known constants; replace uses of those registers with the constant value |
| **Store-load forwarding** | If `store %v → %slot` is immediately followed by `load %slot`, replace the load's uses with `%v` |
| **Dead store elimination** | Remove stores to a slot that is overwritten again before being read |
| **Branch folding** | If a branch's condition is a known constant, replace with an unconditional `Jump` |
| **Unreachable code elimination** | Remove instructions after an unconditional `Jump` or `Ret` until the next `Label` |
| **Redundant jump elimination** | Remove `Jump Lx` when `Lx:` is the very next instruction |
| **Dead code elimination** | Remove instructions whose destination register is never read (except those with side effects like `Store`, `Call`, `Ret`) |

### Side-effect awareness
The `has_side_effects()` helper prevents DCE from removing `Store`, `Call`, `Ret`, and control-flow instructions.

---

## 7 — Code Generator (`codegen.hpp` / `codegen.cpp`)

### Purpose
Translates the optimised IR into x86-64 assembly (at&t syntax, system v abi, targeting linux).

### Strategy: stack-based register allocation
Every virtual register is assigned a **stack slot** (offset from `%rbp`). All operations go through memory:

```
load vreg from stack → hardware register
operate
store result → stack slot
```

This is intentionally simple (no register allocation). The optimiser compensates by reducing the number of virtual registers.

### Stack layout
```
High addresses
  ┌────────────────────┐
  │ return address      │  ← pushed by call
  │ saved %rbp          │  ← push %rbp
  ├────────────────────┤ ← %rbp
  │ vreg %0             │  -8(%rbp)
  │ vreg %1             │  -16(%rbp)
  │ vreg %2             │  -24(%rbp)
  │ ...                 │
  └────────────────────┘ ← %rsp (aligned to 16)
Low addresses
```

### Function prologue / epilogue
```asm
push %rbp
mov %rsp, %rbp
sub $N, %rsp          # N = stack frame size (16-aligned)
# ... function body ...
mov %rbp, %rsp
pop %rbp
ret
```

### Calling convention (System V ABI)
- **Integer args**: `%rdi`, `%rsi`, `%rdx`, `%rcx`, `%r8`, `%r9` (first 6), then stack.
- **Float args**: `%xmm0`–`%xmm7` (first 8), then stack.
- **Return value**: `%rax` (integer) or `%xmm0` (float).
- Caller stores args to param stack slots on function entry.

### Float support
- Float constants go into a `.rodata` section as labelled 64-bit values.
- Float operations use SSE2 instructions (`movsd`, `addsd`, `subsd`, `mulsd`, `divsd`, `ucomisd`).
- Float/int distinction is tracked per virtual register via `m_reg_types`.

### Instruction emission
Each `IR_Instruction` maps to a small sequence of x86 instructions. The `emit_instruction()` method is a large switch over `IR_Op`:

| IR Op | x86 Output |
|---|---|
| `Const_int` | `movq $imm, slot(%rbp)` |
| `Const_float` | `movsd .LF_n(%rip), %xmm0` then store |
| `Alloca` | Slot already reserved — no-op |
| `Load` | Load from source slot, store to dest slot |
| `Store` | Load value, store to target slot |
| `Add/Sub/Mul/Div/Mod` | Load operands → compute → store result |
| `Neg` | `negq` |
| `Eq/Neq/Lt/...` | `cmpq` + conditional `set*` |
| `Branch` | `cmpq $0` + `jne`/`je` |
| `Jump` | `jmp .L_n` |
| `Label` | `.L_n:` |
| `Call` | Move args into ABI registers, `call`, store `%rax`/`%xmm0` |
| `Ret` | Load return value into `%rax`/`%xmm0`, epilogue, `ret` |

### Size awareness
Operations use the correct instruction suffix (`b`/`w`/`l`/`q`) and register name (`al`/`ax`/`eax`/`rax`) based on the type's bit width.

---

## Build System

### CMakeLists.txt
Standard CMake project. Compiles all `src/*.cpp` files into a single executable (`MyExecutable`) placed in `bin/`.

### Shell scripts
| Script | Purpose |
|---|---|
| `build.sh` | Runs CMake build |
| `compile_asm.sh` | Assembles + links `output.s` via `gcc` |
| `run.sh` | Likely combines build + run steps |

### Debug vs Release
- Debug builds (`-DNDEBUG` **not** set) enable all `LOG`, `WARNING`, `ERROR`, `ASSERT` macros and AST/IR print output.
- Release builds compile all debug macros to nothing.

---

## Language Features (current)

| Feature | Syntax |
|---|---|
| Functions | `fn name(type param, ...) : return_type { ... }` |
| Variables | `var x : i32 = 5;` or `var x := 5;` (type inferred) |
| Constants | `const x : i32 = 5;` |
| Types | `i8`–`i64`, `u8`–`u64`, `f32`/`f64`, `bool`, `void`, arbitrary widths |
| If/else | `if (cond) { ... } else { ... }` |
| For loops | `for (init; cond; post) { ... }` |
| Return | `return expr;` |
| Operators | Full set: arithmetic, comparison, logical, bitwise, compound assignment |
| Function calls | `name(arg1, arg2, ...)` |

---

## Data Flow Summary

```
Source string
  │
  ├─ Lexer reads chars, emits Token[] (string_views into source)
  │
  ├─ Parser consumes Token[], pushes nodes into AST vectors,
  │  links them via AST_index
  │
  ├─ Semantic Analyser walks AST with scope stack,
  │  validates types/decls, errors on mismatch
  │
  ├─ IR Generator walks AST, emits IR_Instruction[] per function
  │  (alloca/load/store model for variables)
  │
  ├─ IR Optimiser iterates passes on IR_Instruction[],
  │  folds constants, eliminates dead code, simplifies branches
  │
  └─ Codegen maps IR_Instruction[] → x86-64 asm string,
     one stack slot per virtual register, System V ABI
```

---

## Tips for Contributors

- **Adding a new AST node**: add the struct, a new `AST_index_type` enum value, a new vector in `AST`, then handle it in the parser, semantic analyser, IR generator, and codegen.
- **Adding a new optimisation pass**: add a method to `IR_Optimiser`, call it from `run_pass()`. Return `true` if anything changed.
- **Adding a new operator**: add the `Token_type`, handle in the lexer's `Other` state, add precedence in the parser, handle in semantic analysis, add the `IR_Op`, and emit the right x86.
- **Testing**: write a `.txt` source file, compile with the compiler, assemble with `compile_asm.sh`, run the binary.
- **Debug output**: build in debug mode to see token lists, AST printouts, IR before/after optimisation, and final assembly all printed to stdout.
