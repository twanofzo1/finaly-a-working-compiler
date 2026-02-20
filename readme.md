# Compiler

This is a compiler for a simple programming language that I am developing. It is written in C++ and compiles to x86-64 assembly (AT&T syntax, System V ABI, targeting Linux)


# Syntax
the language is a zig inspired language and you may see many similarities between zig and this language

## variable declaration and assignment
```go
var x := 10; 
var y: f64 = 3.14; 
const z: bool = true; 
const a := 5; 
const str := "Hello, World!";
```

## function declaration and calling
```go
fn add(a: i32, b: i32) : i32 {
    return a + b;
}
fn main() {
    var x := add(1, 2);
    return x;
}
```

## control flow
```go
fn main() {
    var x := 10;
    if x > 5 {
        return 1;
    } else {
        return 0;
    }
}
```

## loops
```go
fn main() {
    for {
        // infinite loop
    }
    for 10 {
        // loop 10 times
    }
    for i in 0..10 {
        // loop from 0 to 9
    }
}
```

## strings
```go  
fn main() {
    var s := "Hello, World!";
    s = s + " How are you?";
    print(s);
}
```

# building

to build the compiler, you need to have a C++ compiler that supports C++17 or later. You can use any C++ compiler

## release build
```bash
./build.sh release 
```
```bash
mkdir build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
make
```

## debug build
```bash
./build.sh debug
```
or
```bash
mkdir build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Debug
make
```

## running the compiler
to run the compiler, you can use the following command:
```bash
./bin/compiler <source_file>
```
This will compile the source file and output an assembly file with the same name but with a .s extension. You can then assemble and link the assembly file using gcc:
```bash
gcc <assembly_file.s> -o <output_executable>
```
or you can use the provided run script to compile and run the source file in one step:
```bash
./run.sh <source_file>
```


# compiler structure
to see the inner workings of the compiler, [here](./architecture.md) is a document that describes the architecture of the compiler and how the different components interact with each other.