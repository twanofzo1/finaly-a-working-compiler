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

# compiler structure
to see the inner workings of the compiler, [here](./architecture.md) is a document that describes the architecture of the compiler and how the different components interact with each other.