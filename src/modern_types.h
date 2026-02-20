/*
Author: Twan Roodenburg
Date: 12/01/2026
File: modern_types.h
Description: 
    A header i made for more convenient type names, and also a byte union for easy bit manipulation of bytes, 
    this is purely for convenience and readability, and has no functionality beyond that.
    can be used for c and c++ code.
*/

#pragma once

#ifdef __cplusplus
    #include <cstdint>
#else
    #include <stdint.h>
#endif

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int8_t   i8;
typedef int16_t  i16;
typedef int32_t  i32;
typedef int64_t  i64;
typedef float    f32;
typedef double   f64;

typedef union {
    u8 full;
    struct {
        bool b0;
        bool b1;
        bool b2;
        bool b3;
        bool b4;
        bool b5;
        bool b6;
        bool b7;
    } bits;
} byte;

