/*
Author: Twan Roodenburg
Date: 18/02/2026
File: log.hpp
Description: 
    The logging system for the program, 
    used for printing debug messages and error messages to the console.
    The logging system is disabled in release builds.
*/


#pragma once

#include <iostream>

#ifndef NDEBUG
    /// @brief prints a log message in blue with note:`LOG:, disabled in release builds`
    #define LOG(msg) std::cout << "\033[34mLOG: " << msg << "\033[0m" << std::endl
    /// @brief prints a log message in yellow with note:`Warning:, disabled in release builds`
    #define WARNING(msg) std::cout << "\033[33mWARNING: " << msg << "\033[0m" << std::endl
    /// @brief prints a log message in red with note:`Error:, disabled in release builds`
    #define ERROR(msg) std::cout << "\033[31mERROR: " << msg << "\033[0m" << std::endl
    /// @brief checks a condition and prints an error message and terminates the program if the condition is false, disabled in release builds`
    #define ASSERT(condition, msg) \
        do { \
            if (!(condition)) { \
                std::cerr << "\033[31mASSERTION FAILED: " << msg \
                          << "\nFile: " << __FILE__ \
                          << "\nLine: " << __LINE__ \
                          << "\033[0m" << std::endl; \
                std::terminate(); \
            } \
        } while (false)
#else
    #define LOG(msg)
    #define WARNING(msg)
    #define ERROR(msg)
    #define ASSERT(condition,msg)
#endif

