/*
Author: Twan Roodenburg
Date: 22/02/2026
File: print_colors.hpp
Description: 
    the terminal color printing helper functions.
*/


#pragma once
#include <iostream>

/// @brief terminal text colors for printing colored output (e.g. error messages)
enum class Terminal_color {
    Default,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White
};

/// @brief sets the terminal text color for subsequent output (until reset to Default)
/// @param color the color to set the terminal text to
inline void set_terminal_color(Terminal_color color) {
    switch (color) {
        case Terminal_color::Red: std::cout << "\033[31m"; break;
        case Terminal_color::Green: std::cout << "\033[32m"; break;
        case Terminal_color::Yellow: std::cout << "\033[33m"; break;
        case Terminal_color::Blue: std::cout << "\033[34m"; break;
        case Terminal_color::Magenta: std::cout << "\033[35m"; break;
        case Terminal_color::Cyan: std::cout << "\033[36m"; break;
        case Terminal_color::White: std::cout << "\033[37m"; break;
        case Terminal_color::Default:
        default: std::cout << "\033[0m"; break;
    }
}


