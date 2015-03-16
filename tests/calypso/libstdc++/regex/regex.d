/**
 * std::regex example.
 *
 * Build with:
 *   $ ldc2 -L-lstdc++ regex.d
 */

modmap (C++) "<regex>";
modmap (C++) "<string>";  // fails because of -std=c++11!!!

import std.string;
import (C++) std.regex;
import (C++) std.basic_string;
import (C++) std._ : cppstring = string;

void main()
{
    
}