/**
 * std::set example.
 *
 * Build with:
 *   $ ldc2 -L-lstdc++ set.d
 */

modmap (C++) "<set>";

import std.stdio, std.conv, std.string;
import (C++) std.set;

void main()
{
    auto v = new set!char;
    writeln("set compiles");
}