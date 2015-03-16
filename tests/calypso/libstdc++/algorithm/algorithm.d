/**
 * std::algorithm example.
 *
 * Build with:
 *   $ ldc2 -L-lstdc++ algorithm.d
 */

modmap (C++) "<algorithm>";

import std.stdio;
import (C++) std._;

void main()
{
    writeln("algorithm compiles");

}