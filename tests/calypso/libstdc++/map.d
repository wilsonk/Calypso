/**
 * std::map example.
 *
 * Build with:
 *   $ ldc2 -L-lstdc++ map.d
 */

modmap (C++) "<map>";
modmap (C++) "<string>";

import std.stdio, std.conv, std.string;
import (C++) std.map;
import (C++) std._ : cppstring = string;

void main()
{
    auto m = new map!(char, cppstring);

    immutable char a = '0';
    m[a].__opAssign(new cppstring("Sedna"));
        // NOTE: since overloading the identify assignment is forbiddden in D, it's not possible to just do
        //      m[a] = new cppstring("Sedna");

    writeln(to!string(m[a].c_str));
}