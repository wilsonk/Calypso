/**
 * std::string example.
 *
 * Build with:
 *   $ ldc2 -L-lstdc++ string.d
 */

modmap (C++) "string";

import std.stdio, std.conv, std.string;
import (C++) std.basic_string;
import (C++) std._ : cppstring = string;

void main()
{
    auto s = new cppstring; // basic_string!char would do too

    immutable char[] charArray = "Haumea";
    s.reserve(charArray.length * 2);
    s.insert(0, charArray.ptr, charArray.length);

    writeln(to!string(s.c_str()));
    writeln("Size: ", s.size());
    writeln("Capacity: ", s.capacity());

    s.insert(0, "< ");
    s.push_back(' ');
    s.push_back('>');
    writeln(to!string(s.c_str()));

    s.clear();
}