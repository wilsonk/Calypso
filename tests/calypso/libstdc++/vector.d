/**
 * std::vector example.
 *
 * Build with:
 *   $ ldc2 -L-lstdc++ vector.d
 */

modmap (C++) "vector";

import std.stdio, std.conv, std.string;
import (C++) std.vector;

void main()
{
    auto v = new vector!char;

    v.reserve(10);

    foreach (c; "Europa")
        v.push_back(c);

    immutable term = '\0';
    v.push_back(term);

    string reconstruct;
    for (int i = 0; i < v.size(); i++)
        reconstruct ~= to!string(v.at(i));

    writeln(reconstruct);
}