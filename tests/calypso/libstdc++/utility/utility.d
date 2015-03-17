/**
 * std::utility example.
 *
 * Build with:
 *   $ ldc2 -L-lstdc++ utility.d
 */

modmap (C++) "<utility>";

import std.stdio;
import (C++) std._ : stlpair = pair;
import (C++) std._ : stlmakepair = make_pair;

void main()
{
    writeln("utility compiles");

    auto p = new stlpair!(int, int);
    p = stlmakepair!(int,int)(10,20);
    writeln("first in pair = ", p.first);
    writeln("second in pair = ", p.second);
}
