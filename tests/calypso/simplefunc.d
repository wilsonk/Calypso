modmap (C++) "simplefunc.hpp";

import std.stdio;
import (C++) test._;
import (C++) test.testStruct;

struct whyyyStruct
{
    int fuck;
    int you;
}

void main()
{
    writeln("Simple C++ function call test");
    writeln(testFunc('a'));
    writeln(testFunc('b'));
    writeln(testFunc('c'));

    testStruct t;
    t.f = 1.56;
    writeln(t.f);
//     t.c = 'x';
//     writeln(t.c);
}
