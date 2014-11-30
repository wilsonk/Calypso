modmap (C++) "simplefunc.hpp";

import std.stdio;
import (C++) test._;
import (C++) test.testStruct;
import (C++) test.testClass;
import (C++) test.testInherit;
import (C++) test.anotherClass;
import (C++) test.testMultipleInherit;

void testEcho(int o) { writeln("Echo ", o); }

class testDClass
{
public:
    testStruct mycstruct;
    int test;

    this() { test = 78; mycstruct.f = 6.12; mycstruct.c = 'p'; }
}

void main()
{
    writeln("Simple C++ function call test");
    writeln(testFunc('a'));
    writeln(testFunc('b'));
    writeln(testFunc('c'));

    testEcho(5);

    testStruct t;
    t.f = 1.56;
    t.c = 'x';
    writeln(t.f);
    writeln(t.c);

    testDClass dd = new testDClass;
    dd.mycstruct.f = 9.51;
    dd.mycstruct.c = 'o';
    writeln(dd.mycstruct.f);

    testClass cls = new testInherit;
    cls.priv.f = 5.25;
    writeln("cls.priv.f = ", cls.priv.f);
    cls.n = 2;
    auto well = cls.echo(9, 8);
    writeln("cls.echo(9, 8) = ", well);
    writeln("cls.echo2(2.5) = ", cls.echo2(2.5));

    testMultipleInherit mul = new testMultipleInherit;
    mul.pointerToStruct = &t;
    writeln("mul.pointerToStruct = ", mul.pointerToStruct);
    writeln("mul.hello() = ", mul.hello(false));

    anotherClass ano = new testMultipleInherit;
    writeln("ano.hello() = ", ano.hello(false));
}
