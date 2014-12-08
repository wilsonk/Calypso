modmap (C++) "simplefunc.hpp";

import std.stdio;
import (C++) test._;
import (C++) test.testStruct;
import (C++) test.testClass;
import (C++) test.testInherit;
import (C++) test.anotherClass;
import (C++) test.testMultipleInherit;
import (C++) test.enumTest;

void testEcho(int o) { writeln("Echo ", o); }

class testDClass
{
public:
    testStruct mycstruct;
    int test;

    this() { test = 78; mycstruct.f = 6.12; mycstruct.c = 'p'; }
}

class DInherit : testDClass
{
public:
    float floatArray[5];
}

class DCXXclass : testMultipleInherit
{
public:
    uint someUint = 9;

    override bool hello(bool who)
    {
        if (someUint > 5)
            who = !who;

        return who;
    }

    static bool thunkTest(testMultipleInherit cppthis, bool who)
    {
       void* __tmp1723 = cast(void*) cppthis;
        __tmp1723 += -16L;
        return (cast(DCXXclass)__tmp1723).hello(who);
    }
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

    auto dd = new testDClass;
    dd.mycstruct.f = 9.51;
    dd.mycstruct.c = 'o';
    writeln(dd.mycstruct.f);

    auto yo = new DInherit;

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

    DCXXclass isThisRealLife = new DCXXclass;
    isThisRealLife.someUint = 187;
    writeln("isThisRealLife.someUint = ", isThisRealLife.someUint);
    isThisRealLife.pointerToStruct = &t;
    writeln("isThisRealLife.pointerToStruct = ", isThisRealLife.pointerToStruct);
    writeln("isThisRealLife.hello() = ", isThisRealLife.hello(true));

    testMultipleInherit testCast = isThisRealLife;
    writeln("thunkTest.hello() = ", DCXXclass.thunkTest(testCast, true));
    writeln("testCast.hello() = ", testCast.hello(true));
    writeln("testCast.echo2() = ", testCast.echo2(3));

    enumTest someEnumValue = enumTest.ENUM_SOMEVAL;
    writeln("someEnumValue = ", someEnumValue);
}
