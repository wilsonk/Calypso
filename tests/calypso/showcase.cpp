#include "showcase.hpp"

namespace test
{

double testDoubleVar = 99.99;
testStruct testVar = { 42.24, 'c', 555 };

int testFunc(char c)
{
    if (c == 'a')
        return 2;
    else if (c == 'b')
        return 5;
    else
        return 8;
}

int testClass::echo(int a, int b)
{
    return 0;
}

float testClass::echo2(float f)
{
    return 0;
}

int testInherit::echo(int a, int b)
{
    return 42 * (a-b) * n;
}

float testInherit::echo2(float f)
{
    return 0.5 * f;
}

bool anotherClass::hello(bool who)
{
    return !who;
}

bool testMultipleInherit::hello(bool who)
{
    return anotherClass::hello(who);
}

}
