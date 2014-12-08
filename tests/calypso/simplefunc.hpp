#pragma once

namespace test {
    int testFunc(char c);

    struct testStruct
    {
        float f;
        char c;
        unsigned n;
    };
    
    class testClass
    {
    protected:
        testStruct priv;

    public:
        unsigned n;
        virtual int echo(int a, int b);
        virtual float echo2(float f);
    };
    
    class testInherit : public testClass
    {
    public:
        float f;
        char c;

        int echo(int a, int b) override;
        virtual float echo2(float f) override;
    };

    class anotherClass
    {
    public:
        int number;
        float floating;
        double doubling;

        virtual bool hello(bool who);
    };

    class testMultipleInherit : public anotherClass,
        public testInherit
    {
    public:
        testStruct *pointerToStruct;

        virtual bool hello(bool who) override;
    };

    enum enumTest
    {
        ENUM_FIRSTVAL = 2,
        ENUM_SOMEVAL,
        ENUM_LASTVAL
    };
}
