/**
 * std::set example.
 *
 * Build with:
 *   $ ldc2 -L-lstdc++ set.d
 */

modmap (C++) "<set>";

import std.stdio, std.conv, std.string;
import (C++) std.set;
import (C++) std.multiset;

void main()
{
    auto s = new set!char;
    auto ms1 = new multiset!int;
    auto ms2 = new multiset!int;

    multiset!(int).iterator it;

    int[] ints = [1,9,55,77,99,33];

    //auto ms3 = new multiset!(int)(ints, ints.ptr+3);  // Unhandled D->Clang conversion
    //auto ms3 = new multiset!(int)(ints, ints.ptr+3);  // non-canonical or dependent type in IR-generation

    const int x = 7;
    //ms21.insert(it, x);    // doesn't work, assert in dmd

    for (int i = 0; i < 5; i++)
        ms1.insert(i);

    it = ms1.begin();
    for (int i = 0; i < 5; i++, it++)
        writeln("multiset1 = ", *it);

    ms2.insert(ints.ptr, ints.ptr+3);
    it = ms2.begin();
    for (int i = 0; i < 3; i++, it++)
        writeln("multiset2 after ranged insert = ", *it);

    auto kcomp = ms1.key_comp();
    auto rit = ms1.rbegin();
    auto highest = *rit;

    do {
        writeln("multiset1 using compare and reverse iterator = ", *rit);
    } while ( kcomp(*rit++, highest) );


    writeln("set compiles");
}