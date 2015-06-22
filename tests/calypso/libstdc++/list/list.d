/**
 * std::list example.
 *
 * Build with:
 *   $ ldc2 -L-lstdc++ list.d
 */

modmap (C++) "<list>";

import std.stdio, std.conv, std.string;
import (C++) std.list;

bool single_digit (const int value) { return (value<10); }

struct is_odd {
  bool opCall (const int value) { return (value%2)==1; }
};

void main()
{
    auto l = new list!int;
    //auto l1 = new list!(4, 100);  // FAILURE
    auto l2 = new list!int(*l);
    auto l3 = new list!int;
    auto l4 = new list!int;

    immutable int x = 11;
    immutable int y = 4;
    immutable int z = 3;
    l.push_back(x);
    l.push_back(y);
    l.push_back(z);
    writeln("List front is ", l.front());

    //l.unique(3);        // FAILURE

    l.sort();
    writeln("List front after sort is ", l.front());

    // FAILURE: the next line causes k++ to fail if the iterator
    // isn't explicitly initialized like below!
    // auto k = l.begin();

    list!(int).iterator k;

    // for(; k != l.end(); k++)
    // FAILURE: classes.cpp:256: DValue* DtoCastClass(Loc&,
    // DValue*, Type*): Assertion `to->ty == Tclass' failed


    writeln("\nList:");
    // So we have to use this more laboured method below for now
    k = l.begin;
    for (int i=0; i < l.size() ; k++, i++)
      writeln(*k);

    writeln("\nList2 after swapping in List:");
    // swap works
    l2.swap(*l);
    k = l2.begin;
    for (int i=0; i < l2.size() ; k++, i++)
      writeln(*k);

    l3.assign(l2.begin(), l2.end());

    writeln("\nList4 after assign from array:");
    auto arr = [7, 64, 9, 22, 4];
    l4.assign(arr.ptr, arr.ptr+5);
    k = l4.begin;
    for (int i=0; i < l4.size() ; k++, i++)
      writeln(*k);


    writeln("\nList4 after single_digits removed:");
    l4.remove_if(&single_digit);
    k = l4.begin;
    for (int i=0; i < l4.size() ; k++, i++)
      writeln(*k);

    // FAILURE: unhandled D->C++ conversion
    /*is_odd io;
    bool delegate(const int) iof;
    iof = &io.opCall;
    l4.remove_if(iof);*/

}
