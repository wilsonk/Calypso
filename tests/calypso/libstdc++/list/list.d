/**
 * std::list example.
 *
 * Build with:
 *   $ ldc2 -L-lstdc++ list.d
 */

modmap (C++) "<list>";

import std.stdio, std.conv, std.string;
import (C++) std.list;

void main()
{
    auto l = new list!int;
    //auto l1 = new list!(4, 100);  // FAILURE
    //auto l2 = new list!(l);       // FAILURE

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

    /* FAILURE -- Iterators don't work yet
    auto j = l.begin();
    for (int i = 0; i < l.size(); i++) 
    {
      writeln(j++);
    }
    */
}