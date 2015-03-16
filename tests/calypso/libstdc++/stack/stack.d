/**
 * std::stack example.
 *
 * Build with:
 *   $ ldc2 -L-lstdc++ stack.d
 */

modmap (C++) "<stack>";
modmap (C++) "<vector>";
modmap (C++) "<deque>";

import std.stdio;
import (C++) std.stack;
import (C++) std.vector;
import (C++) std.deque;

void main()
{
    auto dq = new deque!(int);
    //auto v = new vector!(int)(2,200);   //FAILURE

    auto st1 = new stack!char;
    auto dq2 = new stack!(int)(dq);

    writeln("stack appears to work");
}