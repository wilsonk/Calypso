/**
 * std::queue example.
 *
 * Build with:
 *   $ ldc2 -L-lstdc++ queue.d
 */

modmap (C++) "<queue>";

import std.stdio, std.conv, std.string;
import (C++) std.queue;

void main()
{
    auto q = new queue!(char);
    writeln("queue compiles");
}
