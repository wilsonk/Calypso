#include "import.h"

Modmap::Modmap(Loc loc, StringExp *arg)
    : Dsymbol(NULL)
{
    this->loc = loc;
    this->arg = arg;
}
