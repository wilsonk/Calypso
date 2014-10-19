#include "stringref.h"

StringRef::StringRef(const char* str)
{
    assert(str);
    this->data = str;
    size = strlen(str);
}

StringRef& StringRef::operator=(const char *str)
{
    assert(str);
    this->data = str;
    size = strlen(str);
    return *this;
}
