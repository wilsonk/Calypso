
#ifndef STRINGREF_H
#define STRINGREF_H

#if __SC__
#pragma once
#endif

#include "root.h"

// Calypso NOTE: we need not assume zero-terminated strings in many cases
// when interfacing with Clang, hence StringRef replaces const char * in many places.

struct StringRef
{
    const char *data; // may not be null terminated
    size_t size;

    StringRef() : data(NULL), size(0) {}
    StringRef(const char *data, size_t size) : data(data), size(size) {}
    StringRef(const char *str); // assumes str is a valid null terminated string

    StringRef& operator=(const char *str); // used in some places by vanilla DMD
    operator const char *() const { return data; }
};

#endif
