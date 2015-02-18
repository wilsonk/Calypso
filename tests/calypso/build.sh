rm -f calypso_cache* *.o
clang++ -std=c++11 -c showcase.cpp -o showcase.cpp.o
ar rcs libshowcase.a showcase.cpp.o
