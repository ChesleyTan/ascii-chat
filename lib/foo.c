#include <stdio.h>

extern "C" {
    void foo() {
        puts("Hello, I'm a shared library");
    }
}
