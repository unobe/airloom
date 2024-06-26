# Hello, Universe in C

Traditionally, the way to kick the tires on a programming language is to
write a program that simply prints "Hello, universe!" and exits. In C, the
function for printing text is called `printf`, and we use it like this:

```c
printf("Hello, Universe!\n");
```

The whole program looks like this:

```c
#include <stdio.h>
#include <stdlib.h>

int main(void)
{
    printf("Hello, Universe!\n");
    return EXIT_SUCCESS;
}
```
