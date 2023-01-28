<img src="airloom.jpg"
     alt="Logo: a detail of John William Waterhouse's paintinf of the Lady of Shalott, showing a loom."
     width=250
     height=250
     align="right" />

# Air Loom

**Air Loom** is a reverse literate programming tool: it takes code and documentation and weaves them together. It is both language-agnostic and markup language-agnostic.

## Example

Given a `hello.c` file:

```c++
// loom:start(file)
#include <stdio.h>
#include <stdlib.h>

int main(void)
{
    // loom:start(printf)
    printf("Hello, World!\n");
    // loom:end(printf)
    return EXIT_SUCCESS;
}
// loom:end(file)
```

And `hello.md`:

~~~markdown
# Hello, World in C

Traditionally, the way to kick the tires on a programming language is to
write a program that simply prints "Hello, world!" and exits. In C, the
function for printing text is called `printf`, and we use it like this:

```c
loom:include(printf)
```

The whole program looks like this:

```c
loom:include(main)
```
~~~

Then the following:

```bash
$ airloom lift hello.c -o frags.json
$ airloom weave hello.md -f frags.json -o docs.md
```

Will produce a file `docs.md` with the following contents:

~~~markdown
# Hello, World in C

Traditionally, the way to kick the tires on a programming language is to
write a program that simply prints "Hello, world!" and exits. In C, the
function for printing text is called `printf`, and we use it like this:

```c
printf("Hello, World!\n");
```

The whole program looks like this:

```c
#include <stdio.h>
#include <stdlib.h>

int main(void)
{
    printf("Hello, World!\n");
    return EXIT_SUCCESS;
}
```
~~~
    
## Conceptual Overview

Traditionally, there are two approaches to literate programming:

1. You write code inside your documentation, and a program extracts and concatenates the listings into source code. This worked in the 70's, but nowadays, you lose all the benefits of IDEs.
2. A newer approach is to write the documentation within the code as block comments, then a program strips the comment markers and turns everything that's not a comment into a code block. The problem with this approach is many languages require declarations to appear in a specific order, which is rarely the ideal reading order.

With Air Loom, you write your code in source files, retaining all the benefits of using IDEs, and use comment directives to define named chunks of code called _fragments_. Then, in your documentation, you use directives to include fragments. Air Loom then lifts the fragments out of the code and weaves them into the documentation. Code and documentation are entirely separated.

## Usage

The `lift` command extracts fragments from a set of files into a fragments file:

```bash
airloom lift lib.c hello.c test.c -o fragments.json
```

The `weave` command takes a number of documentation files and a fragments file, it weaves the fragments into the documentation and concatenates them together:

```bash
airloom weave intro.md examples.md api.md -f fragments.json -o docs.md
```

## Downloading

## Building

You need [stack][stack] installed. Then:

[stack]: https://docs.haskellstack.org/en/stable/

```bash
$ git clone https://github.com/eudoxia0/airloom.git
$ cd airloom
$ stack build
```

## License

Copyright (c) 2023 [Fernando Borretti](https://borretti.me/).

Licensed under the Apache 2.0 license.
