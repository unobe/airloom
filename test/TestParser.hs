module TestParser (suite) where

import AirLoom.Parser
  ( DocLine (DocTagLine, DocTextLine),
    DocTag (TranscludeTag),
    SourceLine (SourceTagLine, SourceTextLine),
    SourceTag (FragmentEndTag, FragmentStartTag),
    parseDocFile,
    parseSourceFile,
    parseSourceLine,
  )
import Test.HUnit

parseSourceLineTest :: Test
parseSourceLineTest =
  TestCase
    ( do
        -- Source line cases.
        assertEqual "" (SourceTextLine "") (parseSourceLine "")
        assertEqual "" (SourceTextLine "abc") (parseSourceLine "abc")
        assertEqual "" (SourceTextLine "Hello, world!") (parseSourceLine "Hello, world!")
        assertEqual "" (SourceTextLine " loom:start(broken") (parseSourceLine " loom:start(broken")
        -- loom:start cases
        assertEqual "" (SourceTagLine (FragmentStartTag "b")) (parseSourceLine "   loom:start(b)   ")
        assertEqual "" (SourceTagLine (FragmentStartTag "a")) (parseSourceLine "loom:start(a)")
        assertEqual "" (SourceTagLine (FragmentStartTag "/usr/test/a.js")) (parseSourceLine "loom:start(/usr/test/a.js)")
        -- loom:end cases
        assertEqual "" (SourceTagLine (FragmentEndTag "c")) (parseSourceLine "loom:end(c)")
        assertEqual "" (SourceTagLine (FragmentEndTag "d")) (parseSourceLine "    loom:end(d)   ")
        assertEqual "" (SourceTagLine (FragmentEndTag "/usr/test/e.pm")) (parseSourceLine "    loom:end(/usr/test/e.pm)   ")
    )

parseSourceFileTest :: Test
parseSourceFileTest =
  TestCase
    ( do
        assertEqual "empty file" [] (parseSourceFile "")
        assertEqual "one text line" [SourceTextLine "abc"] (parseSourceFile "abc")
        assertEqual "two text lines" [SourceTextLine "abc", SourceTextLine "def"] (parseSourceFile "abc\ndef")
        assertEqual "text line and start tag" [SourceTextLine "abc", SourceTagLine (FragmentStartTag "foo")] (parseSourceFile "abc\nloom:start(foo)")
        assertEqual "start tag and end tag" [SourceTagLine (FragmentStartTag "bar"), SourceTagLine (FragmentEndTag "bar")] (parseSourceFile "loom:start(bar)\nloom:end(bar)")
        assertEqual "text lines, start and end tags" [SourceTextLine "abc", SourceTagLine (FragmentStartTag "foo"), SourceTextLine "def", SourceTagLine (FragmentEndTag "foo"), SourceTextLine "ghi"] (parseSourceFile "abc\nloom:start(foo)\ndef\nloom:end(foo)\nghi")
        assertEqual "text line and start tag /" [SourceTextLine "abc", SourceTagLine (FragmentStartTag "foo/a.pm")] (parseSourceFile "abc\nloom:start(foo/a.pm)")
        assertEqual "start tag and end tag /" [SourceTagLine (FragmentStartTag "bar/long.js"), SourceTagLine (FragmentEndTag "bar/long.js")] (parseSourceFile "loom:start(bar/long.js)\nloom:end(bar/long.js)")
        assertEqual "text lines, start and end tags /" [SourceTextLine "abc", SourceTagLine (FragmentStartTag "/foo/bar.js"), SourceTextLine "def", SourceTagLine (FragmentEndTag "/foo/bar.js"), SourceTextLine "ghi"] (parseSourceFile "abc\nloom:start(/foo/bar.js)\ndef\nloom:end(/foo/bar.js)\nghi")
    )

parseDocFileTest :: Test
parseDocFileTest =
  TestCase
    ( do
        -- Doc line cases.
        assertEqual "Empty file." [] (parseDocFile "")
        assertEqual "One text line." [DocTextLine "abc"] (parseDocFile "abc")
        assertEqual "Hello, world!" [DocTextLine "Hello, world!"] (parseDocFile "Hello, world!")
        assertEqual "Broken include." [DocTextLine " loom:include(broken"] (parseDocFile " loom:include(broken")
        -- loom:include cases
        assertEqual "loom:include(a) with spaces" [DocTagLine (TranscludeTag "b")] (parseDocFile "   loom:include(b)   ")
        assertEqual "loom:include(a) without spaces" [DocTagLine (TranscludeTag "a")] (parseDocFile "loom:include(a)")
        assertEqual "Multiple lines" [DocTextLine "abc", DocTagLine (TranscludeTag "a"), DocTextLine "def"] (parseDocFile "abc\nloom:include(a)\ndef")
        assertEqual "loom:include(/a/b.js) with spaces" [DocTagLine (TranscludeTag "/a/b.js")] (parseDocFile "   loom:include(/a/b.js)   ")
        assertEqual "loom:include(/a/b.js) without spaces" [DocTagLine (TranscludeTag "/a/b.js")] (parseDocFile "loom:include(/a/b.js)")
        assertEqual "Multiple lines with /" [DocTextLine "abc", DocTagLine (TranscludeTag "a/bar.js"), DocTextLine "def"] (parseDocFile "abc\nloom:include(a/bar.js)\ndef")
    )

parseHelloWorldSourceTest :: Test
parseHelloWorldSourceTest =
  TestCase
    ( do
        let fileContents =
              unlines
                [ "// loom:start(hello.c/file)",
                  "#include <stdio.h>",
                  "#include <stdlib.h>",
                  "",
                  "int main(void)",
                  "{",
                  "    // loom:start(hello.c/printf)",
                  "    printf(\"Hello, World!\\n\");",
                  "    // loom:end(hello.c/printf)",
                  "    return EXIT_SUCCESS;",
                  "}",
                  "// loom:end(hello.c/file)"
                ]
        let expected =
              [ SourceTagLine (FragmentStartTag "hello.c/file"),
                SourceTextLine "#include <stdio.h>",
                SourceTextLine "#include <stdlib.h>",
                SourceTextLine "",
                SourceTextLine "int main(void)",
                SourceTextLine "{",
                SourceTagLine (FragmentStartTag "hello.c/printf"),
                SourceTextLine "    printf(\"Hello, World!\\n\");",
                SourceTagLine (FragmentEndTag "hello.c/printf"),
                SourceTextLine "    return EXIT_SUCCESS;",
                SourceTextLine "}",
                SourceTagLine (FragmentEndTag "hello.c/file")
              ]
        assertEqual "Parsing source file" expected (parseSourceFile fileContents)
    )

parseHelloWorldDocTest :: Test
parseHelloWorldDocTest =
  TestCase
    ( do
        let fileContents =
              unlines
                [ "# Hello, World in C",
                  "",
                  "Traditionally, the way to kick the tires on a programming language is to",
                  "write a program that simply prints \"Hello, world!\" and exits. In C, the",
                  "function for printing text is called `printf`, and we use it like this:",
                  "",
                  "```c",
                  "loom:include(hello.c/printf)",
                  "```",
                  "",
                  "The whole program looks like this:",
                  "",
                  "```c",
                  "loom:include(hello.c/file)",
                  "```"
                ]
        let expected =
              [ DocTextLine "# Hello, World in C",
                DocTextLine "",
                DocTextLine "Traditionally, the way to kick the tires on a programming language is to",
                DocTextLine "write a program that simply prints \"Hello, world!\" and exits. In C, the",
                DocTextLine "function for printing text is called `printf`, and we use it like this:",
                DocTextLine "",
                DocTextLine "```c",
                DocTagLine (TranscludeTag "hello.c/printf"),
                DocTextLine "```",
                DocTextLine "",
                DocTextLine "The whole program looks like this:",
                DocTextLine "",
                DocTextLine "```c",
                DocTagLine (TranscludeTag "hello.c/file"),
                DocTextLine "```"
              ]
        assertEqual "Parsing documentation file" expected (parseDocFile fileContents)
    )

suite :: Test
suite =
  TestList
    [ TestLabel "parseSourceLine" parseSourceLineTest,
      TestLabel "parseSourceFile" parseSourceFileTest,
      TestLabel "parseDocFileTest" parseDocFileTest,
      TestLabel "parseHelloWorldSourceTest" parseHelloWorldSourceTest,
      TestLabel "parseHelloWorldDocTest" parseHelloWorldDocTest
    ]
