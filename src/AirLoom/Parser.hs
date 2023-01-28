module AirLoom.Parser where 

-- The types of tags we can encounter in source files.
data SourceTag = FragmentStartTag String
               | FragmentEndTag String

-- The types of tags we can encounter in documentation files.
data DocTag = TranscludeTag String

-- The type of lines in source files.
data SourceLine = SourceTextLine String
                | SourceTagLine SourceTag

-- The type of lines in documentation files.
data DocLine = DocTextLine String
             | DocTagLine DocTag

loomStartRegex :: String
loomStartRegex = "[^\\\\]loom:start\\(([a-zA-Z0-9,':]+)\\)"

loomEndRegex :: String
loomEndRegex = "[^\\\\]loom:start\\(([a-zA-Z0-9,':]+)\\)"
