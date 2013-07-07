### cl-parsec
cl-parsec is a port of parsec from Haskell to Common Lisp.  It is written by [Alex Suraci](https://github.com/vito), the original can be found [here](http://darcsden.com/alex/cl-parsec).  I have merely packaged it, exported the relevant symbols, written some more tests and fixed a few minor bugs.  All the good bits are written by Suraci.

This readme is intentionally short because cl-parsec is so similar to parsec proper that any tutorial for that library will do.

#### Dependencies
* cl-unicode

#### Differences from Parsec
Some of the functions go by different names, sometimes this is because functions exported by parsec proper have names that conflict with symbols in CL, otherwise it is because the new names are more descriptive.  The following aliases are used in cl-parsec.
* space     -> spacet
* count     -> replicate
* sepBy     ->  separate-by
* sepBy1    -> seperate-by-1
* sepEndBy  -> separate-end-by
* sepEndBy1 -> separate-end-by-1
