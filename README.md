Autor: Hubert Jasudowicz (hj370968)

Kompilator języka Latte zbudowany został w języku Haskell przy użyciu następujących bibliotek:
    - parsec - do parsowania wejściowego programu
    - pretty - do pretty-printowania AST, LLVM IR
    - mtl - do tworzenia monad kompilatora

Kompilator generuje kod w języku LLVM, który następnie przekazywany jest do
llc i llvm-link

Runtime języka znajduje się w katalogu lib, został napisany w języku C.
