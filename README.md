Editor integration for Piglet. Currently contains:

- piglet-mode, a major mode based on tree-sitter
- pdp, a Piglet Dev Protocol server implementation, for interactive eval

The package is called `piglet-emacs` for lack of a better name. 

- `piglet-mode` is too reductive, it's more than just the mode
- `piglet` is a misnomer, you're not installing the language
- We already called the repo that

You need Emacs 29 _with tree-sitter_ support. It may seem like some tree-sitter functions are available even if support isn't compiled in. On debian/ubuntu systems you need the package `libtree-sitter0`, and then compile emacs with `./configure --with-tree-sitter`.
