# clj-parse Design Goals / Roadmap

clj-parse is an Emacs Lisp library for parsing Clojure code and EDN data. It
supports several input and output formats, all powered by the same shift-reduce
parser function.

This documents describes the design goals for clj-parse, and as such may describe features which are not implemented yet.

## Motivation

People's parsing needs can differ for several reasons

- parsing code (Clojure) vs parsing data (EDN, a subset of Clojure)
- asserting valid input (fail fast) vs dealing gracefully with syntax errors (editor integration)
- analyzing code (whitespace can be ignored) vs doing transformations on code (whitespace aware round-tripping)
- parsing the contents of a buffer, or a string, or a file
- strict parsing (all tagged literals must be understood) vs pass-through (parsing/unparsing unknown tagged literals while ignoring semantics)

This library aims to support all of these use cases, either directly, or by providing the building blocks to do it yourself.

## Prior art

[edn.el](https://github.com/expez/edn.el) is an EDN-to-elisp parser based on the PEG parser generator library.

## Challenges

The data structures available in Emacs are less rich than those used by Clojure.

- Clojure has `nil` and `false`, Emacs only has `nil`.
- Emacs has no notion of sets
- Emacs has no date/timestamp type
- Emacs has no "character" type (characters are represented as numbers)
- Emacs does not support custom records/types (there is a Common Lisp inspired object system, but it implements types on top of regular lists and vectors).
- Emacs does not support adding metadata to values

On the other hand Emacs supports strings/buffers with arbitrary encoding, on the JVM and on JavaScript strings are always UTF-16/UCS-2.

## Architecture

The implementation is implemented in three parts: a lexer, a parser, and multiple reducers.

### Lexer

The *lexer* turns the input text, a buffer, into tokens, data structures representing a single syntactical unit, such as a symbol, a number, or a delimiter like "(", ")", "#{", or "#_".

In clj-parse the lexer is a single function `clj-lex-next` which can be called repeatedly to get a sequence of tokens. `clj-lex-next` returns the token at "point" (i.e. the Emacs cursor position), and moves point to after the token.

A *token* is a association list (list of cons cells), with keys `:token-type`, `:form`, `:position`, and optionally `:error-type`.

Note: we don't add line/column numbers to the token, the consumer can add these if needed based on the position of point before calling `clj-lex-next`.

Example:

This Clojure/EDN input:

``` clojure
(42 "hello" #_ignored #{:a})
```

Results in these tokens

``` emacs-lisp
((:token-type . :lparen)
 (:form . "(")
 (:pos . 1))

((:token-type . :number)
 (:form . "42")
 (:pos . 2))

((:token-type . :whitespace)
 (:form . " ")
 (:pos . 4))

((:token-type . :string)
 (:form . "\"hello\"")
 (:pos . 5))

((:token-type . :whitespace)
 (:form . " ")
 (:pos . 12))

((:token-type . :discard)
 (:form . "#_")
 (:pos . 13))

((:token-type . :symbol)
 (:form . "ignored")
 (:pos . 15))

((:token-type . :whitespace)
 (:form . " ")
 (:pos . 22))

((:token-type . :set)
 (:form . "#{")
 (:pos . 23))

((:token-type . :keyword)
 (:form . ":a")
 (:pos . 25))

((:token-type . :rbrace)
 (:form . "}")
 (:pos . 27))

((:token-type . :rparen)
 (:form . ")")
 (:pos . 28))

((:token-type . :eof)
 (:form . nil)
 (:pos . 29))
```

Note that the lexer makes no attempt to "interpret" the tokens, it merely finds their boundaries. Concatentating the `:form` values yields a string identical to the original input.

Tokens can be recognized by the `:token-type` key, which must always come first in the association list.

## Shift-reduce parser

The parser is again a single function `clj-parse-reduce`. It is a higher order function, with much of the final result determined by the `reduce-leaf` and `reduce-node` functions passed in as arguments.

`clj-parse-reduce` internally operates by using a stack. This stack contains tokens (as returned by `clj-lex-next`), and reduced values.

`reduce-leaf` is a two-argument function. It takes the current value of the stack, and a token, and returns an updated stack, typically by parsing the token to a value and pushing that value onto the stack.

`reduce-node` is a three-argument function. It takes the current value of the stack, a node type, and a list of children, and returns an updated stack.

The parser reads consecutive input tokens. If the token represents a leaf node (like a number, symbol, string), then it calls `reduce-leaf`, giving it a chance to add a value to the stack. If the token is a non-leaf node (a delimiter) it gets put on the stack as-is. This part is known as the "shift" phase.

After "shifting" a value on to the stack, the parser tries to "reduce", by inspecting the top one, two, or three items on the stack.

If the top item is a closing delimiter token, then the parser scans down the stack until it finds a matching opening delimiter. It pops both delimiters and everything in between them off the stack, and passes them to `reduce-node`, which can "reduce" this sequence to a single value (say, a list), and push that item onto the stack.

The type of values pushed onto the stack depends on the reducing functions used. The parser only distinguishes between tokens and non-tokens. It follows that a reducing functions should not push raw tokens back onto the stack.

When parsing finishes the stack will contain all parsed forms in reverse order. It will call `reduce-node` once more with a node type of `:root`, to give it a chance to finalize.

### Example

An example, when parsing the following EDN, with parsing done up to the position of `|`, the stack looks as follows.

``` clojure
;; input
(1 2 (3|))
```

``` emacs-lisp
;; stack
(3
 ((:token-type . :lparen) (:form . "(") (:pos . 6)))
 2
 1
 ((:token-type . :lparen) (:form . "(") (:pos . 1)))
```

Now the parser encounters the first closing parenthesis. It pops `3` and `:lparen` off the stack, and passes `(((:token-type . :lparen) 3 ((:token-type . :rparen)))` to `reduce-node`, which reduces this a single list, and pushes it onto the stack.

``` clojure
;; input
(1 2 (3)|)
```

``` emacs-lisp
;; stack
((3)
 2
 1
 ((:token-type . :lparen) (:form . "(") (:pos . 1)))
```

Now the parser encounters the second closing parenthesis. It pops everything until `:lparen` off the stack, and passes it to `reduce-node`, which turns the result into a list and pushes it onto the stack.

``` clojure
;; input
(1 2 (3))|
```

``` emacs-lisp
;; stack
((1 2 (3)))
```

### Dealing with parse errors

`clj-parse-reduce` needs to be able to parse invalid input. Imagine analyzing a user's buffer while they are editing, to provide contextual help or do linting. Even when delimiters are unbalanced it should still be possible to get a "best effort" parse result. It turns out the shift-reduce approach provides that out of the box. The result of parsing invalid input is a stack which still has unreduced tokens in it.

Unmatched opening delimiter:

``` clojure
;; input
(1 2 { 3)
```

``` emacs-lisp
;; result
((1 2 ((:token-type . :lbrace)) 3))
```

Unmatched closing delimiter:

``` clojure
;; input
(1 2 3))
```

``` emacs-lisp
;; result
((1 2 3) ((:token-type . :lparen)))
```

In many cases it will be desirable to "fail fast", and raise an error as soon as a syntax error is encountered. A `reduce-node` function can do so if it wishes by checking its input sequence for raw tokens, and raising an error if any are present.

## EDN vs Clojure

EDN syntax is a subset of Clojure syntax used for representing data (as opposed to code).

Several constructs that are common in Clojure code are not valid in EDN. This includes

- quoting, syntax quoting, syntax quote splicing, etc.
- read time evaluation with `#=(,,,)`

## Dealing with tagged literals

EDN/Clojure syntax is extensible. Values can be prefixed with user-provided tags, which indicates they need to be transformed after reading by a registered handler function.

In general a consumer of EDN needs to be aware of the possible tags that can occur in the input, as well as their semantics, in order to preserve the data's semantics. When a parser encounters an unknown tag then this is considered an error.

The EDN parser functions will take an optional tag handler function. This function receives two arguments, the tag symbol, and the parsed form that follows the tag. It should either return a correctly coerced value, or raise an error. A default function that knows how to deal with the tags that are part of the EDN spec will be provided.

When parsing code to an AST the situation is different, here tags simply become part of the AST, without caring about their semantics.

## AST

An AST (abstract syntax tree) is a tree structure made up of nodes. A node looks like this

``` emacs-lisp
((:node-type . :root)
 (:position . 0)
 (:children . (((:node-type . :comment)
                (:position . 0)
                (:form . ";; cool stuff\n;; in here"))
               ((:node-type . :list)
                (:position . 26)
                (:children . (((:node-type . :number)
                               (:position . 27)
                               (:form "123")
                               (:value 123))))))))
```

Nodes are an alist with `:node-type` as the first key and a `:position`. Leaf nodes have `:form` and `:value` keys, for the unparsed and parsed form respectively. `:whitespace` and `:comment` nodes only have a `:form`, not a `:value`.

Non-leaf nodes contain a list of `:children`.

## Public API

clj-parse provides three "parse modes"

- `edn` meant for parsing data, it parses EDN to emacs lisp data
- `ast` meant for analyzing source code, parses to a "semantic" AST, does not preserve whitespace or comments
- `source` meant for transforming/round-tripping source codes. Like `ast`, but preserves whitespace and comments

For each of these there can be the following functions

- `clj-parse-{mode}` parse the current buffer starting at `point`, raise an error when syntax/lexing errors are encountered
- `clj-parse-{mode}-full` same as above but ignore syntax errors, returning a partially parsed result
- `clj-print-{mode}` turn the result of the corresponding parse function back into Clojure/EDN, and insert it into the current buffer

Each of these have `-str` variant which instead works on strings. This yields a total potential API of:

```
(defun clj-parse-edn (&OPTIONAL tag-handler))
(defun clj-parse-edn-full (&OPTIONAL tag-handler))
(defun clj-print-edn (edn))
(defun clj-parse-edn-str (string &OPTIONAL tag-handler))
(defun clj-parse-edn-full-str (string &OPTIONAL tag-handler))
(defun clj-print-edn-str (edn))
(defun clj-parse-ast ())
(defun clj-parse-ast-full ())
(defun clj-print-ast (node))
(defun clj-parse-ast-str ())
(defun clj-parse-ast-full-str ())
(defun clj-print-ast-str (node))
(defun clj-parse-source ())
(defun clj-parse-source-full ())
(defun clj-print-source (node))
(defun clj-parse-source-str ())
(defun clj-parse-source-full-str ())
(defun clj-print-source-str (node))
```

This may seem like a massive API surface, but these will all be only one or two lines, combining the generic parsing function with the right reducers.

Beyond that we provide two sets of functions for working with AST nodes.

- zipper-like functions for navigating and transforming ASTs
- functions for reading values out of ASTs, effectively treating them as the data structure they represent.

This second set of functions is important for applications that need to deal with EDN data, but for which the lossy nature of EDN->Elisp transformation is not an option. For instance, unrepl sends EDN messages, but these messages contain code forms that we need to be able to reproduce. In this case converting `false` to `nil` or a set to a list is not acceptable. Instead we can parse the EDN to an AST, and deal with the AST directly.
