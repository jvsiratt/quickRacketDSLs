#lang racket

; The first goal will be to make a game of Hangman.
; To do this we will need to select a random word from a list,
; such as with the function here.
(define (pick-from-list lst)
  (car (shuffle lst)))

; We could do this by hard coding a list of words,
;  but it would be nicer to have a word list in a separate file.
; In keeping with the theme of creating DSLs,
;  we will do this by creating a very simple reader.
; We will start with the parser tools that are provided by the core language.
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

; Define a symbolic token for the end of the file,
;  and a token for each word in the list.
(define-empty-tokens the-empty-tokens
  (EOF))
(define-tokens the-tokens
  (WORD))

; The input will be received as a string from the input port.
; The lexer will convert the string to tokens.
; This lexer will return the EOF token when it encounters the end of the file,
;  a WORD token (containing the appropriate string) when it encounters a
;  sequence of one or more alphabetic characters, and when it encounters
;  whitespace it will do nothing and call the lexer again -- effectively
;  skipping the whitespace.
(define word-list-lex
  (lexer
   [(eof) (token-EOF)]
   [(:+ alphabetic) (token-WORD lexeme)]
   [whitespace (word-list-lex input-port)]))

; Using the lexer we can define a tokenizer which takes an input stream
;  and returns a nullary procedure that, on each call, returns the next token.
(define (word-list-tokenize inp)
  (define (next-token) (word-list-lex inp))
  next-token)

; The parser will apply the grammar we defined to the sequence of tokens
;  and construct a parse tree based on that.
; These particular parsing tools use yacc-style parsing.
; We define the entry point of the grammar, or the top-level -- a list of words.
; This list of words will either be empty, or it will be some word token
;  followed by a list of words.
; If it is empty, then the parser is supposed to return the empty list.
; If it is a word token followed by a list of words, the parser is supposed to
;  create a list where the word lexeme is the head and the tail is whatever
;  the result of parsing the remaining list-of-words is.
(define word-list-parse
  (parser
   [start list-of-words]
   [end EOF]
   [error void]
   [tokens the-empty-tokens the-tokens]
   [grammar
    [list-of-words [(WORD list-of-words) (cons $1 $2)]
                   [() empty]]]))

; Our next step is to create a read-syntax procedure that will be provided whenever
;  another file tells Racket to use this file as its reader.
; The job of the reader is to translate the content of the file into Racket code.
; We define this procedure to first apply the tokenizer and parser to the file, which
;  produces a list of strings.
; The procedure then constructs a syntax object defining a module which stores the
;  list constructed by the parser as word-list and defines a procedure that will choose
;  a random word from the list.
; The final action of the module is to provide the choose-word procedure for external use.
(require syntax/strip-context)
(define (read-syntax path port)
  (define inp (word-list-parse (word-list-tokenize port)))
  (strip-context
   #`(module word-file-module racket
       (define word-list '#,inp)
       (define (choose-word)
         (if (= 0 (length word-list))
             (error "Word list is empty!")
             (car (shuffle word-list))))
       (provide choose-word))))
(provide read-syntax)

; Now a separate file "word-list.rkt" can be created in this directory that starts with:
;  #lang reader "word-list-reader.rkt"
; We can freely add or remove words from this file and our Hangman program will be
;  able to use (require "word-list.rkt") to access the choose-word procedure as needed.