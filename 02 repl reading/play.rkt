#lang racket


; We will build a parser to process our REPL interactions, so these steps will
;  share a lot in common with the word list parser.
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

; The most likely interactions will be guessing letters, responding to prompts
;  and typos. The first two should contain purely alphabetic characters, so
;  we can have the lexer sort things based on that.
(define-empty-tokens mttk (EOF INVALID))
(define-tokens tk (VALIDATE))

(define interaction-lexer
  (lexer
   [(eof) (token-EOF)]
   [(:+ alphabetic) (token-VALIDATE lexeme)]
   [(complement (:+ alphabetic)) (token-INVALID)]))

(define (interaction-tokenize inp)
  (define (next-token) (interaction-lexer inp))
  next-token)

; The result of the user interactions will be a bit more complex that simply
;  concatenating the words in a file into a list.
; Rather than defining all of our actions within the parser, we will define
;  procedures for the parser to call in each situation.
(define interaction-parse
  (parser
   [start PossibleInput]
   [end EOF]
   [error void]
   [tokens mttk tk]
   [grammar
    [PossibleInput [(VALIDATE) (Validate (string-upcase $1))]
                   [(INVALID) (Invalid)]]]))

; If we get valid input, we need to check the current game state.
; If we are still playing, then the input needs to be interpreted as a guess, if
;  the game is over, then the input should be a response indicating whether the
;  player wants to play again, etc.
(define (Validate text)
  #`(cond [(equal? game-state 'PLAYING) (begin (guess #,text)
                                               (displayln shown)
                                               (displayln (format "Guesses remaining: ~a" attempts))
                                               (end-turn))]
          [(equal? game-state 'GAMEOVER) (if (equal? #,text "YES")
                                             (begin (set! game-state 'PLAYING)
                                                    (start-game))
                                             (begin (displayln "Goodbye!")
                                                    (set! game-state 'DONE)))]
          [(equal? game-state 'DONE) (displayln "Goodbye!")]
          [(equal? game-state 'INITIALIZED) (displayln "Initialized")]
          [else (displayln "uhoh...")]))

; Now we just need something simple to return when we receive input we don't expect.
(define (Invalid) #'(displayln "What?"))

; This procedure defines how to pass input from the REPL port through our new tokenizer
;  and parser.
(define (our-repl-interactions origin port)
  (define inp (read-line port))
  (if (eof-object? inp)
      eof
      (interaction-parse
       (interaction-tokenize
        (open-input-string inp)))))

; And we set our intended interaction procedure to override the built in REPL behavior.
(current-read-interaction our-repl-interactions)

; The procedures which drive the guessing game itself are defined here. Although they are
;  required for our game to work, they are not relevant to to the language component.
;;;;;

; This pulls in our word bank, which the reader we created in the previous section parses
;  and uses to create a random word procedure.
(require "word-list.rkt")

(define theword "")
(define hidden "")
(define shown "")
(define attempts 0)
(define game-state 'INITIALIZED)

(define (start-game)
  (set! theword (string-upcase (choose-word)))
  (set! hidden (format "~a" theword))
  (set! shown (make-dashes hidden))
  (set! attempts
        (quotient (* (string-length hidden) 4)
                  5))
  (set! game-state 'PLAYING)
  (displayln shown)
  (displayln (format "Guesses remaining: ~a" attempts)))

(define (make-dashes strng)
  (make-string (string-length strng) #\-))

(define (flop strng1 strng2 position)
  (let ([new1 (string-ref strng2 position)]
        [new2 (string-ref strng1 position)])
    (string-set! strng1 position new1)
    (string-set! strng2 position new2)))

(define (string-index-of strng char)
  (let ([charlist (string->list strng)])
    (index-of charlist char)))

(define (guess strng)
  (if (= 0 (string-length strng))
      (void)
      (let ([char (string-ref strng 0)]
            [rest (substring strng 1)])
        (if (member char (string->list hidden))
            (begin
              (do-flop char)
              (guess rest))
            (begin
              (set! attempts (- attempts 1))
              (guess rest))))))

(define (do-flop char)
  (let ([temp (member char (string->list hidden))])
    (if temp
        (let ([ind (string-index-of hidden char)])
          (flop hidden shown ind)
          (or (do-flop char) #t))
        #f)))

(define (did-we-win?)
  (and (< 0 attempts)
       (equal? hidden (make-dashes hidden))))

(define (did-we-lose?)
  (>= 0 attempts))

(define (end-turn)
  (cond [(did-we-win?) (begin (set! game-state 'GAMEOVER)
                              (displayln "Congratulations, you've won!")
                              (displayln "Would you like to play again?"))]
        [(did-we-lose?) (begin (set! game-state 'GAMEOVER)
                               (displayln "Too bad. You've lost.")
                               (displayln (format "The word was '~a'." theword))
                               (displayln "Would you like to play again?"))]
        [else (void)]))

;;;;;
; Finally, we start the game environment.

(displayln "Are you ready to play a guessing game?")
(start-game)