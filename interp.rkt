#lang racket
(require "parser.rkt")

(define-syntax (display-parse stx)
  (let* ([postfix-name
         (lambda (name postfix)
           (string->symbol
            (string-append (format "~a-~a"
                                   (symbol->string name)
                                   (symbol->string postfix)))))]
         [build-lexer-name
          (lambda (name-datum)
            (postfix-name (syntax->datum name-datum) 'lexer))]
         [build-parser-name
          (lambda (name-datum)
            (postfix-name (syntax->datum name-datum) 'parser))])
    (syntax-case stx ()
      [(_ "STRING" name str)
       (with-syntax ([lexer-name (datum->syntax stx (build-lexer-name #'name))]
                     [parser-name (datum->syntax stx (build-parser-name #'name))])
         #'(let ([input (open-input-string str)])
             (displayln (parser-name (lambda () (lexer-name input))))))]
      [(_ "FILE" name filename)
       (with-syntax ([lexer-name (datum->syntax stx (build-lexer-name #'name))]
                     [parser-name (datum->syntax stx (build-parser-name #'name))])
         #'(let ([input (open-input-file filename)])
             (begin
               (displayln (parser-name (lambda () (lexer-name input))))
               (close-input-port input))))])))

(display-parse "STRING" snail "print a;")

(display-parse "FILE" snail "sample-code.txt")