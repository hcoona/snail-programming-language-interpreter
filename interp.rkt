#lang racket
(require racket/match)
(require "parser.rkt")

(define env0 '())
(define (ext-env k v env)
  (cons `(,k . ,v) env))
(define (bulk-ext-env ks vs env)
  (for/fold ([env '()])
    ([k ks]
     [v vs])
    (ext-env k v env)))
(define (lookup-env k env)
  (cdr (assoc k env)))

(define (interp1 stat env)
  (match stat
    [`(BEGIN ,ss ...) (for/fold ([env env])
                        ([s ss])
                        (interp1 s env))]
    [`(ASSIGN ,n ,e) (ext-env n (eval. e env) env)]
    [`(PRINT ,e) (begin
                   (display (eval. e env))
                   env)]
    [`(PRINT-STRING ,s) (begin (display s) env)]
    ['(NEWLINE) (begin (newline) env)]
    [`(IF ,e ,then)
     (if (eval. e env)
         (interp1 then env)
         env)]
    [`(IF ,e ,then ,else)
     (if (eval. e env)
         (interp1 then env)
         (interp1 else env))]))

(define (eval. exp env)
  (cond
    [(number? exp) exp]
    [(symbol? exp) (lookup-env exp env)]
    [else
     (match exp
       [`(NEG ,x)      (-   (eval. x env))]
       [(list '+ x y)  (+   (eval. x env) (eval. y env))]
       [(list '- x y)  (-   (eval. x env) (eval. y env))]
       [(list '* x y)  (*   (eval. x env) (eval. y env))]
       [(list '/ x y)  (/   (eval. x env) (eval. y env))]
       [(list '< x y)  (<   (eval. x env) (eval. y env))]
       [(list '<= x y) (<=  (eval. x env) (eval. y env))]
       [(list '> x y)  (>   (eval. x env) (eval. y env))]
       [(list '>= x y) (>=  (eval. x env) (eval. y env))]
       [`(== ,x ,y)    (=   (eval. x env) (eval. y env))]
       [`(!= ,x ,y)    (not (= (eval. x env) (eval. y env)))])]))

(define (interp stat)
  (interp1 stat env0))

(define (interp-port port [close #f])
  (begin
    (interp (snail-parser (lambda () (snail-lexer port))))
    (if close
        (close-input-port port)
        (void))))

(define (interp-string str)
  (let ([input (open-input-string str)])
    (interp-port input)))

(define (interp-file filename)
  (let ([input (open-input-file filename #:mode 'text)])
    (interp-port input)))

(provide (rename-out [interp snail-interp]
                     [interp-port snail-interp-port]
                     [interp-string snail-interp-string]
                     [interp-file snail-interp-file]))

;(interp-file "sample-code.txt")