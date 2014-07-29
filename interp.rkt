#lang racket
(require racket/match)
(require "parser.rkt")

(define-syntax (get-ast stx)
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
         #'(let* ([input (open-input-string str)]
                  [ast (parser-name (lambda () (lexer-name input)))])
             (begin
               (close-input-port input)
               ast)))]
      [(_ "FILE" name filename)
       (with-syntax ([lexer-name (datum->syntax stx (build-lexer-name #'name))]
                     [parser-name (datum->syntax stx (build-parser-name #'name))])
         #'(let* ([input (open-input-file filename)]
                  [ast (parser-name (lambda () (lexer-name input)))])
             (begin
               (close-input-port input)
               ast)))])))

(get-ast "STRING" snail "print a;")
(get-ast "STRING" snail "if 1 < 2 then if a < b then print a; endif else print b; endif")
(get-ast "FILE" snail "sample-code.txt")

(define env0 '())
(define (ext-env k v env)
  (cons `(,k . ,v) env))
(define (bulk-ext-env ks vs env)
  (for/fold ([env '()])
    ([k '(a b c)]
     [v '(1 2 3)])
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
     (let-syntax ([delegate 
              (lambda (stx)
                (syntax-case stx ()
                  [(_ eval op env)
                   (with-syntax ([x (datum->syntax stx 'x)]
                                 [y (datum->syntax stx 'y)])
                     (syntax (`(op ,x ,y) (op (eval x env) (eval y env)))))]))])
     (match exp
       [`(NEG ,x) (- (eval. x env))]
       [delegate eval. + env]
       [delegate eval. - env]
       [delegate eval. * env]
       [delegate eval. / env]
       [delegate eval. < env]
       [delegate eval. <= env]
       [delegate eval. > env]
       [delegate eval. >= env]
       [`(== ,x ,y) (= (eval. x env) (eval. y env))]
       [`(!= ,x ,y) (not (= (eval. x env) (eval. y env)))]))]))

(define (interp stat)
  (interp1 stat env0))

(interp1 (get-ast "FILE" snail "sample-code.txt") env0)
