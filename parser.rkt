#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)
(require racket/match)

(define-tokens primitives (INT ID STRING))
(define-empty-tokens
  operators
  (= + * - / < > <= >= == != NEG))
(define-empty-tokens
  delimiters
  (SEMICOLON LPAR RPAR EOF))
(define-empty-tokens
  keywords
  (PRINT NEWLINE IF THEN ELSE ENDIF))

(define snail-lexer
  (lexer-src-pos
   [(:or "-" "+" "*" "/" "<" ">" "<=" ">=" "==" "!=" "=") (string->symbol lexeme)]
   ["(" (token-LPAR)]
   [")" (token-RPAR)]
   [";" (token-SEMICOLON)]
   [(:or "print" "newline") (string->symbol (string-upcase lexeme))]
   [(:or "if" "then" "else" "endif") (string->symbol (string-upcase lexeme))]
   [(:+ numeric) (token-INT (string->number lexeme))]
   [(:+ alphabetic) (token-ID lexeme)]
   [#\" (token-STRING (list->string (string-lexer input-port)))]
   [(:: #\/ #\/ (:* (:- any-char #\newline)) #\newline) (return-without-pos (snail-lexer input-port))]
   [whitespace (return-without-pos (snail-lexer input-port))]
   [(eof) (token-EOF)]))

(define string-lexer
  (lexer
   [(:~ #\" #\\) (cons (car (string->list lexeme))
                       (string-lexer input-port))]
   [(:: #\\ #\\) (cons #\\ (string-lexer input-port))]
   [(:: #\\ #\") (cons #\" (string-lexer input-port))]
   [#\" null]))

(define (build-statment-list source)
  (cons 'BEGIN (reverse source)))

(define snail-parser
  (parser
   [src-pos]
   [suppress]
   [start prog]
   [end EOF]
   [error (lambda (ok name val start end)
            (printf
             "parsing ~a with value ~a error. POS: [~a, ~a]\n"
             name
             val
             start
             end))]
   [tokens primitives operators delimiters keywords]
   [precs (right =)
          (left < > <= >= == !=)
          (left - +)
          (left * /)
          (left NEG)]
   [grammar
    (prog         [(stmt_list) (build-statment-list $1)])
    (stmt_list    [(stmt_list stmt) (cons $2 $1)]
                  [(stmt) (list $1)])
    (stmt         [(simple_stmt SEMICOLON) $1]
                  [(compond_stmt) $1]
                  [() '()])
    (simple_stmt  [(assign_stmt) $1]
                  [(print_stmt) $1])
    (assign_stmt  [(ID = expr) `(ASSIGN ,$1 ,$3)])
    (print_stmt   [(PRINT expr) `(PRINT ,$2)]
                  [(PRINT NEWLINE) '(NEWLINE)]
                  [(PRINT STRING) `(PRINT ,$2)])
    (compond_stmt [(IF expr THEN stmt_list ENDIF)
                   `(IF ,$2 ,(build-statment-list $4))]
                  [(IF expr THEN stmt_list ELSE stmt_list ENDIF)
                   `(IF ,$2 ,(build-statment-list $4) ,(build-statment-list $6))])
    (expr         [(expr + expr)       `(+ ,$1 ,$3)]
                  [(expr - expr)       `(- ,$1 ,$3)]
                  [(expr * expr)       `(* ,$1 ,$3)]
                  [(expr / expr)       `(/ ,$1 ,$3)]
                  [(expr < expr)       `(< ,$1 ,$3)]
                  [(expr <= expr)      `(<= ,$1 ,$3)]
                  [(expr > expr)       `(> ,$1 ,$3)]
                  [(expr >= expr)      `(>= ,$1 ,$3)]
                  [(expr == expr)      `(== ,$1 ,$3)]
                  [(expr != expr)      `(!= ,$1 ,$3)]
                  [(- expr) (prec NEG) `(NEG ,$2)]
                  [(LPAR expr RPAR)    $2]
                  [(INT)               $1]
                  [(ID)                $1])]))

(define (run-lexer lexer port)
  (let* ([t (position-token-token (lexer port))]
         [tn (token-name t)])
    (cond
      [(eq? tn 'EOF) 'EOF]
      [else
       (begin
         (display t)
         (newline)
         (run-lexer lexer port))])))

(provide snail-lexer snail-parser)