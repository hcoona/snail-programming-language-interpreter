#lang racket
(require "interp.rkt")

(let ([args (current-command-line-arguments)])
  (if (eq? 0 (vector-length args))
      (snail-interp-port (current-input-port))
      (let ([filename (vector-ref args 0)])
        (cond
          [(equal? "-" filename)
           (snail-interp-port (current-input-port))]
          [else
           (snail-interp-file filename)]))))