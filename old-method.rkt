#lang racket
(require syntax-color/scribble-lexer)
(define (get-them port)
  (let loop ([mode #f])
    (define-values (str/eof token-symbol paren start end backup-dist new-mode)
      (scribble-inside-lexer port 0 mode))
    (cond
      [(eof-object? str/eof) '()]
      [else (cons (list str/eof token-symbol) (loop new-mode))])))

(define sp (open-input-file "mouse.scrbl"))
(read-line sp)
(read-line sp)
(read-line sp)

(get-them (open-input-string "@f{x}"))