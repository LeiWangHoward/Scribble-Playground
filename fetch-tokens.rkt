#lang racket
#|
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
|#

(require framework)

(define txt (new racket:text%))
(send txt insert "#lang scribble/base\n@f{x}")
(for/list ([x (in-range (send txt last-position))])
  (send txt classify-position x))

(send txt get-token-range 0)

(send txt paragraph-start-position 1)

(define txt2 (new racket:text%))
(send txt2 insert "#lang scribble/base\n@f{\n x\n}")

(define txt3 (new racket:text%))
(send txt3 insert "#lang scribble/base\n@f[\n x\n]")

;; position-line, 

;; backward-match, backward-containing-sexp, forward-match

;; determine-spaces : text position[natural] -> spaces @ front or #f

; position-paragraph (method of text%)  provided from racket/gui/base, racket/gui
; paragraph-end-position (method of text%)  provided from racket/gui/base, racket/gui
; paragraph-start-position (method of text%)  provided from racket/gui/base, racket/gui