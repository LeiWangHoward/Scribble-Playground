#lang racket
(require framework)

(define (txt-position-classify txt start end)
  (for/list ([x (in-range start end 1)])
    (send txt classify-position x)))

(define t (new racket:text%))
(send t insert "#lang scribble/base\ndddd dddd\n @title[sa \"dasdasda\" bb bb\n@cccc{dd dd\n\nee ee}\nff ff]")

(define t2 (new racket:text%))
(send t2 insert "#lang scribble/base\n@(let ()(define pth (collection-file-path \"info.rkt\" \"drracket\"))\n");;define considered as key word

(define t3 (new racket:text%))
(send t3 insert "#lang scribble/base\nthe @as-index{@onscreen{Check Syntax} button}")

(txt-position-classify t 20 50)
(let-values ([(c d) (send t get-token-range 40)]);start to end+1
  (displayln c)
  (displayln d))

(txt-position-classify t2 20 50)
(let-values ([(a b) (send t get-token-range 26)]);start to end+1
  (displayln a)
  (displayln b))