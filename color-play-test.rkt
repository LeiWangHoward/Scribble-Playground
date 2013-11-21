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


(define t4 (new racket:text%))
(send t4 insert "#lang scribble/base\na\n @b{} wqe\n\n  c cc\nd sdsd  wewe\n ereer \n\n wr")

(define t5 (new racket:text%))
(send t5 insert "#lang scribble/doc\n@(require \"common.rkt\"\nscribble/decode scribble/eval scribble/struct scribble/racket\n(for-label racket/gui/base framework)\nsetup/getinfo racket/pretty string-constants)")
(require "scribble-indentation.rkt")
(require rackunit)

(txt-position-classify t5 0 33)

#|
(txt-position-classify t 20 50)
(let-values ([(c d) (send t get-token-range 40)]);start to end+1
  (displayln c)
  (displayln d))

(txt-position-classify t2 20 50)
(let-values ([(a b) (send t get-token-range 26)]);start to end+1
  (displayln a)
  (displayln b))

(define (insert-break t start width-end end)
  (let ([right-break (for/first ([break-rs (in-range width-end end)] ;;todo: make it search back also
                                 #:when (equal? #\space (send t get-character break-rs)))
                       break-rs)]
        [left-break (for/first ([break-ls (in-range width-end start -1)]
                                #:when (equal? #\space (send t get-character break-ls)))
                      break-ls)])
      (cond [(and right-break left-break)
             (if (> (- width-end left-break) (- right-break width-end))
                 right-break
                 left-break)]
            [left-break
             (if (< (- width-end left-break) (- end width-end))
                 left-break
                 #f)]
            [right-break
             right-break]
            [t #f])))
           
(require rackunit)
|#