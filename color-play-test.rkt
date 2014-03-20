#lang racket
(require framework)
#|
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
|#
(require "scribble-indentation.rkt")
(require rackunit)

;(txt-position-classify t5 0 33)
#|
(define t6 (new racket:text%))
(send t6 insert 
      "#lang scribble/base\n@itemlist[ @item{item1}\n@item{item2}\n];")
;;#lang scribble/base\n@f{\n @a{asdadsasd}\n@b\n}")

(txt-position-classify t6 0 150)



(define txt_11 (new racket:text%))
(send txt_11 insert "#lang scribble/base\n@a[\n    ]\n")

(let* ([current-para (send txt_11 position-paragraph 24)]
       [para-start (send txt_11 paragraph-start-position current-para)]
       [para-start-skip-space (send txt_11 skip-whitespace para-start 'forward #t)]
       [sexp (send txt_11 backward-containing-sexp para-start-skip-space 0)])
  (displayln current-para)
  (displayln para-start)
  (displayln para-start-skip-space)
  (displayln sexp))

(displayln (send txt_11 classify-position 27))
|#

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


(let ([t (new racket:text%)])
  (send t insert "{abcde   \nfgh\n}")
  (send t delete 5 9)
  (send t get-text))


#|(check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang scribble/base\ntestcase @a{b\n\n\n\n\n      c}\n")
                  (determine-spaces t 44))
              12) |#

;(displayln #\tab)

(define tn (new racket:text%))
(send tn insert "#lang scribble/base\ntest\ncase @a{b\n\n\n\n\n      c}\n")

#|(let* ([current-line (send tn position-paragraph 25)]
       [para-start-line (for/first ([line (in-range current-line 0 -1)]
                                    #:when (or (empty-line? tn line)
                                               (= line 0)))
                          line)])
  (displayln para-start-line))

(check-equal? (let ([t (new racket:text%)])
                (send t insert "#lang scribble/base\n\ntest1\n     test2\n\t\ttest3\n")
                (determine-spaces t 28))
              0)


 (let ([t (new racket:text%)])
                  (send t insert "#lang scribble/base\n\ntestcase @a{b\n\n\n\n\n      c}")
                  (send t backward-containing-sexp 38 0))
   

 (let ([t (new racket:text%)])
                  (send t insert "#lang scribble/base\nt@a{b\n\n\n\n\n      c  \n\n\n\n     }\n")
                  (send t forward-match 29 -1))
|#

(let ([t (new racket:text%)])
  (send t insert "#lang scribble/base\n@a{b\n\n  c}")
  (send t backward-containing-sexp 26 0))

(let ([t (new racket:text%)])
  (send t insert "#lang scribble/base\n@a{\n\n  c}")
  (send t backward-containing-sexp 27 0))

(let ([t (new racket:text%)])
  (send t insert "#lang scribble/base\n@a{\n4\n\n\t  c}")
  (send t skip-whitespace 29 'backward #t))

;;find-up-sexp
(let ([t (new racket:text%)])
  (send t insert "#lang scribble/base\n@a{\n'{'\n\n\t  c}")
  (send t find-up-sexp 29))
;find-up-sexp

(let ([t (new racket:text%)])
  (send t insert "#lang scribble/base\n@f[@x\n@y\n]");;"#lang scribble/base\n@a{\n{a} c}")
  (send t paragraph-end-position 4))
  ;;(send t find-up-sexp 24))
;;;;find-up-sexp

(define (txt-position-classify txt start end)
  (for/list ([x (in-range start end 1)])
    (send txt classify-position x)))
(let ([t (new racket:text%)])
  (send t insert "#lang scribble/base\ntest1 test2 @test3\n")
  (txt-position-classify t 19 100))

#|(displayln "current:")
                   (displayln para-end)
                   (displayln nxt-para-end);;test
|#