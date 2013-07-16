#lang racket
#|simple result
1)function "tab adjusted"

@itemlist[@item{item1}
          @item{item2}]

2)String "one-space adjusted"

@centered{
 @bold{Cookies Wanted}
 @italic{Chocolate chip preferred!}
}
|#

(require framework)

(define txt (new racket:text%))
(send txt insert "#lang scribble/base\n@itemlist[@item{item1}\n
                                @item{item2}\n]");#lang scribble/base\n@f{x}")

;;first basic position classify method
(define (txt-position-classify txt)
  (for/list ([x (in-range (send txt last-position))])
    (send txt classify-position x)))

(send txt get-token-range 0)

(send txt paragraph-start-position 1)

(define txt2 (new racket:text%))
(send txt2 insert "#lang scribble/base\n@f{\n x\n}")
#|should be:

#lang scribble/base
@f{
 x
}

after adjusted|#

(define txt3 (new racket:text%))
(send txt3 insert "#lang scribble/base\n@f[\n x\n]")

(define (space-filter-inserter txt) 
  txt)
;; position-line, 

;; backward-match, backward-containing-sexp, forward-match

;; determine-spaces : text position[natural] -> spaces @ front or #f

; position-paragraph (method of text%)  provided from racket/gui/base, racket/gui
; paragraph-end-position (method of text%)  provided from racket/gui/base, racket/gui
; paragraph-start-position (method of text%)  provided from racket/gui/base, racket/gui


;;test cases
(require plai)
(module+ test
  ;;first test: able to process string correctly
  (test (txt-position-classify (space-filter-inserter txt2))  
        '(other other other other other other other other other other 
                other other other other other other other other other ;;19 other, represents #lang...
                parenthesis symbol parenthesis white-space string parenthesis)))