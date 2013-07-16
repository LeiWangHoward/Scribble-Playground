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

3) Tips:
 whenever there is a "\n", this line has a white-space
|#
(require framework)
;;txt used for testing
(define txt (new racket:text%))
(send txt insert "#lang scribble/base\n@itemlist[@item{item1}\n
                                @item{item2}\n]");#lang scribble/base\n@f{x}")
(define txt2 (new racket:text%))
(send txt2 insert "#lang scribble/base\n@f{\n x\n}")

(define txt3 (new racket:text%))
(send txt3 insert "#lang scribble/base\n@f[\n x\n]\n")


;;first basic position classify method: text -> position classify of the whole text
(define (txt-position-classify txt)
  (for/list ([x (in-range (send txt last-position))])
    (send txt classify-position x)))

;;determine-spaces : text position[natural] -> spaces @ front or #f
;;should be paragraph position
(define (determine-spaces txt posi)      
  #f)
;(send txt get-token-range 0)

;;next step
(define (space-filter-inserter txt) 
  txt)

;;;usage instructions
;; position-line, given: a position start from 0, return line number it is at
 ;(send txt3 position-line 20)

;; backward-match, backward-containing-sexp, forward-match


;; position-paragraph (method of text%)  provided from racket/gui/base, racket/gui
;; given a position, return what line it is at
 ;(send txt2 position-paragraph 24)
;; paragraph-end-position (method of text%)  provided from racket/gui/base, racket/gui
;; return the position before invisible item(newline)
 ;(send txt2 paragraph-end-position 0)

;; paragraph-start-position (method of text%)  provided from racket/gui/base, racket/gui
 ;(send txt2 paragraph-start-position 0)

;(txt-position-classify txt3)
;;test cases
(require plai)
(define txt_1 (new racket:text%))
(send txt_1 insert "#lang scribble/base\n@f{x}")
(define txt_2 (new racket:text%))
(send txt_2 insert "#lang scribble/base\n@f{\n x\n}")
(define txt_3 (new racket:text%))
(send txt_3 insert "#lang scribble/base\n@f[;what\n x\n]")
(define txt_4 (new racket:text%))
(send txt_4 insert "#lang scribble/base\n@itemlist[@item{item1}\n
                                                   @item{item2}\n]")

(module+ test
  (test (determine-spaces txt_1 0) #f)
  (test (determine-spaces txt_1 1) #f)
  (test (determine-spaces txt_2 2) 1)
  (test (determine-spaces txt_4 2) #f)
  (test (determine-spaces txt_4 3) 10)
  ;;first test: able to process string correctly
  (test (txt-position-classify (space-filter-inserter txt2))  
        '(other other other other other other other other other other 
                other other other other other other other other other ;;19 other, represents #lang...
                parenthesis symbol parenthesis white-space string parenthesis)))