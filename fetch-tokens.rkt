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
 (send a-text set-line-spacing space) → void?
 space : (and/c real? (not/c negative?))
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

;;is-at-sign: check if the given position is an @: text position[natural] -> #t/#f
(define (is-at-sign? txt posi)
  (and (equal? (send txt classify-position posi) 'parenthesis)
       (let-values ([(start end) (send txt get-token-range posi)])
         (and (equal? start posi)
              (equal? end (+ posi 1))))
       (equal? #\@ (send txt get-character posi))))

;;is-parenthesis: check if given position is a parenthesis
(define (is-parenthesis txt posi)
  (if (equal? (send txt classify-position posi) 'parenthesis)
      (not (equal? (send txt classify-position (add1 posi)) 'symbol))
      #f))
;;determine-spaces : text position[natural] -> spaces @ front or #f
(define (determine-spaces txt posi)
  (let* ([current_para (send txt position-paragraph posi)]
         [para_start (send txt paragraph-start-position current_para)]
         [para_start_skip_space (send txt skip-whitespace para_start 'forward #t)];skip comment also
         [current_space (- para_start_skip_space para_start)];; current already have space
         [para_end (send txt paragraph-end-position current_para)])
    (if (is-at-sign? txt para_start_skip_space);if the biginning is not an @, just return #f
        (- 1 current_space)
        #f)))
;(send txt get-token-range 0)

;;next step
(define (space-filter-inserter txt) 
  txt)

;;;usage instructions
;; position-line, given: a position start from 0, return line number it is at
;(send txt3 position-line 20)

;; backward-match, backward-containing-sexp, forward-match
; beckward-match:given ) position find ( position
; forward-match:given ( position find ) position


;; position-paragraph (method of text%)  provided from racket/gui/base, racket/gui
;; given a position, return what line it is at
;(send txt2 position-paragraph 24)
;; paragraph-end-position (method of text%)  provided from racket/gui/base, racket/gui
;; return the position before invisible item(newline)
;(send txt2 paragraph-end-position 0)

;; paragraph-start-position (method of text%)  provided from racket/gui/base, racket/gui
;(send txt2 paragraph-start-position 0)

(module+ test
  (require rackunit)
  
  (define txt_1 (new racket:text%))
  (send txt_1 insert "#lang scribble/base\n@f{x}")
  (define txt_2 (new racket:text%))
  (send txt_2 insert "#lang scribble/base\n@f{\n @a\n@b\n}")
  (define txt_3 (new racket:text%))
  (send txt_3 insert "#lang scribble/base\n@f[;what\n x\n]")
  (define txt_4 (new racket:text%))
  (send txt_4 insert "#lang scribble/base\n@itemlist[@item{item1}\n@item{item2}\n]")
  
  (check-equal? (send txt_4 backward-match 35 0) 31) ;;not sure about "cutoff"
  (check-equal? (send txt_4 forward-match 29 100) 57) ;;not working properly on different lines? 
  (check-equal? (send txt_2 forward-match 22 0) #f) 
  (check-equal? (send txt_4 forward-match 31 0) 35)
  (check-equal? (send txt_4 forward-match 43 0) 44) ;; the whole thing is a string????
  (check-equal? (send txt_4 backward-containing-sexp 29 0) 0);;start from #lang???
  (check-equal? (send txt_4 backward-containing-sexp 35 0) 30);;@item{item1}
  ;test is-at-sign is-parenthesis
  (check-equal? (is-at-sign? txt_1 20) #t)
  (check-equal? (is-at-sign? txt_1 22) #f)
  (check-equal? (is-parenthesis txt_1 22) #t)
  (check-equal? (is-parenthesis txt_1 23) #f)
  ;test skip-white-space
  (check-equal? (send txt_2 skip-whitespace 23 'forward #t) 25)
  ;test determine-spaces
  (check-equal? (determine-spaces txt_1 15) #f)
  (check-equal? (determine-spaces txt_1 21) #f)
  (check-equal? (determine-spaces txt_2 25) 1)
  (check-equal? (determine-spaces txt_2 28) 1)
  (check-equal? (determine-spaces txt_4 22) #f)
  (check-equal? (determine-spaces txt_4 43) 10)
  
;;first test: able to process string correctly
#|(check-equal? (txt-position-classify (space-filter-inserter txt2))  
        '(other other other other other other other other other other 
                other other other other other other other other other ;;19 other, represents #lang...
                parenthesis symbol parenthesis white-space string parenthesis))|#
  
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "(x)")
                  (is-at-sign? t 0))
                #f))