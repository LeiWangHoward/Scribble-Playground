#lang racket
#|simple rules:

1)tab adjusted

@itemlist[@item{item1}
          @item{item2}]

2)one-space adjusted

@centered{
 @bold{Cookies Wanted}
 @italic{Chocolate chip preferred!}
}

@a[ 
 test
 @b{c}
 @d{e}
]

3)line break rules:

starting with non white space:

Each paragraph shall not have more than 70 characters. The last "word"
will be passed to the next line

starting with several white space:

more than 65 char will be passed to next line
4) Tips:
 whenever there is a "\n", this line has a white-space
 (send a-text set-line-spacing space) → void?
 space : (and/c real? (not/c negative?))
|#
(require framework)

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

;;indent-racket-func: text position[natural] ->  1 for first #\(, or #f
(define (indent-racket-func txt posi)
  (let* ([prev-posi (sub1 posi)]
         [back-one-level (send txt backward-containing-sexp prev-posi 0)])
    (if back-one-level
        (let ([paren (sub1 back-one-level)])
          (if (or (equal? #\{ (send txt get-character paren))
                  (equal? #\[ (send txt get-character paren)))
              1
              #f))
        #f)))
;;count-parens: text position[natural] ->  number of parens including current one
(define (count-parens txt posi)
  (define count 0)
  (do ([p posi (send txt backward-containing-sexp p 0)])
    ((not p) count)
    (begin
      (set! p (sub1 p))
      ;(displayln (send txt get-character p))
      ;(displayln p)
      (when (or (equal? #\{ (send txt get-character p))
                (equal? #\[ (send txt get-character p)))
        (set! count (add1 count))))))

;;determine-spaces : text position[natural] -> spaces in front of current paragraph
(define (determine-spaces txt posi)
  (let* ([current-para (send txt position-paragraph posi)]
         ;;[para-paren (count-parens txt posi)]
         [para-start (send txt paragraph-start-position current-para)]
         [para-start-skip-space (send txt skip-whitespace para-start 'forward #t)];skip comment also
         [para-check (send txt position-paragraph para-start-skip-space)])
    (if (= para-check current-para);not an empty paragraph
        (let ([sexp-start-posi (send txt backward-containing-sexp para-start-skip-space 0)])
          (if sexp-start-posi
              (let* ((prev-posi (sub1 sexp-start-posi))
                     (this-para (send txt position-paragraph prev-posi)))
                ;(displayln txt_length)
                ;(displayln sexp-start-posi)
                ;(displayln (send txt get-character sexp-start-posi))
                (cond ((equal? #\[ (send txt get-character prev-posi))
                       (let ((this-para-start (send txt paragraph-start-position this-para)))
                         (if (= current-para this-para)
                             0
                             (add1 (- prev-posi this-para-start)))))
                      ;;if it is a racket function
                      ((equal? #\( (send txt get-character prev-posi))
                       (indent-racket-func txt prev-posi));call corresponding function
                      (else (count-parens txt sexp-start-posi))))  
              sexp-start-posi))
        #f)))

(define (reindent-and-save in outs)
  (define t (new racket:text%))
  (send t load-file in)
  (send t set-filename #f)
  (indent-all t)
  (call-with-output-file outs
    (λ (port)
      (display (send t get-text) port))
    #:exists 'truncate))

(define (indent-all t)
  (for ([i (in-range (send t last-paragraph) -1 -1)]);counting down from the last paragraph
         ;(in-range (send t last-paragraph) 1)])
    (define posi (send t paragraph-start-position i))
    ;;for line break
    (define para-end (send t paragraph-end-position i))
    (define para-length (add1 (- para-end posi)))
    (when (> para-length 70)
      (adjust-para t i para-end posi)
      (set! posi (send t paragraph-start-position i)))
    (define amount (determine-spaces t posi))
    (adjust-spaces t i amount posi)))

(define (adjust-spaces t para amount posi)
  (define posi-skip-space (send t skip-whitespace posi 'forward #f));not skip comments
  (define origin-amount (- posi-skip-space posi))
  (when amount
    (send t delete posi posi-skip-space)
    (when (> amount 0)
      (send t insert (make-string amount #\ ) posi))) 
  #t);;delete and insert

(define (adjust-para t para para-end posi); posi is paragraph start
  (define posi-skip-space (send t skip-whitespace posi 'forward #f));not skip comments
  (define new-length (add1 (- para-end posi-skip-space)))
  (when (> 70 new-length)
    (define new-start (- para-end 70))
    (send t delete (sub1 posi) posi-skip-space);clear out all white space, and previous line breaker
    (send t insert #\ new-start)
    (send t insert #\newline (add1 new-start)))
  #t)
(reindent-and-save (collection-file-path "interface-essentials.scrbl" "scribblings" "drracket") "x_auto.scrbl")
;;note 1: blank lines/comments cause the skip-space position larger than paragraph end position

;;;usage instructions
;; position-line, given: a position start from 0, return line number it is at
;(send txt3 position-line 20)

;; backward-match, backward-containing-sexp, forward-match
; beckward-match:given ) position find ( position
; forward-match:given ( position find ) position


;; position-paragraph (method of text%)  provided from racket/gui/base, racket/gui
;; given a position, return what paragraph it is at
;(send txt2 position-paragraph 24)

;; paragraph-end-position (method of text%)  provided from racket/gui/base, racket/gui
;; return the position before invisible item(newline)
;(send txt2 paragraph-end-position 0)

;; paragraph-start-position (method of text%)  provided from racket/gui/base, racket/gui
;(send txt2 paragraph-start-position 0)

;;[txt-length (send txt last-position)]
;[para_end (send txt paragraph-end-position current_para)]
(module+ test
  (require rackunit)
  (define txt_1 (new racket:text%))
  (send txt_1 insert "#lang scribble/base\n@f{x}")
  
  ;test is-at-sign
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "(x)")
                  (is-at-sign? t 0))
                #f)
  (check-equal? (is-at-sign? txt_1 20) #t)
  (check-equal? (is-at-sign? txt_1 22) #f)
  ;test determine-spaces
  (check-equal? (determine-spaces txt_1 15) #f)
  (check-equal? (determine-spaces txt_1 21) #f)
  
  (define txt_2 (new racket:text%))
  (send txt_2 insert "#lang scribble/base\n@f{\n @a\n@b\n}")
  (check-equal? (determine-spaces txt_2 25) 1)
  (check-equal? (determine-spaces txt_2 28) 1)
  
  (define txt_3 (new racket:text%))
  (send txt_3 insert "#lang scribble/base\n@f[@x\n@y\n]")
  (check-equal? (determine-spaces txt_3 24) #f) 
  (check-equal? (determine-spaces txt_3 31) 3)
  
  (define txt_4 (new racket:text%))
  (send txt_4 insert "#lang scribble/base\n@itemlist[@item{item1}\n@item{item2}\n]")
  ;;(itemlist (item "item1") (item "item2"))
  (check-equal? (determine-spaces txt_4 22) #f)
  (check-equal? (determine-spaces txt_4 44) 10)
  
  (define txt_5 (new racket:text%))
  (send txt_5 insert "#lang scribble/base\n@boldlist{@me{item1}\n@me{item2}\n}")
  (check-equal? (determine-spaces txt_5 31) #f)
  (check-equal? (determine-spaces txt_5 46) 1);;
  ;(check-equal? (count-parens txt_5 46) 1);;play
  
  (define txt_6 (new racket:text%))
  (send txt_6 insert "@list{@me{item1}\n\n@me{item2}\n}")
  (check-equal? (determine-spaces txt_6 16) #f)
  (check-equal? (determine-spaces txt_6 17) #f);empty line!
  (check-equal? (determine-spaces txt_6 18) 1)
  
  (define txt_7 (new racket:text%))
  (send txt_7 insert "@(define (foo . a)\n(bar b))")
  (check-equal? (determine-spaces txt_7 19) #f)
  
  (define txt_8 (new racket:text%))
  (send txt_8 insert "@a{me}\n@b[\n@c{@d{e} f\ng\nh}\n")
  (check-equal? (count-parens txt_8 22) 2)
  (check-equal? (count-parens txt_8 14) 2)
  (check-equal? (determine-spaces txt_8 22) 2)
  (check-equal? (determine-spaces txt_8 12) 1) 
  
  (define txt_9 (new racket:text%))
  (send txt_9 insert "@a[\n(b c)\n(d\n[(e) f]\n[g h])\n]\n")
  (check-equal? (indent-racket-func txt_9 4) 1)
  ;(check-equal? (indent-racket-func txt_9 10) 1)
  (check-equal? (indent-racket-func txt_9 6) #f)
  (check-equal? (determine-spaces txt_9 13) #f) 
  (check-equal? (determine-spaces txt_9 4) 1)
  ;;(check-equal? (send txt_9 backward-containing-sexp 13 0) 11) ;play
  ;(check-equal? (send txt_9 backward-containing-sexp 9 0) 3) ;play
  ;(check-equal? (send txt_8 get-character 22) #\g)
  ;;first test: able to process string correctly
  ;(check-equal? (send txt_4 last-position) 57)
  #|(check-equal? (txt-position-classify (txt-position-classify txt_2))  
        '(other other other other other other other other other other 
                other other other other other other other other other ;;19 other, represents #lang...
                parenthesis symbol parenthesis white-space string parenthesis))|#
  #|
  (check-equal? (send txt_4 backward-match 35 0) 31) ;;not sure about "cutoff"
  (check-equal? (send txt_4 forward-match 29 100) 57) 
  (check-equal? (send txt_2 forward-match 22 0) #f) 
  (check-equal? (send txt_4 forward-match 31 0) 35)
  (check-equal? (send txt_4 forward-match 43 0) 44) 
  (check-equal? (send txt_4 backward-containing-sexp 29 0) 0);;start from #lang???
  (check-equal? (send txt_4 backward-containing-sexp 35 0) 30);;[@item{item1}
  |#
  #|
  (define txt_6 (new racket:text%))
  (send txt_6 insert "#lang racket\n(define (me)\n(let ((a 1))\n(b (c 1)\n(d 2))))")
  (check-equal? (send txt_6 backward-containing-sexp 46 100) 43)
  (check-equal? (send txt_6 backward-containing-sexp 40 100) 40)
  (check-equal? (send txt_6 backward-containing-sexp 42 100) 40)
  
  (check-equal? (send txt_4 backward-containing-sexp 29 (send txt_4 last-position)) #f);;[@item...
  
  (check-equal? (send txt_5 backward-containing-sexp 33 (send txt_5 last-position)) 30);;@me{item
  (check-equal? (send txt_5 backward-containing-sexp 36 (send txt_5 last-position)) 34);;item
  (check-equal? (send txt_5 backward-containing-sexp 32 (send txt_5 last-position)) 30)
  
  ;(check-equal? (let-values ([(start end)(send txt_1 get-token-range 22)]) end) 23);based on paragraph
  
  ;(check-equal? (send txt_1 forward-match 22 100) 25) |#
  #|(check-equal? (let ([t (new racket:text%)])
                  (send t insert "  (niubi)")
                  (send t delete 0 2)
                  (send t get-character 0))
                '#\()
  (check-equal? (let ([t2 (new racket:text%)])
                  (send t2 insert " woshishui")
                  (send t2 insert "  " 0)
                  (send t2 get-character 3))
                '#\w)
  (check-equal? (make-string 3 #\ ) "   ")|#
  )