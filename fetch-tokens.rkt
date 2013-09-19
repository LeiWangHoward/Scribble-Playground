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

3)line breaking rules:

Only handle string inside {} or pure string

Each paragraph shall not have more/less than given character number(e.g. 70). The last few "words"
will be passed to the next line, or some words from the next line will be pass back to current line

4) Tips:
 whenever there is a "\n", this line has a white-space
 (send a-text set-line-spacing space) → void?
 space : (and/c real? (not/c negative?))
|#

;;5) helper function: {}
(require framework)

;;first basic position classify method: text -> position classify of the whole text
(define (txt-position-classify txt)
  (for/list ([x (in-range (send txt last-position))])
    (send txt classify-position x)))

;;is-at-sign?: check if the given position is an @: text position[natural] -> #t/#f
(define (is-at-sign? txt posi)
  (and (equal? (send txt classify-position posi) 'parenthesis)
       (let-values ([(start end) (send txt get-token-range posi)])
         (and (equal? start posi)
              (equal? end (+ posi 1))))
       (equal? #\@ (send txt get-character posi))))

;;is-non-empty-paragraph?: check if the given paragraph is a valid paragraph: text position[natural] -> #t/#f
(define (is-non-empty-paragraph? txt para)
  (let* ([para-start (send txt paragraph-start-position para)]
         [para-start-skip-space (send txt skip-whitespace para-start 'forward #t)];skip comment also
         [para-check (send txt position-paragraph para-start-skip-space)])
    (if (= para-check para)
        #t
        #f)))

;;delete-end-spaces: delete all #\space at the end of current paragraph
(define (delete-end-spaces txt para)
  (let* ([para-end (send txt paragraph-end-position para)]
         [last-non-white (send txt skip-whitespace para-end 'backward #f)]);do not skip comment
    (send txt delete last-non-white para-end)))

;;delete-end-spaces: delete all #\space at the beginning of current paragraph
(define (delete-start-spaces txt para)
  (let* ([para-start (send txt paragraph-start-position para)]
         [first-non-white (send txt skip-whitespace para-start 'forward #f)]);do not skip comment
    (when (> first-non-white para-start)
      (send txt delete para-start first-non-white))))

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
                      ;;if it is inside a racket function and not the first line of the racket function
                      ((equal? #\( (send txt get-character prev-posi))
                       (indent-racket-func txt prev-posi));call corresponding function
                      (else (count-parens txt sexp-start-posi))))  
              sexp-start-posi))
        #f)))

;;adjust-para-width : text position[natural] width[natural] -> modify the paragraph of given position if its
;;                    inside "{}" and excedded the width limit, or return #f
(define (adjust-para-width txt posi width)
  (let* ([current-para (send txt position-paragraph posi)]
         [next-para (add1 current-para)]
         [para-start (send txt paragraph-start-position current-para)]
         [para-skip-space (send txt skip-whitespace para-start 'forward #t)]
         [current-para-parent (send txt backward-containing-sexp para-skip-space 0)]
         [next-para-start (send txt paragraph-start-position next-para)]
         [next-skip-space (send txt skip-whitespace next-para-start 'forward #t)]
         [next-para-parent (send txt backward-containing-sexp next-skip-space 0)])
    ;;filter out all empty space at the end of current paragraph and the next paragraph
    (delete-end-spaces txt next-para)
    (if (and (equal? current-para-parent next-para-parent);1) both string 2) both inside same {} or []
             (is-non-empty-paragraph? txt current-para));current paragraph shall not be empty
        (begin
          (delete-end-spaces txt current-para)
          (let* ([para-end (send txt paragraph-end-position current-para)]
                 [current-para-length (add1 (- para-end para-start))]
                 [difference (add1 (- current-para-length width))])
            ;first clean up all start spsces at the next paragraph
            (cond ((> difference 0);too long
                   (when (is-non-empty-paragraph? txt next-para)
                     ;if next paragraph not empty, replace newline with space
                     (delete-end-spaces txt next-para) ;delete all space at end
                     (delete-start-spaces txt next-para);delete all space in front
                     (send txt delete (add1 para-end) 'back)
                     ;delete #\newline, now para-end represents (add1 para-end) 
                     (send txt insert #\space para-end));insert space
                   (for/first ([new-break (in-range (- para-end difference) para-end 1)]
                               #:when (equal? #\space (send txt get-character new-break)))
                     (send txt delete (add1 new-break) 'back) 
                     ;delete #\space, new-break now represents the non-space char after
                     (send txt insert #\newline new-break)))
                  ((< difference 0);too short
                   (when (is-non-empty-paragraph? txt next-para);non-empty? give back chars
                     (delete-end-spaces txt next-para) ;delete all space at end
                     (delete-start-spaces txt next-para);delete all space in front
                     (send txt delete (add1 para-end) 'back)
                     (send txt insert #\space para-end)
                     (define next-end (send txt paragraph-end-position next-para))
                     (for/first ([new-break (in-range (- para-end difference) next-end 1)]
                                 #:when (equal? #\space (send txt get-character new-break)))
                       (send txt insert #\newline new-break))))
                  (else #f))))
        #f)))

;;keymap%
#|
[9/12/13 3:25:40 PM] Robby Findler: (define f (new frame% [label ""] [width 400] [height 600]))
[9/12/13 3:25:43 PM] Robby Findler: (define t (new text%))
[9/12/13 3:25:55 PM] Robby Findler: (define ec (new editor-canvas% [parent f] [editor t]))
[9/12/13 3:26:00 PM] Robby Findler: (send t set-keymap …)
[9/12/13 3:26:06 PM] Robby Findler: (send f show #t)
(send t load-file ….)
|#
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
  (for ([i ;(in-range (send t last-paragraph) -1 -1)]);counting down from the last paragraph
         (in-range 0 (send t last-paragraph) 1)]);counting up from first paragraph
    (define posi (send t paragraph-start-position i))
    (define amount (determine-spaces t posi))
    (begin (adjust-spaces t i amount posi)
           (adjust-para-width t posi 50))))

(define (adjust-spaces t para amount posi)
  (define posi-skip-space (send t skip-whitespace posi 'forward #f));not skip comments
  (define origin-amount (- posi-skip-space posi))
  (when amount
    (send t delete posi posi-skip-space)
    (when (> amount 0)
      (send t insert (make-string amount #\ ) posi))) 
  #t);;delete and insert

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
  
  ;test (is-non-empty-paragraph? txt para)
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "(\n\nx)")
                  (is-non-empty-paragraph? t 1))
                #f)
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "(\n\nx)")
                  (is-non-empty-paragraph? t 0))
                #t)
  
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
  
  ;;test cases for:delete-end-spaces delete-start-spaces
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "{abcde   \nfgh\n}")
                  (delete-end-spaces t 0)
                  (send t get-character 6)) #\newline)
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "{abcde   \n\n3\n}")
                  (delete-end-spaces t 0)
                  (send t get-character 7)) #\newline)
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "  {abcde\nfgh\n}")
                  (delete-start-spaces t 0)
                  (send t get-character 0)) #\{)
  #|(check-equal? (let ([t (new racket:text%)])
                  (send t insert "{abcde   \nfgh\n}")
                  (send t delete 2 'back)
                  (send t get-character 1)) #\a)|# ;;deletes the char at position 1
  ;;test cases for: adjust-para-width
  ;;todo: how to compare text string!
  #|(check-equal? (let ([t (new racket:text%)])
                  (send t insert "{abcde\nfgh\n}")
                  (adjust-para-width t 1 4)
                  (display t))
                "abcd\nefgh\n")|#
  
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
                  (send t2 insert " 4" 1)
                  (send t2 get-character 2))
                '#\4)
  (check-equal? (make-string 3 #\ ) "   ")|#
  )