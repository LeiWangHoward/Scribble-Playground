#lang racket
#|
Indentation rules:

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

Line breaking rules:

1)Only handle string inside {} or pure string

2)Each paragraph shall not have more/less than given character number(e.g. 70). The last few "words"
will be passed to the next line, or some "words" from the next line will be pass back to current line|#

(require framework)

;;first basic position classify method: text start[natural] end[natural] ->
;position classify of the text in certain range
(define (txt-position-classify txt start end)
  (for/list ([x (in-range start end 1)])
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
  (let ([para-start (send txt paragraph-start-position para)]
        [para-end (send txt paragraph-end-position para)])
    (for/first ([new-break (in-range para-start para-end 1)]
                #:unless (and (char-whitespace? (send txt get-character new-break))
                              (not (member (send txt get-character new-break)
                                           '(#\newline #\return)))))
      #t)))

;;not-empty?: classify results[list] -> #t/#f
(define (para-not-empty? classify-lst) ;;we consider 'other  and 'comment as empty
  (if (or (member 'parenthesis classify-lst)
          (member 'string classify-lst)
          (member 'symbol classify-lst))
      #t
      #f))

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

;;indent-racket-func: text position[natural] ->  1 for first #\(, or #f
(define (indent-racket-func txt posi)
  (let* ([prev-posi (sub1 posi)]
         [back-one-level (send txt backward-containing-sexp prev-posi 0)])
    (if back-one-level
        (let ([paren (sub1 back-one-level)])
          (cond ((equal? #\{ (send txt get-character paren)) 1)
                ((equal? #\[ (send txt get-character paren));might be inside cond etc
                 (let ([back-two-level (send txt backward-containing-sexp (sub1 paren) 0)])
                   (if back-two-level
                       #f
                       1)))
                (else #f)))
        #f)));;#f needs to be replaced by racket indentation function

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

;;select-cut-option : text start[natural] end[natural] -> cut the line by selecting 
;;1) breake whole @... to next line, 
;;2) keep @.... in current line 
;;3) if it is a simple string, just cut it
(define (select-cut-option txt start len width classify-lst)
  (let ([adjust-result (list-ref classify-lst (sub1 width))]);;get the "end" position adjust result
    (cond ((equal? adjust-result 'string) 
           (for/first ([new-break (in-range (+ start width) (+ start len))] ;;todo: make it search back also
                       #:when (equal? #\space (send txt get-character new-break)))
             (send txt delete (add1 new-break) 'back)
             (send txt insert #\newline new-break)));;replace the #\space with #\newline
          ;((equal? adjust-result 'symbol) #t);;inside @
          ;((equal? adjust-result 'parenthesis) #t) ;;could be @
          ;;'symbol 'parenthesis or 'space
          (else (let ([string-posi 
                       (for/last ([posi (in-range width)] 
                                  #:when (equal? 'string (list-ref classify-lst posi)))
                         posi)])
                  (when (< (- width string-posi) (- len width))
                    (send txt insert #\newline (+ start string-posi))))))))


;;adjust-para-width : text position[natural] width[natural] -> modify the paragraph of given position if its
;;                    excedded the width limit, shorter than the limit, or return #f
(define (adjust-para-width txt posi width)
  (let* ([para-num (send txt position-paragraph posi)]
         [para-start (send txt paragraph-start-position para-num)]
         [para-end (send txt paragraph-end-position para-num)]
         [para-len (add1 (- para-end para-start))]
         [para-classify (txt-position-classify txt para-start para-end)])
    (if (para-not-empty? para-classify)
        (cond ((> para-len width)
               (select-cut-option txt para-start para-len width para-classify)
               (if (is-non-empty-paragraph? txt (+ para-num 2)) ;; next paragraph not empty
                   (begin (delete-end-spaces txt (+ para-num 1))
                          (delete-start-spaces txt (+ para-num 2))
                          (let* ([nxt-para-num (+ para-num 2)]
                                 [nxt-para-start (send txt paragraph-start-position nxt-para-num)]
                                 [nxt-para-end (send txt paragraph-end-position nxt-para-num)]
                                 [nxt-para-classify (txt-position-classify txt nxt-para-start nxt-para-end)])
                          (when (equal? 'string (first nxt-para-classify))
                            (send txt delete nxt-para-start 'back)
                            (send txt insert #\space (sub1 nxt-para-start)))))
                   
                   #t))
              ;;now determine if the next paragraph will be "push up"
              ((< para-len width)#t);;only conside string
              (else #t))
        #t)))

;;for play
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
           ;(adjust-para-width t posi 50)
           )))

(define (adjust-spaces t para amount posi)
  (define posi-skip-space (send t skip-whitespace posi 'forward #f));not skip comments
  (define origin-amount (- posi-skip-space posi))
  (when amount
    (send t delete posi posi-skip-space)
    (when (> amount 0)
      (send t insert (make-string amount #\ ) posi))) 
  #t);;delete and insert

(reindent-and-save (collection-file-path "interface-essentials.scrbl" "scribblings" "drracket") "x_new.scrbl")
;;test cases
(module+ test
  (require rackunit)
  (define txt_1 (new racket:text%))
  (send txt_1 insert "#lang scribble/base\n@f{x}\n@;ghj\ntyty\n\n")
  
  ;test para-not-empty?
  (check-equal? (let ([result (txt-position-classify txt_1 0 5)])
                  (para-not-empty? result))
                #f);;consider 'other as empty line
  
  (check-equal? (let ([result (txt-position-classify txt_1 20 24)])
                  (para-not-empty? result))
                #t)
  
  (check-equal? (let ([result (txt-position-classify txt_1 27 31)])
                  (para-not-empty? result))
                #f);comment
  
  (check-equal? (let ([result (txt-position-classify txt_1 37 38)])
                  (para-not-empty? result))
                #f);empty line
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
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "(\n  \nx)")
                  (is-non-empty-paragraph? t 1))
                #f)
  
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
  (check-equal? (determine-spaces txt_5 46) 1)
  
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
  (check-equal? (indent-racket-func txt_9 6) #f)
  (check-equal? (determine-spaces txt_9 13) #f) 
  (check-equal? (determine-spaces txt_9 4) 1)
  
  (define txt_10 (new racket:text%))
  (send txt_10 insert "(d f\n(l [()\n(f ([a (b c)])\n(d e)))])")
  (check-equal? (indent-racket-func txt_10 12) #f)      
  
  ;;test cases for:delete-end-spaces delete-start-spaces
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "{abcde   \nfgh\n}")
                  (delete-end-spaces t 0)
                  (send t get-text)) "{abcde\nfgh\n}")
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "{abcde   \n\n3\n}")
                  (delete-end-spaces t 0)
                  (send t get-text)) "{abcde\n\n3\n}")
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "  {abcde\nfgh\n}")
                  (delete-start-spaces t 0)
                  (send t get-text)) "{abcde\nfgh\n}")
  ;;test case for adjust paragraph width 
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang scribble/base\naaa bbb ccc ddd @e[f @g{h}]")
                  (adjust-para-width t 22 9) 
                  (send t get-text))
                "#lang scribble/base\naaa bbb ccc\nddd @e[f @g{h}]")
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang scribble/base\na b c d @e[f @g{h}]")
                  (adjust-para-width t 21 9) 
                  (send t get-text))
                "#lang scribble/base\na b c d\n @e[f @g{h}]");;keep the space, does not matter
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang scribble/base\na b c d @e{}\n f g\n")
                  (adjust-para-width t 21 9) 
                  (send t get-text))
                "#lang scribble/base\na b c d\n @e{} f g\n")
  )