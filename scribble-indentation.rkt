#lang racket
(require framework)

;;paragraph-indentation : txt[text] posi[natural] width[natural] -> indentation result
(define (paragraph-indentation txt posi width)
  (let* ([current-line (send txt position-paragraph posi)]
         [para-start-line (for/first ([line (in-range current-line 0 -1)]
                                      #:when (empty-line? txt line))
                            line)])
    (if para-start-line
        (for ([i (in-range (+ para-start-line 1) (+ para-start-line 1000) 1)]
              #:break  (empty-line? txt i))
          (define posi (send txt paragraph-start-position i))
          (define amount (determine-spaces txt posi))
          (begin (adjust-spaces txt i amount posi)
                 (adjust-para-width txt posi width)))
        #f)))

(define (empty-line? txt line)
  (let* ([line-start (send txt paragraph-start-position line)]
         [line-end (send txt paragraph-end-position line)]
         [line-classify (txt-position-classify txt line-start line-end)])
    (not (para-not-empty? line-classify))))

;;determine-spaces : text position[natural] -> spaces in front of current paragraph (end in "\n")
(define (determine-spaces txt posi)
  (let* ([current-para (send txt position-paragraph posi)]
         [para-start (send txt paragraph-start-position current-para)]
         [para-start-skip-space (send txt skip-whitespace para-start 'forward #t)]);skip comment also
    (if (not (empty-line? txt current-para));not an empty paragraph/comment string
        (let ([sexp-start-posi (send txt backward-containing-sexp para-start-skip-space 0)])
          (if sexp-start-posi
              (let* ((prev-posi (sub1 sexp-start-posi))
                     (this-para (send txt position-paragraph prev-posi)))
                ;(displayln sexp-start-posi);;test
                (cond ((equal? #\[ (send txt get-character prev-posi))
                       (let ((this-para-start (send txt paragraph-start-position this-para)))
                         (if (= current-para this-para)
                             0
                             (add1 (- prev-posi this-para-start)))))
                      ;;if it is inside a racket function and not the first line of the racket function
                      ((equal? #\( (send txt get-character prev-posi))
                       (indent-racket-func txt prev-posi));call corresponding function to indent racket stuff
                      (else (count-parens txt sexp-start-posi))))  
              sexp-start-posi))
        #f)))

;;adjust-para-width : text position[natural] width[natural] -> modify the paragraph of given position if its
;;                    excedded the width limit, shorter than the limit, or return #f
(define (adjust-para-width txt posi width)
  (let* ([para-num (send txt position-paragraph posi)]
         [para-start (send txt paragraph-start-position para-num)]
         [para-end (send txt paragraph-end-position para-num)]
         [para-len (add1 (- para-end para-start))]
         [para-classify (txt-position-classify txt para-start para-end)])
    (if (para-not-empty? para-classify) ;continue when current paragraph is not empty
        (cond ((> para-len width) ;paragraph too long
               (define new-line-created (select-cut-option txt para-start para-len width para-classify))
               (when (equal? new-line-created #t)
                 (let* ([next-para-num (+ para-num 2)]
                        [next-para-start (send txt paragraph-start-position next-para-num)]
                        [next-para-end (send txt paragraph-end-position next-para-num)]
                        [next-para-classify (txt-position-classify txt next-para-start next-para-end)])
                   (if (para-not-empty? next-para-classify) ;; next paragraph not empty
                       (begin (delete-end-spaces txt (+ para-num 1))
                              (delete-start-spaces txt (+ para-num 2))
                              (let* ([nxt-para-num (+ para-num 2)]
                                     [nxt-para-start (send txt paragraph-start-position nxt-para-num)]
                                     [nxt-para-end (send txt paragraph-end-position nxt-para-num)]
                                     [nxt-para-classify (txt-position-classify txt nxt-para-start nxt-para-end)])
                                (when (equal? 'text (first nxt-para-classify))
                                  ;now text
                                  (send txt delete nxt-para-start 'back)
                                  (send txt insert #\space (sub1 nxt-para-start)))))     
                       #t))))
              ;;now determine if the next paragraph will be "push up"
              ((< para-len width) #t);;only consider text
              (else #t))
        #t)))

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

;;not-empty?: classify results[list] -> #t/#f
(define (para-not-empty? classify-lst) ;;we consider 'other  and 'comment as empty
  (if (or (member 'parenthesis classify-lst)
          (member 'string classify-lst)
          (member 'symbol classify-lst)
          (member 'text classify-lst))
      #t
      #f))
;;start-skip-spaces: return the position of first non #\space position from current position
(define (start-skip-spaces txt para)
  (let* ([para-start (send txt paragraph-start-position para)]
         [para-end (send txt paragraph-end-position para)])
    (for/first ([start-skip-space (in-range para-start para-end 1)]
                #:when (not (equal? #\space (send txt get-character start-skip-space))))
      start-skip-space)))

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

;;select-cut-option : text start[natural] end[natural] -> new line generalized [1/0]
;;1) breake whole @... to next line, 
;;2) keep @.... in current line 
;;3) if it is a simple text, just cut it
(define (select-cut-option txt start len width classify-lst)
  (let ([adjust-result (list-ref classify-lst (sub1 width))]);;get the "end" position adjust result
    (cond [(equal? adjust-result 'text) 
           (let ([new-break (insert-break-text txt start (+ start width) (+ start len))])
             (if new-break
                 ;replace the #\space with #\newline 
                 (begin (send txt delete (add1 new-break) 'back)
                        (send txt insert #\newline new-break)
                        #t)
                 #f))]
          ;;'symbol 'parenthesis 'string or 'space
          ;;1)went backward to find @
          ;;2)went forward to find first 'text
          [else 
           (let ([posi (insert-break-func txt start len width classify-lst)])
             (if posi
                 (begin (send txt insert #\newline posi);;directly insert before @ or 'text
                        #t)
                 #f))])))

;;insert-break-text : text start[natural] width-end[natural] end[natural] -> position[natural]/#f
(define (insert-break-text t start width-end end)
  (for/first ([break-ls (in-range width-end start -1)]
              #:when (equal? #\space (send t get-character break-ls)))
    break-ls))

;;insert-break-func : text start[natural] len[natural] width[natural] classify-result[list] 
;-> position[natural]/#f
(define (insert-break-func t start len width classify-lst)
  (let ([at-sign-posi
         (for/first ([sign-posi (in-range (+ start width) start -1)]
                     #:when (is-at-sign? t sign-posi))
           sign-posi)])
    (if (and at-sign-posi
             (not (equal? 'white-space (list-ref classify-lst (sub1 (- at-sign-posi start))))))
        at-sign-posi
        (for/first ([posi (in-range width (- len 1))] 
                    #:when (equal? 'text (list-ref classify-lst posi)))
          posi))))

;;adjust-spaces for text
(define (adjust-spaces t para amount posi)
  (define posi-skip-space (send t skip-whitespace posi 'forward #f));not skip comments
  (define origin-amount (- posi-skip-space posi))
  (when amount
    (send t delete posi posi-skip-space)
    (when (> amount 0)
      (send t insert (make-string amount #\space) posi))) 
  #t)

;;test cases
(module+ test
  (require rackunit)
  
  ;test start-skip-spaces
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "{abcd\n  efgh\n}")
                  (start-skip-spaces t 1)) 8)
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "{abc\n       \n}")
                  (start-skip-spaces t 1)) #f)
  
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
  
  (check-equal? (let ([txt_7 (new racket:text%)])
                  (send txt_7 insert "@(define (foo . a)\n(bar b))")
                  (determine-spaces txt_7 19)) #f)
  
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
  
  (define txt_11 (new racket:text%))
  (send txt_11 insert "@a[\n     ]\n")
  (check-equal? (determine-spaces txt_11 4) 0)      
  
  
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
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "@a[\n      ]\n")
                  (delete-start-spaces t 1)
                  (send t get-text)) "@a[\n]\n")
  
  ;;adjust-spaces
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "@a[\n     ]\n")
                  (adjust-spaces t 1 1 4)
                  (adjust-spaces t 1 1 4)
                  (send t get-text)) "@a[\n ]\n")
  
  ;;paragraph indentation
  
  ;;test case for adjust paragraph width 
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang scribble/base\naaa bbb ccc ddd @e[f @g{h}]")
                  (adjust-para-width t 22 12) 
                  (send t get-text))
                "#lang scribble/base\naaa bbb ccc\nddd @e[f @g{h}]")
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang scribble/base\na b c d @e[f @g{h}]")
                  (adjust-para-width t 21 9) 
                  (send t get-text))
                "#lang scribble/base\na b c d \n@e[f @g{h}]");;keep the space, does not matter
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang scribble/base\na b c d @e{}\n f g\n")
                  (adjust-para-width t 21 9) 
                  (send t get-text))
                "#lang scribble/base\na b c d \n@e{} f g\n")
  
  ;;test insert-break
  (check-equal? (let ((t (new racket:text%)))
                  (send t insert "aaa bbb ccc ddd")
                  (let ([new-break (insert-break-text t 0 6 14)])
                    (send t delete (add1 new-break) 'back)
                    (send t insert #\newline new-break)
                    (send t get-text))) "aaa\nbbb ccc ddd");;prefer shorter than width
  
  ;;for the situation there isn't any #\space on right side 
  (check-equal? (let ((t (new racket:text%)))
                  (send t insert "aaaa bbbb")
                  (let ([new-break (insert-break-text t 0 5 8)])
                    (send t delete (add1 new-break) 'back)
                    (send t insert #\newline new-break)
                    (send t get-text))) "aaaa\nbbbb") 
  ;;if @ at the very beginning of the line, it follows the more powerful rule, don't touch it.
  ;; if it is indented, do something following rules
  )

(provide determine-spaces adjust-para-width paragraph-indentation)


;;is-at-sign not good, [] 
;;don't get actual character
;; atsign, [text, {text, text 