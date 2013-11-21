#lang racket
(require "scribble-indentation.rkt")
(require framework)

(provide determine-spaces)
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
         (in-range 20 (send t last-paragraph) 1)]);counting up from first paragraph
    (define posi (send t paragraph-start-position i))
    (define amount (determine-spaces t posi))
    (begin (adjust-spaces t i amount posi)
           (adjust-para-width t posi 50)
           )))

(define (adjust-spaces t para amount posi)
  (define posi-skip-space (send t skip-whitespace posi 'forward #f));not skip comments
  (define origin-amount (- posi-skip-space posi))
  (when amount
    (send t delete posi posi-skip-space)
    (when (> amount 0)
      (send t insert (make-string amount #\ ) posi))) 
  #t);;delete and insert

(reindent-and-save (collection-file-path "interface-essentials.scrbl" "scribblings" "drracket") "x_width.scrbl")