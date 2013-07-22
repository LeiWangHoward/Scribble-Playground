#lang at-exp racket

'@foo{blah blah blah}

(define (foo str) (printf "He wrote ~s.\n" str))
  @foo{blah blah blah}
  
'@foo[1 2]{3 4}
	
'@foo{bar @baz{3}
     blah}