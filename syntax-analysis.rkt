#lang at-exp racket

'@foo{1 2 3}

(define (foo str) (printf "He wrote ~s.\n" str))
  @foo{blah blah blah}
  
'@foo[1 2]{3 4}

'@foo[1 2]{3 4}

'@foo{bar @baz{3}
     blah}

	
'@foo{(+ 1 2) -> @(+ 1 2)!}

'@foo{A @"string" escape}

'@foo{A @"{" begins a block}

'(define \@email
   "foo@bar.com")

;;spaces are not allowed before a [ or a { !
'@foo{bar @baz[2 3] {4 5}}
'@foo{bar @baz[2 3]{4 5}}

;`
'@(lambda (x) x){blah}
	
'@`(unquote foo){blah}

'@`(unquote foo)

'@foo{{{}}{}}
;;there are currently no special rules for using @ in the command itself, which can lead to things like:

  
'@@foo{bar}{baz}
  ;reads as  
;((foo "bar") "baz")

'@foo|{"}" follows "{"}|
	
'@foo{@"}" follows @"{"}