#lang scribble/doc
@(require scribble/manual
          scribble/bnf
          (for-label scriblib/figure))
@(define (sample . text) (nested #:style 'inset (apply verbatim text)))
@(define (result . text) (apply nested #:style 'inset text))

@title[#:tag "interface-essentials" #:style 'toc]{1. Scribble Indentation}

This document has two sections: scribble indentation rules
and scribble indentation functions.

@section{1.1 Scribble Indentation Rules}

The scribble indentation rules are created based on the
judgement of @italic{parent parenthesis}. It will perform
the indentation on each line of code based on two facts:
check @italic{parent parenthesis} and counting number of
parenthesis till the outmost @elem["@"] notation that the
current line is in. Then the program will put spaces before
each line of code based on how many @italic{parent parenthesis}
outside and following rules.

@section{1.1.1 Parenthesis judgement rules} 

@itemize[
         
 @item{Tab adjusted:
  @sample|{
   @itemlist[@item{item1}
             @item{item2}]}|
  }
               
 @item{One-space adjusted:
  @sample|{
   @centered{
    @bold{blah}
    @italic{blah}
    }
   }|
                                   
  @sample|{
   @itemlist[ 
    @item{blah}
    @item{blah}]
   }|
  }

 ]

@section{1.2.1 Line breaking rules}
                                    
1)Only handle string inside {} or pure string

2)Each paragraph shall not have more/less than given
character number(e.g. 70). The last few "words" will be
passed to the next line, or some "words" from the next line
will be pass back to current line
