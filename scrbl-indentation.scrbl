#lang scribble/doc
@(require scribble/manual
          scribble/bnf
          (for-label scriblib/figure))
@(define (sample . text) (nested #:style 'inset (apply verbatim text)))
@(define (result . text) (apply nested #:style 'inset text))

@title[#:tag "interface-essentials" #:style 'toc]{1. Scribble Indentation}

This document has two sections: scribble indentation rules
and scribble indentation functions.

@section{Scribble Indentation Rules}

The scribble indentation rules are created based on the
judgement of @italic{parent parenthesis}. It will perform
the indentation on each line of code based on two facts:
check @italic{parent parenthesis} and counting
@italic{number of parenthesis} till the outmost @elem["@"]
notation that the current line is in. Then the program will
put spaces before each line of code based on how many 
@italic{parent parenthesis} outside and the following 
@italic{parenthesis judgement rules}

@itemize[
         
 @item{Tab adjusted: exists items at the same line after @italic{[}
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

@section{Line breaking rules}

@itemize[
         
 @item{Only handle text or text inside @italic{@"{" @"}"} }

 @item{Each line of code's length cannot exceed the given
  paragraph width limit(e.g. 70 characters per line). When
  exceed the limitation, the last few @italic{words}
  seperated by @italic{#\space} will be passed to the next
  line of code}

 ]

@section{Indentation functions}

@itemize[
         
 @item{
  (adjust-para-width a-racket:text position width) → boolean?
  @linebreak{}
  position : exact-integer? = current position 
  @linebreak{}
  width : exact-integer? = predefined value
  @linebreak{}
  Modify the given paragraph(line) if it exceed width limit
  by inserting @italic{#\newline} to proper position
  }

 @item{
  (determine-spaces a-racket:text position) →
  exact-integer?/boolean?
  @linebreak{}
  position : exact-integer? = current position
  @linebreak{}
  Based on indentation rules, returns exact integer
  represents number of @italic{#\space} to put in front of
  current paragraph(line) or #f
  }
 @item{
  (paragraph-indentation a-racket:text posi width) → void?
  @linebreak{}
  posi : exact-integer? = current given position
  @linebreak{}
  width : exact-integer? = user defined line width
  limitation
  @linebreak{}
  Indent a whole paragraph(multiple lines) that contains
  the given position
  }
 ]