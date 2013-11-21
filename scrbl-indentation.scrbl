#lang scribble/base

@title{Indentation rules}

1) Tab adjusted

@itemlist[@item{item1}
          @item{item2}]

2) One-space adjusted

@centered{
 @bold{Cookies Wanted}
 @italic{Chocolate chip preferred!}
}

@;@a[ 
@; test
@; @b{c}
@; @d{e}
@;]

Line breaking rules:

1)Only handle string inside {} or pure string

2)Each paragraph shall not have more/less than given character number(e.g. 70). The last few "words"
will be passed to the next line, or some "words" from the next line will be pass back to current line
