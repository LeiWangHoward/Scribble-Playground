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

If you give a mouse a cookie, he's going to ask for a
glass of milk.

He's a @smaller{small mouse}. The glass is too @larger{big}---@bold{way @larger{too @larger{big}}}. So, he'll @italic{probably} ask you for a straw.

If a mouse eats all your cookies, put up a sign that says
@centered{
 @bold{Cookies Wanted}
 @italic{Chocolate chip preferred!}
}
and see if anyone brings you more.

@section{Notice to Mouse}
@centered{@bold{Notice to Mice}}

@itemlist[@item{We have cookies for you.}
          @item{If you want to eat a cookie,
                 you must bring your own straw.}]

@tabular[#:sep @hspace[1]
               (list (list @bold{Animal} @bold{Food})
                     (list "mouse"       "cookie")
                     (list "moose"       "muffin"))]

@section{The Consequences of Milk}

That ``squeak'' was the mouse asking for milk. Let's
suppose that you give him some in a big glass.

He's a small mouse. The glass is too big---way too big
but now it is too small. So, he'll probably ask you for
a straw. You might as well give it to him.

@section{Not the Last Straw}

For now, to handle the milk moustache, it's enough to give
him a napkin. But it doesn't end there... oh, no.


@(require slideshow/pict)

This cookie has lost its chocolate chips:
@(colorize (filled-ellipse 40 40) "beige").
