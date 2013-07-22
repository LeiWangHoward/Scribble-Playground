#lang racket
(require framework)
(define txt_1 (new racket:text%))
(send txt_1 insert " lang scribble/base\n@f{x}")
(send txt_1 backward-containing-sexp 22 (send txt_1 last-position))