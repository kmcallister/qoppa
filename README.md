This is a metacircular evaluator in two parts:

* `qoppa.scm` contains Scheme code to implement Qoppa, a tiny language of
  fexprs / operatives.

* `prelude.qop` is a Qoppa library which implements enough Scheme features
  to run `qoppa.scm` within the Qoppa evaluator.

[This blog post](http://mainisusuallyafunction.blogspot.com/2012/04/scheme-without-special-forms.html) has a detailed explanation.
