= Substructural Operational Semantics as Ordered Logic Programming =
== By Frank Pfenning and Robert J. Simmons, published in LICS 2009 ==

This directory contains examples from the paper ''Substructural Operational
Semantics as Ordered Logic Programming'' by Frank Pfenning and Robert
J. Simmons, available [[http://www.cs.cmu.edu/~fp/papers/lics09.pdf|here]].

== Figures from paper ==

Each of the figures from the paper are presented, along with illustrative 
examples.

* Figure 3:  [[cbv.olf        | Call-by-value functions]]
* Figure 4:  [[mutable.olf    | Mutable storage]]
* Figure 5:  [[pairs.olf      | Parallel evaluation for pairs]] 
* Figure 6:  [[asynch.lolf    | Asynchronous communication]]
* Figure 7:  [[cbn.olf        | Call-by-name functions]]
* Figure 8:  [[cbn-dest.olf   | Call-by-name with destinations for binding]]
* Figure 9:  [[cbneed.lolf    | Call-by-need functions]]
* Figure 10: [[exceptions.olf | Exceptions]]

== Other examples ==

The discussion of exceptions includes a comment that 
''"Exceptions entail a certain violation of modularity if
the language specification requires latent propositions
waiting on more than one result, as, for example, in
parallel evaluation of pairs."'' This problem is illustrated in
[[par_exn1.olf]], and one possible solution is discussed in
[[par_exn2.olf]].


