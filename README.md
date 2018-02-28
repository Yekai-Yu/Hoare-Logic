# Hoare Logic Incremental Test
Component of Web Assignment autograder

There are five rules: __Assignment Rule, Pre-condition Strengthening Rule, Post-condition Weakening Rule, Sequencing Rule, IfThenElse Rule, While Rule__.

* Assignment Rule has a __meta-level__ side condition check, which is invoking a substitution function to mathematically apply the assignment command to the postcondition, checking whether the result of substitution is same with the precondition or not.

* Pre-condition Strengthening Rule and Post-condition Weakening Rule have __object-level__ side condition checks, which is checking whether the implication expressions within the side condition fields match their corresponding condition fields or not. E.g., we have a side condition of Pre-condition Strengthening Rule, sc:{1+2=3 -> true}. We will have ___1+2=3___ in the bottom level of the tree as prescondition, ___true___ in the upper level of the tree as precondition strengthening condition, then the checker will vaildate the fields in _sc_ and the corresponding fields in the tree.

__Lexer, parser__
* __Transition Semantics__ is employed and __Simple Imperative Programming Language (SIMP)__ is applied.
* Parser mainly has three categories of expression, __bool_expression, exp_expression, com_expression__.



## List of files:
- _hlcommon.ml_ contains utility definitions and functions
- _hllex.mll_ - lexer
- _hlparse.mly_ - parser
- _hlprocessing.ml_ accepts string input to get string parsed (invoking parser).
- _hlcheck.ml_ - Rules are defined, side condition checkers are included.
