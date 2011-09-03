// Caterwaul nonlinear parser combinators | Spencer Tipping
// Licensed under the terms of the MIT source code license

// Introduction.
// This is a generalization of parser combinators onto nonlinear data structures such as trees or graphs (though it also works with strings). The idea is that a location within a data structure
// doesn't have to be scalar, though it generally would be when parsing text. In order to define a combinatory parser over a structure, you need to do a few things:

// | 1. Construct a memoization key. This is a function from the input position to a string. Not all parsers are memoized; returning null indicates that memoization shouldn't happen.
//   2. Provide parser combinators that return new states. Most of the higher-order combinators are general in that they will work with low-level combinators.
//   3. Provide a function that constructs the initial location(s) given an input. For strings, this is just f(s) = [0].
//   4. Provide a function that takes a given state and returns a list of forward adjacent states. For strings, this is f(i) = [i + 1].

// Because this parser combinator library implements a non-backtracking packrat parser, the usual nondeterminism caveats apply.

// Implementation specifics.
// Unlike some parser combinator implementations, this one provides a way to specify detailed failure information. This gives the user an idea about why and where a parse failed. Also unlike many
// Javascript parser combinator libraries, this one uses Caterwaul macros to make it easy to build up grammars. Each operator corresponds to a higher-order combinator provided by the Caterwaul
// parsing library.

// The definitions above require that the graph is directed, but it is allowed to be cyclic. Perhaps more interestingly, it can also be infinite provided that the parse rules have
// appropriately-defined termination conditions. This means that you could theoretically use these parser combinators to identify properties of continuous vector fields or other non-discrete
// directed structures.

// Nonlinear parsing.
// Let's suppose that we want to assign scopes to all local variables in a Caterwaul parse tree. This can be done by manually iterating over the tree, but it can also be set up as a parser that
// traverses the tree and looks for variables. Here's what those rules would look like, in pseudocode:

// | locals              = nondeterministic(many, not(is_function_node), local_binding)
//   local_binding       = nondeterministic(many, comma_node, local_or_assignment)
//   local_or_assignment = alternative(identifier, map(assignment, given.x in x[0]))

// Obviously the prefix notation here is cumbersome. Nonlinear parser combinators are generally denoted differently, using operators and/or regular expressions.

//   Linear paths.
//   Let's assume that we're parsing over a strictly hierarchical tree and that we start at the root (mainly for ease of thought; I think this example is fully general to all DAGs with exactly
//   one in-degree-zero node). We want to parse from the top to the bottom, accumulating all paths that are parseable as arithmetic expressions. So, for example, we might have a tree that looks
//   like this:

//   |         (3)
//            /   \
//          (+)   (4)
//          /     / \
//        (4)   (+) (6)
//       /   \
//     (*)   (5)
//      |     |
//     (4)   (+)

//   Since we're working with a tree, we know that the 'forward' function returns an array of a node's children. So each parse step is really a list of alternatives. Parsing the 'next character'
//   or 'next X' amounts to flat-mapping across forward motions. This will probably make more sense with an example:

//   | expression = term '+' expression | term
//     term       = number '*' term | number

//   Starting with 'expression', we parse this way, starting with the roots (in this case, only one):

//   | expression(root) = expression( (3) ) -> term( (3) ) -> number( (3) ) -> 3           <- at this point, we've consumed (3) and should move to the next inputs.
//                          '*'( (+) ) | '+'( (+) ) -> +                                   <- use disjunction to ascend to matching terminal; advance to next inputs.
//                            expression( (4) ) -> term( (4) ) -> number( (4) ) -> 4       <- right-recursion into expression(), advance.
//                              '*'( (*) ) -> *                                            <- matching within the term() expansion
//                                term( (4) ) -> number( (4) ) -> 4                        <- second term() within the term() above
//                              '*'( (5) ) | '+'( (5) ) -> fail                            <- no match here; reject this subtree
//                          '*'( (4) ) | '+'( (4) ) -> fail                                <- no match here; reject this subtree

//   At the end we have an array of the only surviving alternative, (3 + (4 * 4)). (Presumably the parser combinators are configured to construct parse trees.) This is basically a linear parse
//   that removed alternatives as it went; the result was an array of valid parse trees.
// Generated by SDoc 
