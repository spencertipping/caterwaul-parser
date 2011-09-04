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

// Linear paths.
// Let's assume that we're parsing over a strictly hierarchical tree and that we start at the root (mainly for ease of thought; I think this example is fully general to all DAGs with exactly one
// in-degree-zero node). We want to parse from the top to the bottom, accumulating all paths that are parseable as arithmetic expressions. So, for example, we might have a tree that looks like
// this:

// |         (3)
//          /   \
//        (+)   (4)
//        /     / \
//      (4)   (+) (6)
//     /   \
//   (*)   (5)
//    |     |
//   (4)   (+)

// Since we're working with a tree, we know that the 'forward' function returns an array of a node's children. So each parse step is really a list of alternatives. Parsing the 'next character' or
// 'next X' amounts to flat-mapping across forward motions. This will probably make more sense with an example:

// | expression = term '+' expression | term
//   term       = number '*' term | number

// Starting with 'expression', we parse this way, starting with the roots (in this case, only one):

// | expression(root) = expression( (3) ) -> term( (3) ) -> number( (3) ) -> 3           <- at this point, we've consumed (3) and should move to the next inputs.
//                        '*'( (+) ) | '+'( (+) ) -> +                                   <- use disjunction to ascend to matching terminal; advance to next inputs.
//                          expression( (4) ) -> term( (4) ) -> number( (4) ) -> 4       <- right-recursion into expression(), advance.
//                            '*'( (*) ) -> *                                            <- matching within the term() expansion
//                              term( (4) ) -> number( (4) ) -> 4                        <- second term() within the term() above
//                            '*'( (5) ) | '+'( (5) ) -> fail                            <- no match here; reject this subtree
//                        '*'( (4) ) | '+'( (4) ) -> fail                                <- no match here; reject this subtree

// At the end we have an array of the only surviving alternative, (3 + (4 * 4)). (Presumably the parser combinators are configured to construct parse trees.) This is basically a linear parse that
// removed alternatives as it went; the result was an array of valid parse trees.

// Leveraging nonlinearity.
// Nonlinearity comes in handy when you're asking about properties of highly structured data. For example, maybe we want to know whether a Caterwaul syntax tree contains a console.log() statement
// that follows an assignment to a variable. However, those statements need to occur in the same control flow branch; we can't have them on opposite sides of a conditional. (If we did, then
// console.log() wouldn't really follow the assignment.) To do this, we first need to define a traversal pattern that follows the evaluation order. This differs from a breadth-first traversal
// because it needs to become nondeterministic when we hit a decision but be linear when we hit a side-effect like a semicolon.

// We then write this rule to find what we're looking for:

// | log = any* '_variable = _v1' non_decision* 'console.log(_v2)'

// If we run this with nonlinear parsing, we'll get a list of all code paths that end up assigning a variable and then definitely call console.log (barring exceptions). As a nice perk, we'll also
// get the side-effects leading up to those code paths as well as the statements that happened in between the assignment and the log statement.

caterwaul.js_all()(function ($) {

// Implementation specifics.
// Unlike some parser combinator implementations, this one provides a way to specify detailed failure information. This gives the user an idea about why and where a parse failed. Also unlike many
// Javascript parser combinator libraries, this one uses Caterwaul macros to make it easy to build up grammars. Each operator corresponds to a higher-order combinator provided by the Caterwaul
// parsing library.

// The definitions in the introduction require that the graph is directed, but it is allowed to be cyclic. Perhaps more interestingly, it can also be infinite provided that the parse rules have
// appropriately-defined termination conditions. This means that you could theoretically use these parser combinators to identify properties of continuous vector fields or other non-discrete
// directed structures.

  $.parser = {},

  $.merge($.parser, capture [

//   Traversal combinators.
//   Linear parser combinator libraries generally implement a 'seq' or 'join' combinator that causes one parser to be activated and then followed by another one. Because there is only one path to
//   follow, there isn't a distinction between breadth-first and depth-first searching. When you have multiple paths, though, the ordering becomes important. It may be the case that a path never
//   ends; in this case breadth-first with eager termination is more useful.

//   Taken outside of the traditional parsing context, traversal combinators can be seen as search strategies. Some searches explore all alternatives simultaneously while others optimistically
//   search for a single solution and assume that none will devolve into infinite recursion. Some return all solutions, others abandon further searching after a single solution is found. As such,
//   this parser library implements several different join combinators that embody these different behaviors.

    bfs(ps = arguments)(states) = ps /[states][x(x0)] -seq,

//   Choice combinators.
//   Nonlinearity provides choice among inputs, but we still need combinators to choose grammar productions. There isn't anything particularly remarkable about this library's implementation of
//   choice combinators; they are logically identical to the ones used in linear parsing expression grammars.

    alt(ps = arguments)(states) = states *~!state[ps |[x(state) -re [it.length && it]] |seq || []] -seq,

//   Repetition combinators.
//   Because there are multiple types of joining, repetition is not as simple as it is for a linear parser. However, repetition can be expressed as recursion and a join:

//   | repeat(parser) = R -> parser R | parser

//   This library's repetition combinator takes two parameters. One is the parser to be repeated, and the other is the join combinator that is used to connect it to the recursive step.

    many(p, join) = f -where [f(states) = alt(join(p, f), p)],

//   Trivial combinators.
//   Most combinator libraries are modeled to have separate zero-or-more, one-or-more, and zero-or-one functions. This one is different in that it provides a universal zero combinator that
//   consumes nothing and does nothing. You can use it with alternatives to form optional rules. Similarly uninteresting is the fail combinator, which always rejects its input.

    zero(states) = states,
    fail(states) = [],

//   Zero-length combinators.
//   These don't impact the parse state in any way, but they can cause a parse to fail by rejecting certain branches. They are more commonly known as lookahead combinators.

    match(p)(states)  = states % [p([x]).length] -seq,
    reject(p)(states) = states %![p([x]).length] -seq,

//   Pluralization combinator.
//   This is used to adapt linear terminal combinators to be used in a nonlinear context. It assumes that the linear combinator maps a truthy parse state into either another truthy parse state or
//   a null value.

    pluralize(p)(states) = states %~!p -seq]),

// Data type drivers.
// This is where we tie the parsers to actual data types. Each data type driver should provide these methods:

// | 1. id()               returns an optional memoization key, which is a string
//   2. next()             returns an array containing forward steps
//   3. map(f(x))          returns an identical state whose result value is mapped through f



// Generated by SDoc 
