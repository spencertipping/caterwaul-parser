// Caterwaul nonlinear parser combinators | Spencer Tipping
// Licensed under the terms of the MIT source code license

// Introduction.
// This is a generalization of parser combinators onto nonlinear data structures such as trees or graphs (though it also works with strings). The idea is that a location within a data structure
// doesn't have to be scalar, though it generally would be when parsing text. In order to define a combinatory parser over a structure, you need to do a few things:

// | 1. Construct a memoization key. This is a function from the input position to a string. Not all parsers are memoized; returning null indicates that memoization shouldn't happen.
//   2. Provide parser combinators that return new states. Most of the higher-order combinators are general in that they will work with low-level combinators.
//   3. Provide a function that takes a distance and a value, and returns a list of forward states of that distance that have that value.

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

  $.parser = capture [

  // Traversal combinators.
//   Linear parser combinator libraries generally implement a 'seq' or 'join' combinator that causes one parser to be activated and then followed by another one. Because there is only one path to
//   follow, there isn't a distinction between breadth-first and depth-first searching. When you have multiple paths, though, the ordering becomes important. It may be the case that a path never
//   ends; in this case breadth-first with eager termination is more useful.

  // Taken outside of the traditional parsing context, traversal combinators can be seen as search strategies. Some searches explore all alternatives simultaneously while others optimistically
//   search for a single solution and assume that none will devolve into infinite recursion. Some return all solutions, others abandon further searching after a single solution is found. As such,
//   this parser library implements several different join combinators that embody these different behaviors.

  // Different combinators do different things with the parse values. As a convention, combinators ending with 's' are searches that just return the final values. Combinators ending in 'c' are
//   collectors that return all of the intermediate values as arrays.

  // The construction of bfc() is a bit gnarly. Here's what's going on. We start off with an array of states, and we need to, in a breadth-first manner, follow each state and collect the
//   intermediate values. The simplest way to do this is to create a state matrix, where rows are parse paths and columns are steps within each path. So we'd have something like this:

  // | states[0]  ps[0](states[0])  ps[1](ps[0](states[0]))  ...
//     states[1]  ps[0](states[1])  ps[1](ps[0](states[1]))  ...
//     states[2]  ps[0](states[2])  ps[1](ps[0](states[2]))  ...
//     ...

  // The only trouble is that each parser might have multiple or no return states. We solve this by duplicating or removing whichever origin states are necessary to keep the matrix rectangular
//   and dense. For instance, suppose that ps[0](states[1]) produced two values. Then we'd have this:

  // | states[0]  ps[0](states[0])     ps[1](ps[0](states[0]))     ...
//     states[1]  ps[0](states[1])[0]  ps[1](ps[0](states[1])[0])  ...
//     states[1]  ps[0](states[1])[1]  ps[1](ps[0](states[1])[1])  ...
//     states[2]  ps[0](states[2])     ps[1](ps[0](states[2]))     ...
//     ...

  // I'm modeling this by representing each row as an array and having the arrays grow rightward as more parsers are used. I was tempted to statefully update the initial arrays, but this is
//   tricky given that we're potentially cloning them on every step. This logic is captured by step_matrix_mutable(), which optimizes linear cases. (step_matrix_immutable doesn't employ this
//   optimization, which may be safer if you want to preserve intermediate matrices.)

    bfs(ps = parsers('bfs', arguments), annotate(result, 'bfs', ps))(states) = ps /[states][x /-memo/ x0] -seq,
    bfc(ps = parsers('bfc', arguments), annotate(result, 'bfc', ps))(states) = ps /[states /!$.parser.state_matrix][step(x)(x0)] /seq /!$.parser.row_composite_states_from
                                                                               -where [step = $.parser.step_matrix_mutable],

    state_matrix(states)         = states *[[x]] -seq,
    step_matrix_mutable(p)(m)    = m *~!r[xs.length === 1 ? r.push(xs[0]) && [r] : xs *~[r + [x]] -seq, where [xs = p /-memo_single/ r[r.length - 1]]] -seq,
    step_matrix_immutable(p)(m)  = m *~!~r[memo_single(p, r[r.length - 1]) *~[r + [x]]] -seq,
    row_composite_states_from(m) = m *r[r[r.length - 1].map("r.slice(1, r.length) *[x.value()] -seq".qf)] -seq,

  // Choice combinators.
//   Nonlinearity provides choice among inputs, but we still need combinators to choose grammar productions. There are two such combinators provided by this library. One, alt(), returns the first
//   possibility that has states. This is useful in linear parsing contexts where a full search is not needed. The other, all(), accumulates every possibility of every sub-parser. This is useful
//   when it's necessary to search an entire structure.

  // Put differently, alt() introduces a cut into the search, whereas all() does not.

    alt(ps = parsers('alt', arguments), annotate(result, 'alt', ps))(states) = states *~!state[ps   |[x /-memo_single/ state -re [it.length && it]] |seq || []] -seq,
    all(ps = parsers('all', arguments), annotate(result, 'all', ps))(states) = states *~!state[ps *~![x /-memo_single/ state]] -seq,

  // Repetition combinators.
//   Because there are multiple types of joining, repetition is not as simple as it is for a linear parser. However, repetition can be expressed as recursion and a join:

  // | repeat(parser) = R -> parser R | parser

  // This library's repetition combinator takes two parameters. One is the parser to be repeated, and the other is the join combinator that is used to connect it to the recursive step. Note that
//   the output of many() is a right-folded set of binary joins. The funky f(states) = f(states) statement just sets up a temporary function that will proxy to the real 'f' when we redefine it.
//   This way we have access to 'f' both before and after it exists (and it will do the same thing in either case).

  // If you want to collect an array of many things, you're better off using the 'manyc' combinator -- this returns a flat array rather than a folded one, and knows to use breadth-first with
//   collection. It's probably also more efficient than using many() if you want all of the intermediate results, especially if the parser behaves linearly and matches many times.

  // Repeating a parser isn't quite as simple as breadth-first collection. The reason is that some states' paths might terminate before others' do. Going back to the matrix model above, this
//   means that some rows have fewer columns than others. In order to deal with this in a breadth-first way, we need to keep track of which states have terminated and stop iterating those while
//   simultaneously flat-mapping others. I'm doing this by appending a null entry to terminated arrays. The iteration is done when all rows end with null.

    manyc(p, annotate(result, 'manyc', [p]))(states)   = $.parser.state_matrix(states) /~!step /seq /!$.parser.row_null_states_from
                                                         -where [iterate = $.parser.step_matrix_mutable_null(p), step(m) = $.parser.has_non_null_states(m) ? iterate(m) : null],

    many(p, join, annotate(result, 'many', [p, join])) = f -where [j = join || $.parser.bfs, f(states) = f(states), f = p /-j/ annotate(f, 'recursive', []) /-$.parser.alt/ p],

    optional(p, annotate(result, 'optional', [p]))     = p /-$.parser.alt/ $.parser.zero(),

    step_matrix_immutable_null(p)(m) = m *~!r[xs ? xs.length ? xs *~[r + [x]] -seq : [r + [null] -seq] : [r], where [xs = r[r.length - 1] -re [it && p /-memo_single/ it]]] -seq,
    step_matrix_mutable_null(p)(m)   = m *~!r[xs ? l ? l === 1 ? r.push(xs[0]) && [r] : xs *~[r + [x]] -seq : r.push(null) && [r] : [r],
                                              where [xs = r[r.length - 1] -re [it && p /-memo_single/ it], l = xs && xs.length]] -seq,

    has_non_null_states(m)           = m |r[r[r.length - 1]] |seq,
    row_null_states_from(ms)         = ms[ms.length - 1] *r[r[r.length - 2].map("r.slice(1, r.length - 1) *[x.value()] -seq".qf)] /seq,

  // Trivial combinators.
//   Most combinator libraries are modeled to have separate zero-or-more, one-or-more, and zero-or-one functions. This one is different in that it provides a universal zero combinator that
//   consumes nothing and does nothing. You can use it with alternatives to form optional rules. Similarly uninteresting is the fail combinator, which always rejects its input.

    zero(annotate(result, 'zero', []))(states) = states,
    fail(annotate(result, 'fail', []))(states) = [],

  // Zero-length combinators.
//   These don't impact the parse state in any way, but they can cause a parse to fail by rejecting certain branches. They are more commonly known as lookahead combinators.

    match(p,  annotate(result, 'match',  [p]))(states) = states % [memo_single(p, x).length] -seq,
    reject(p, annotate(result, 'reject', [p]))(states) = states %![memo_single(p, x).length] -seq,

  // Pluralization combinator.
//   This is used to adapt linear terminal combinators to be used in a nonlinear context. It assumes that the linear combinator maps a truthy parse state into either another truthy parse state or
//   a null/falsy value.

    pluralize(p, annotate(result, 'pluralize', [p]))(states) = states %~!p -seq,

  // Mapping combinator.
//   This lets you remain in combinator-space (as opposed to state-space) while mapping over values. There are two such mapping combinators; one is a flat-map and the other is a componentwise
//   map. Variants exist in case you want access to the state in its entirety.

    map(p, f,            annotate(result, 'map',            [p, f]))(states) = p(states) *[x.map(f)] -seq,
    flat_map(p, f,       annotate(result, 'flat_map',       [p, f]))(states) = p(states) *~!~[f(x.value()) *y[x.map(delay in y)]] -seq,

    map_state(p, f,      annotate(result, 'map_state',      [p, f]))(states) = p(states) *  [f(x)] -seq,
    flat_map_state(p, f, annotate(result, 'flat_map_state', [p, f]))(states) = p(states) *~![f(x)] -seq],

// Data type drivers.
// This is where we tie the parsers to actual data types. Each data type driver should provide these methods:

// | 1. id()               returns an optional memoization key, which is a string -- should be unique for each input position
//   2. input()            returns the input structure, which the combinators must know how to use
//   3. position()         returns the current position within the input structure, which the combinators must know how to use
//   4. value()            returns the current result value
//   5. next(n, v)         returns an array containing forward steps, each of which has the value 'v'
//   6. map(f(x))          returns an identical state whose result value is mapped through f
//   7. memo_table()       returns a consistent reference to the memo table for this parse (the table is just a regular object)

  // Driver generator.
//   Most driver functions are repetitive enough that it's worth factoring out the common logic. This function takes a few parameters and returns a constructor. The 'options' parameter should be
//   a hash that looks like this:

  // | {step:     function (position, value) -> [state],
//      id:       function () -> number
//      defaults: {position: X, value: Y}}

  // The only mandatory option is 'step', since this has no sensible default. Be sure to remember that step() needs to produce an array! It will cause all kinds of problems if you return a state
//   that isn't encapsulated in an array.

    $.parser.logical_state(options) = ctor -se- it.prototype /-$.merge/ methods_for(options.step)
      -where [defaults                  = options.defaults || {},

              default_position          = defaults.position,
              default_value             = defaults.value,
              id_function               = defaults.id || "++memo_id".qf,

              ctor(i, p, v, memo_table) = this -se [it.i = i, it.p = p || default_position, it.v = v || default_value, it.table = memo_table || {}],

              methods_for(step)         = capture [id()           = this.cached_id || (this.cached_id = id_function.call(this)),
                                                   input()        = this.i,  next(n, v)   = n === 1 ? step.call(this, this.p, v) : this.next(n - 1, v) *~![x.next(1, v)] -seq,
                                                   position()     = this.p,  map(f)       = new this.constructor(this.i, this.p, this.v /!f, this.table),
                                                   value()        = this.v,  memo_table() = this.table,
                                                   toString()     = '#{this.i} @ #{this.p} : #{this.v}',
                                                   change(values) = new this.constructor(values.input || this.i, values.position || this.p, values.value || this.v, this.table)]],

  // String driver.
//   This is a probably-linear parser. I say probably because it's simple enough to implement a subclass of it that jumps around within the string. However, we don't assume that initially; for
//   our purposes we just define a linear, forward string traversal pattern.

    $.parser.linear_string_state = capture [step(p, v) = [this.change({position: p + 1, value: v})],
                                            id()       = this.position(),
                                            defaults   = {position: 0, value: null}] /!$.parser.logical_state /-$.merge/

                                   capture [end(annotate(result, 'end', []))(states) = states %[x.position() === x.input().length] -seq],

  // String combinators.
//   Whether you're using linear or nonlinear parsing, you'll probably want some terminal string combinators to work with. These are all regexp-based, hence the dependency on Caterwaul's regexp
//   parsing extension. Note that this parser is plural, not singular; you won't need to use the pluralize() function with it.

  // Note that regexp() works only in a sequential linear context. If you're doing things like jumping around a string within a single parse step, then you'll need to precompute the jumps by
//   generating a new string and then parsing against that. (I'm doing it this way for performance in the most common case.) Alternatively, you can write a new regexp() parser combinator that is
//   aware of jumping.

  // Regular expression matching has worst-case O(n log n) time complexity, where n is the match length. This is done by bisecting the match region until we identify the longest possible match.
//   It's possible to do this because we know up-front the minimum match length; Caterwaul's regexp library provides this. We then double this until the match fails or we run off the end of the
//   string. Then we bisect between the minimum and the failure length until we find the point at which the match fails.

    $.merge($.parser, capture [anchor_regexp(r) = new RegExp('^#{body}$', flags) -where [pieces = /^\/(.*)\/(\w*)$/.exec(r.toString()), body = pieces[1], flags = pieces[2]],

                               linear_string(s, annotate(result, 'linear_string', [s]))(states) = states *~![x.input().substr(x.position(), s.length) === s ? x.next(s.length, s) : []] -seq,
                               linear_regexp(r, annotate(result, 'linear_regexp', [r]))         = matcher
                                                                                          -where [minimum_length   = $.regexp(r).minimum_length() || 1,
                                                                                                  anchored         = r /!$.parser.anchor_regexp,
                                                                                                  matcher(states)  = states *~!match_one -seq,
                                                                                                  match_one(state) = new_states
                                                                                                             -where [s              = state.input(),
                                                                                                                     offset         = state.position(),
                                                                                                                     maximum_length = s.length - offset,

                                                                                                                     match(l)       = l <= maximum_length && anchored.test(s.substr(offset, l)),
                                                                                                                     longest(l)     = l /!match ? longest(l << 1) : l,
                                                                                                                     valid(l, m, u) = l < u - 1 ? m /!match ? valid(m, m + u >> 1, u) :
                                                                                                                                                              valid(l, l + m >> 1, m) : m,

                                                                                                                     new_states     = minimum_length /!match ?
                                                                                                                                        state.next(match_length,
                                                                                                                                                   anchored.exec(s.substr(offset, match_length)))
                                                                                                                                        -where [max          = minimum_length /!longest,
                                                                                                                                                match_length = valid(minimum_length,
                                                                                                                                                                     minimum_length + max >> 1,
                                                                                                                                                                     max)] :
                                                                                                                                        []]]]),

  // Structure driver.
//   This is used when you have a set of objects and/or arrays. The idea is to traverse the structure from the top down in some way, optionally collecting path-related information. Atoms, then,
//   are the keys that dereference elements in the structure.

    $.parser.structure_state = capture [step(p, v) = this.input() /pairs *[this.change({input: x[1], position: x[0]})] -seq] /!$.parser.logical_state,

  // Array-like driver.
//   This is used when you know that you've got objects that will support array-like traversal patterns. Caterwaul syntax trees fall into this category. This is distinct from the structure driver
//   above because it doesn't iterate through properties, just from 0 to the last element as determined by the 'length' property.

    $.parser.array_state     = capture [step(p, v) = this.input() *[this.change({input: x, position: xi})] -seq] /!$.parser.logical_state,

  // Structure combinators.
//   Unlike string combinators, some of these are based on position and others are based on value predicates. This is due to the common use case for structural parsing: we want to traverse some
//   structure and manipulate values based on some property of their paths. Terminal combinators, then, accept or reject paths based on their current position. What we actually need is a
//   higher-order combinator that maps the current state's position into value-space. More generally, we need a proxy for a state that can map any aspect of that state into its value space.

    $.merge(($.parser.proxy_state(s, value_function) = this -se [it.state = s, it.value_function = value_function]).prototype,

    capture [id()       = this.cached_id || (this.cached_id = ++memo_id),
             input()    = this.value_function.call(this),  next(n, v)   = this.state.next(n, v),
             position() = this.state.position(),           map(f)       = this.state.map(f),
             value()    = this.state.value(),              memo_table() = this.state.memo_table()]),

    $.parser /-$.merge/ capture [position_state(s)   = new $.parser.proxy_state(s, "this.position()".qf),
                                 position(p)(states) = p(states *$.parser.position_state -seq)],

// Memoization.
// This happens at the combinator level. Each combinator generated by the parser is assigned a unique identifier (this happens automatically), and that identifier is then used to track the
// memoization partition.

  where [memoization_key       = $.gensym('memo'),
         memo_id               = 0,

         memo_single(f, state) = value -where [f_key = f[memoization_key] || (f[memoization_key] = ++memo_id),
                                               s_key = state.id(),
                                               table = state.memo_table(),
                                               key   = '@#{s_key}_#{f_key}',            // Prefix with @ to eliminate the possibility of collisions with other properties
                                               value = f_key && s_key && table.hasOwnProperty(key) ? table[key] : (table[key] = f([state]))],

         memo(f, states)       = states *~![f /-memo_single/ x] -seq,

// Argument conversion.
// These functions are used both to verify incoming arguments and to annotate results. Caterwaul parser combinators are marked with the 'caterwaul_parser' attribute; this indicates that the
// function can be used with other Caterwaul parsers.

         parsers(name, xs)     = xs *! [x                  || raise [new Error('#{name}: undefined parser given as parameter #{xi}')]]
                                    *~![x instanceof Array ? x : [x]]
                                    *! [x.caterwaul_parser || raise [new Error('#{name}: #{x} is not marked with the .caterwaul_parser attribute')]] -seq,

         annotate(f, name, xs) = f -se [it.toString()       = '#{name}(#{xs *[x.toString()] -seq -re- it.join(", ")})',
                                        it.caterwaul_parser = true]]})(caterwaul);

// Generated by SDoc 
