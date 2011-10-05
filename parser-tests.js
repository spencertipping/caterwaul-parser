// Introduction.
// These tests might actually fail, as opposed to the REPL tests which are just for show.

caterwaul.js_all()(function ($) {

// Driver tests.
// It isn't much use testing the parser stuff until we know that the drivers are solid. Run each driver through a series of operations to make sure it behaves in a reasonable way.

  // String driver.
//   This one is pretty simple. It should advance through the string.

  initial.input()                   /-eq/ s         -se-
  initial.position()                /-eq/ 0         -se-
  initial.value()                   /-eq/ undefined -se-
  initial.next(1, 42).length        /-eq/ 1         -se-
  initial.next(1, 42)[0].input()    /-eq/ s         -se-
  initial.next(1, 42)[0].position() /-eq/ 1         -se-
  initial.next(1, 42)[0].value()    /-eq/ 42        -where [s = 'foo', initial = new linear_string_state(s)],

  // Structure driver.
//   This one should dig through a structure, ultimately traversing all children. It should work for both arrays and objects.

  initial.input()                   /-eq/ root        -se-
  initial.position()                /-eq/ undefined   -se-
  initial.value()                   /-eq/ undefined   -se-
  initial.next(1, 42).length        /-eq/ 2           -se-
  initial.next(1, 42)[0].input()    /-eq/ first_child -se-
  initial.next(1, 42)[0].position() /-eq/ 'c1'        -se-
  initial.next(1, 42)[0].value()    /-eq/ 42          -se-
  initial.next(1, 43)[1].input()    /-eq/ other_child -se-
  initial.next(1, 43)[1].position() /-eq/ 'c2'        -se-
  initial.next(1, 43)[1].value()    /-eq/ 43          -where [first_child = {}, other_child = {}, root = {c1: first_child, c2: other_child}, initial = new structure_state(root)],

  // Array driver.
//   This one ignores non-array-like properties and just goes through the numbered ones. I'm going to use it to test Caterwaul syntax trees.

  initial.input()                   /-eq/ tree       -se-
  initial.position()                /-eq/ undefined  -se-
  initial.value()                   /-eq/ undefined  -se-
  initial.next(1, 42).constructor   /-eq/ Array      -se-
  initial.next(1, 42).length        /-eq/ 2          -se-
  initial.next(1, 42)[0].input()    /-eq/ left_node  -se-
  initial.next(1, 42)[0].position() /-eq/ 0          -se-
  initial.next(1, 42)[0].value()    /-eq/ 42         -se-
  initial.next(1, 43)[1].input()    /-eq/ right_node -se-
  initial.next(1, 43)[1].position() /-eq/ 1          -se-
  initial.next(1, 43)[1].value()    /-eq/ 43         -where [tree = '3 + 4 * 5'.qs, left_node = tree[0], right_node = tree[1], initial = new array_state(tree)],

// Traversal tests.
// These make sure that the various traversal combinators do what they're supposed to. This is subtle because a part of the behavior has to do with how eagerly they evaluate things.

  // Breadth-first search combinators.
//   This code tests bfs() and bfc(), which combine parsers in a breadth-first way (see the parser source documentation for specifics).

  any2([initial]).length        /-eq/ 2     -se-
  any2([initial])[0].position() /-eq/ 0     -se-
  any2([initial])[0].input()    /-eq/ left  -se-
  any2([initial])[0].value()    /-eq/ left  -se-
  any2([initial])[1].position() /-eq/ 1     -se-
  any2([initial])[1].input()    /-eq/ right -se-
  any2([initial])[1].value()    /-eq/ right -se-

  any3([initial]).length        /-eq/ 2        -se-
  any3([initial])[0].position() /-eq/ 0        -se-
  any3([initial])[0].input()    /-eq/ right[0] -se-
  any3([initial])[0].value()    /-eq/ right[0] -se-
  any3([initial])[1].position() /-eq/ 1        -se-
  any3([initial])[1].input()    /-eq/ right[1] -se-
  any3([initial])[1].value()    /-eq/ right[1] -se-

  any2c([initial]).length            /-eq/ 2     -se-
  any2c([initial])[0].position()     /-eq/ 0     -se-
  any2c([initial])[0].input()        /-eq/ left  -se-
  any2c([initial])[0].value().length /-eq/ 2     -se-
  any2c([initial])[0].value()[0]     /-eq/ tree  -se-
  any2c([initial])[0].value()[1]     /-eq/ left  -se-
  any2c([initial])[1].position()     /-eq/ 1     -se-
  any2c([initial])[1].input()        /-eq/ right -se-
  any2c([initial])[1].value().length /-eq/ 2     -se-
  any2c([initial])[1].value()[0]     /-eq/ tree  -se-
  any2c([initial])[1].value()[1]     /-eq/ right -se-

  any3c([initial]).length            /-eq/ 2        -se-
  any3c([initial])[0].position()     /-eq/ 0        -se-
  any3c([initial])[0].input()        /-eq/ right[0] -se-
  any3c([initial])[0].value().length /-eq/ 3        -se-
  any3c([initial])[0].value()[0]     /-eq/ tree     -se-
  any3c([initial])[0].value()[1]     /-eq/ right    -se-
  any3c([initial])[0].value()[2]     /-eq/ right[0] -se-
  any3c([initial])[1].position()     /-eq/ 1        -se-
  any3c([initial])[1].input()        /-eq/ right[1] -se-
  any3c([initial])[1].value().length /-eq/ 3        -se-
  any3c([initial])[1].value()[0]     /-eq/ tree     -se-
  any3c([initial])[1].value()[1]     /-eq/ right    -se-
  any3c([initial])[1].value()[2]     /-eq/ right[1] -where [tree = '3 + 4 * 5'.qs, left = tree[0], right = tree[1], initial = new array_state(tree),
                                                            any(states) = states *~![x.next(1, x.input())] -seq, any = annotate(any, 'any', []),

                                                            any2  = any /-bfs/ iv("_".qf), any3  = any /any /-bfs/iv("_".qf),
                                                            any2c = any /-bfc/ iv("_".qf), any3c = any /any /-bfc/iv("_".qf)],

  using [caterwaul.parser],

  where [eq(x, y) = x === y || null['#{x} should === #{y}']]})(caterwaul);

// Generated by SDoc 
