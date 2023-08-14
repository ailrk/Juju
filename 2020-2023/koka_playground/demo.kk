import std/num/random

// mutation is allowed
fun hello-ten()
  var i := 0
  while { i < 10 }
    println("hello")
    i := i + 1

// while is actually implemented as a function. the sugar makes it looks
// like a regular language.
// while desugared:

// this syntatic convience give a nice little benefit:
//   tings between braces are cbn while between parenthesis are cbv.
fun hello-ten1()
  var i := 0
  while (fn() { i < 10 }, fn () {
    println("hello")
    i := i + 1
  })

// effect carries along types.
// implementation for mapM
fun mapM (xs : list<a>, f : a -> e b) : e list <b>
  match xs
    Cons (x, xx) -> Cons( f(x), mapM(xx, f) )
    Nil          -> Nil

// effect handlers

// define yield effect
effect yield
  ctl yield ( i: int ) : bool     // define a control operation

fun traverse( xs : list<int> ) : yield ()
  match xs
    Cons(x, xx) -> if yield (x) then traverse(xx) else ()
    Nil         -> ()

fun print-elems () : console ()
  with ctl yield (i)        // with bind the handler to the rest of the scope
    println("yielded " ++ i.show)
    resume(i<=2)
  traverse([1,2,3,4])

// perceus optimized ref counting
// an memory management technique witout the need of a gc.
// compile to plain c code directly.

// brace elison
fun main1() {
  println("hello world")
}

fun main2()
  println("hello world")

fun encode( s : string, shift : int )
  fun encode-char(c)
    if c < 'a' || c > 'z' then return c
    val base = (c - 'a').int
    val rot = (base + shift) % 26
    (rot.char + 'a')
  s.map(encode-char)

fun caesar(s : string ) : string
  s.encode(3)


// dot selection

// . is essentially >>>
fun showit( s : string )
  s.encode(3) . count . println

// type inference
// hm based imperdicative


// lambda

fun encode2( s : string, shift : int )
  s.map(fn(c) {
    if c < 'a' || c > 'z' then return c
    val base = (c - 'a').int
    val rot = (base + shift) % 26
    (rot.char + 'a')
  })

// sugar for lambda (trailing lambda):
fun print10()
  for (1, 10) fn (i) {
    println(i)
  }

// for fn (){ }, fn can be ignored entirely.
fun printhi10()
  repeat(10) {
    println("hi")
  }

fun print11()
  var i := 10
  while { i >= 0 } {
    println(i)
    i := i - 1
  }


// with statement for pasing function block
fun twice(f) {
  f()
  f()
}

fun test-twice() {
  twice( fn() {
    twice( fn() {
      println("hi")
    })
  })
}

// with is a bit like do notation
fun test-with1() {
  with twice
  with twice
  println("hi")
}

fun test-with2() {
  with x <- list(1, 10).foreach
  println(x)
}

fun test-finally() {
  with finally { println("exiting..")}
  println("entering..")
  throw("oops") + 42
}


// with handlers

effect fun emit(msg: string) : ()
fun hello()
  emit("hello")

fun emit-console1()
  with handler { fun emit(msg) { println(msg) } }
  hello()

// combining effects. Effect can be inferred.

// <div, exn, ndet>
fun combine-effects()
  val i = srandom-int()   // non deterministic
  throw("oops")           // exception raising
  combine-effects()       // non deterministic


// polymorphic effect
