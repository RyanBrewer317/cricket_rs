# Cricket

A functional and object-oriented programming language written in Rust. This is a port of the [cricket](https://github.com/RyanBrewer317/cricket) project, which was written in Haskell, so in some places the code might not be very idiomatic Rust. I write more typical Rust code in [SaberVM](https://github.com/RyanBrewer317/SaberVM). However, note that this implementation has additional features over the Haskell original, and of course runs faster.

Some basic documentation can be found [here](https://github.com/RyanBrewer317/cricket/blob/main/docs/intro.md). In addition I will explain it further here, and note that all the code examples work right now! You can run them like so:
```
cargo build --release
./target/release/cricket_rs main.ct
```
(where the code is in `main.ct`. Note that leaving off the `main.ct` will start a prompt where you can enter code.)

Here's a hello-world example in Cricket:
```
def main:
  console.write("Hello, world!")
```

Cricket is *tiny,* with exactly 1700 lines of well-formatted Rust code at time of writing, including blank lines and comments. This is intentional: it has a small footprint when embedded in other systems, and it's easy for anyone to understand and modify. This minimalism still allows for laziness, a decently fast implementation, a linear-time type-based linter, a module system, abstract data types with bare-bones pattern matching, great error messages, and more. I designed Cricket carefully to make this happen.

Cricket is *lazy,* which means that it doesn't execute code that doesn't impact the final result. This comes with some awesome optimization, but it also complicates side effects (which often don't affect the final result). So Cricket also has a simple way to declare that some code *does* affect the final result:
```
def main:
  let force _ = console.write("Hello,")
  in console.write("world!")
```
The `force` keyword is a special function that forces the execution of the code inside it. If we leave it out, the first console write won't happen:
```
def main:
  // only prints "world!"
  let _ = console.write("Hello,")
  in console.write("world!")
```
Laziness in Cricket is implemented with a Krivine machine, so it's a fairly efficient stack-based execution model with highly-shared immutable closures.

Cricket is *gradually typed,* which means that it does its best to infer types and give warnings, but it gives things the type `Dynamic` if it can't figure it out. Therefore typechecking is more like a linter than a static analyzer, and indeed Cricket code can still be run even if it has type errors. The mantra of gradually typed systems is that all correct programs are valid, but some incorrect programs are caught. This is opposed to statically typed systems, where all incorrect programs are caught, but some correct programs are not allowed either.

Here's a program that Cricket *doesn't* catch:
```
def main:
  let f = x-> x.y
  in console.write(f(3))

Runtime error. `3` is not an object, cannot access method `y` at `main.ct:2:18`.
```
Here's a program that Cricket *does* catch:
```
def main:
  (x-> x.y)(let z = 3 in z)

Warning: Expected object, got `Int` at `main.ct:2:11`.
Hit `Enter` or `Return` to run anyway, or `Ctrl-C` to exit...
```
I choose this example to show off the fact that Cricket uses "application mode bidirectional type checking," (which isn't really bidirectional), based on the "Let Arguments Go First" paper by Xie and Oliviera. Cricket desugars let-bindings into immediately-applied lambdas, so the application mode is very helpful.

Cricket also has a simple notion of effect type: if computing a value would cause some I/O side effects, then the linter will keep track of that, and suggest `let force`-ing it into a pure value before giving it as the argument to any function. This is helpful for if you ever forget about Cricket's laziness; `let force`-ing the side effects away ensures predictable program behavior.

Cricket is *object-oriented,* which means that it uses objects for most of the computation. It uses an immutable variant of typical (so to speak) prototypical OOP. In other words, instead of classes, there's a way to extend objects:
```
def main:
  let dog = {bark: "woof!"} // the prototype
  in let cooper = dog <- name: "Cooper"
  in console.write(cooper.name + " says: " + cooper.bark)
```
The `<- :` operator is not-updating in-place, but returning a modified copy. Therefore we pronounce it "with."

In fact, `Cricket` objects have something like a `this` implicit parameter! They're introduced per-method, like so:
```
{x: 3, this.y: this.x}
```
The `this.y:` part brings a new variable into scope, which we've named `this`, and it refers to the whole object, so `this.x` is `3`! The linter tries its best with this recursive typing, and honestly does a pretty good job. Here's a program that shows off the linter's understanding of this recursive typing:
```
{x: {y: z->1}, this.w: console.write(this.x)}

Warning: Expected `Int | Float | String`, but found `{y: (Dynamic) -> Int}` at `input:1:42`.
Hit `Enter` or `Return` to run anyway, or `Ctrl-C` to exit...
```

Cricket is *functional,* meaning everything is immutable and functions are first-class. It isn't *pure,* so arbitrary code can have side effects, which is very useful. On the other hand, side-effectful code often has to be annotated with `force`, which is helpful for clarity. Here's a program showing off functional idioms:
```
def fold(acc)(f)(l): 
  l.case{
    Empty: acc,
    Has(first)(rest): 
      let force x = f(acc)(first) in
      fold(x)(f)(rest)
  }

import stdlib/List

def main:
  let add = x-> y-> x + y
  in let l = List[1, 2, 3]
  in let l2 = l.map(add(1))
  in fold(0)(acc-> x-> let force _ = console.write(x) in acc)(l2)
```
Why are parameters individually enclosed in parentheses? Cricket functions only take one argument, so this is a nice syntax sugar for currying.

Want to know a secret? The `List` ADT above, with its `Empty` and `Has` constructors, is actually just objects and functions! In cricket, `x{...}` is syntax sugar for `x({...})`, so `.case{...}` is actually an object method call, and the case analysis is just a regular object being passed as the argument! `List` is defined in `stdlib/List.ct` like so:
```
def Empty: {
  case(c): c.Empty,
  map(f): Empty,
  concat(l): l,
  flat: Empty
}

def Has(first)(rest): {
  case(c): c.Has(first)(rest),
  map(f): Has(f(first))(rest.map(f)),
  concat(l): Has(first)(rest.concat(rest)),
  flat: first.concat(rest.flat)
}
```
See how the `case` method just calls the corresponding method of its argument? That's the whole magic that brings the ADT to Cricket. This means that ADTs in Cricket are actually "Church-encodings," but with objects instead of lambdas. For reference, here's the corresponding Church-encoded list:
```
def Empty: empty-> has-> empty
def Has(first)(rest): empty-> has-> has(first)(rest)

def main:
  let l = Has(1)(Has(2)(Empty))
  // print 1
  in console.write(
    l("empty")(first-> rest-> first)
  )
```
Hopefully you can see that even though this is just lambda-calculus, it completely mirrors the object-oriented version. Hence I continue to call Cricket's ADTs Church encodings. Using objects affords us some niceties though, like a better case analysis syntax and things like `l.map(f)` which wouldn't be possible if `l` were a function instead of an object.

In the context of Cricket's recursive objects, it also means that our case analysis can be recursive without defining an external function! Here's an example:
```
import stdlib/List

def main:
  let l = List[1, 2, 3]
  // print 1 2 3
  in l.case{
    Empty: 0,
    this.Has(first)(rest):
      let force _ = console.write(first) in
      rest.case(this) // where the magic happens!
  }
```

What's with that `List[1, 2, 3]` syntax? We saw the definition of List, which had no square brackets, so is this thing just syntax sugar too? Yes! Cricket rewrites `x[a, b, ...]` to `x.Has(a)(x.Has(b)(...(x.Empty)))`, so `List[1, 2, 3]` is actually `List.Has(1)(List.Has(2)(List.Has(3)(List.Empty)))`, which should look familiar if you're used to the linked list ADTs common in functional languages.

I call this a "fold function" and they're useful in other situations too. Here's an example of a variadic sum function:
```
def sum: {
  Has(first)(rest): first + rest,
  Empty: 0
}

def main:
  // prints 10
  console.write(sum[1, 2, 3, 4])
```

### Todo
- I'd love real (equirecursive) self types without compromising the linear time complexity of the linter.
- low-hanging fruit optimizations, like representing method names as integers instead of doing runtime string comparisons.
- explicit type annotations? If adding all that syntax isn't too much more code. In theory object subsumption is already supported but has no use until type annotations can be added.
- runtime type switching: do one thing if something is one type, and another thing if it's another type. I might skip this in favor of categorical purity lol.
- a much better standard library, especially for booleans and hashmaps.
- conversions between numeric types and strings.
- a license, now that this repo is more featured than the original.
- an IDE plugin; the linter is begging for this.
