# ContEffects

A tiny programming language with algebraic effects. It was named ContEffects because its implementation was inspired by [the `Cont` monad](https://hackage.haskell.org/package/transformers-0.5.6.2/docs/Control-Monad-Trans-Cont.html). I thought I'd end up using that monad to implement effect handlers, similar to the way I usually use `ExceptT` to implement throwing and catching exceptions in an interpreter, but I ended up just needing `ReaderT`.

Algebraic effects are like exceptions that you can resume from. Rather than "throwing" and "catching" an exception and continuing from where you caught it, you "perform" and "handle" an effect and resume at the place where it was "performed"

Effects are implemented as implicitly passed callbacks

```
try (1 + 2 + perform 5)
handle e (e + e)
```
will evaluate to 12

In this example, 5 is an effect, and the handler is `e -> e + e`. This is a little silly, since usually effects are structured objects with information, but having effects be any old value is enough to demonstrate the mechanism. In a more sophisticated language, effects would have their own special values and you'd handle specific exceptions like this:

```
try (...)
handle ThisEffect as e (...)
handle ThatEffect as e(...)
```

These effects would also have data in them. A real-world example would be running an HTTP request:

```
function go() {
  ...
  let json = perform Fetch("https://www.some.url.com/endpoint") in
  ...
}
```

You could handle a `Fetch` effect by really running an HTTP request, or you could return some dummy data if you want to test this function:

```
try( go() ) handle Fetch as f ( {"name": "big chungus"} )
```

Effects bubble up to the nearest handler. When the handler's body evaluates, the `perform` evaluates to that value and the computation continues. Handlers themselves can perform effects, and an outer handler can handle the effect, allowing the inner handler to resume and provide the initial `perform` with a value. For example,

```
try (
  try (1 + perform 2)
  handle e (e + perform 3)
) handle e (e + e)
```
will evaluate to 9

If an effect is unhandled and reaches the top-level, the program crashes.

`perform` can be used outside of a `try`:

```
let go = \x -> 1 + perform x in
try( go(3) )
handle e (e + e)
```

will evaluate to 7

# how it works

I use the reader monad with an environment that contains a mapping of variable names to values and the current handler:

```haskell
data Env = Env { _vars :: Map String Value, _handler :: Value }

data Value = VInt Int | VUnit | VLambda (Map String Value) (Maybe Value) String Expr | VBuiltinFun (Value -> Interpreter Value)
```

in a try/handle expression, I just convert the `handle e (...)` to a `VLambda` and evaluate the try expression using that lambda as the handler. There needs to be some special care for ensuring the handler and try code use the correct `Env`s, but that's the gist of how it works. Here are the gotchas:
* a handler's lambda must close over the variables that were in scope before the try and not use the environment in which the effect was performed
* when running a handler's code, the "current handler" in its env must be the handler of the whole try/handle, that way handlers can perform effects too
* a lambda should be able to perform effects and then be ran in a variety of handlers. So the behavior of `perform` has to be dynamic in the same way that an exception thrown in a function can be caught a variety of ways depending on how the caller catches it
