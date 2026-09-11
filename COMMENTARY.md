# Commentary: Understanding the Interpreter Plumbing

This file is a future-reference note for the parts of this Scheme
interpreter that are easy to forget, especially `ExceptT`, `liftIO`,
`liftThrows`, and `runIOThrows`.

The goal is not to explain the abstractions academically. It is to
record what the code is mechanically doing.

## 1. The interpreter's basic pipeline

At the highest level:

``` text
Scheme source
    ↓
parse
    ↓
LispVal
    ↓
eval
    ↓
LispVal
```

For example:

``` scheme
(+ 1 2)
```

is parsed approximately into:

``` haskell
List [Atom "+", Number 1, Number 2]
```

and then evaluated.

A function application eventually reaches:

``` haskell
eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
```

Conceptually:

``` text
evaluate function
→ evaluate arguments
→ apply function to evaluated arguments
```

------------------------------------------------------------------------

## 2. Why `eval` needs more than `LispVal -> LispVal`

The simplest imaginary evaluator would be:

``` haskell
eval :: Env -> LispVal -> LispVal
```

But evaluation can fail.

For example, valid Scheme syntax can still be invalid during evaluation:

``` scheme
(+ 1 "hello")
```

So we need something like:

``` haskell
eval :: Env -> LispVal -> Either LispError LispVal
```

where:

``` text
Right value = success
Left error  = evaluation failure
```

But evaluation also needs IO.

There are two main reasons in this interpreter:

1.  The environment is mutable and implemented using `IORef`.
2.  Scheme code can perform actual IO: reading input, writing output,
    loading files, opening ports, etc.

So the straightforward combined type would be:

``` haskell
eval :: Env -> LispVal -> IO (Either LispError LispVal)
```

This type is perfectly valid.

The problem is that it becomes annoying to compose repeatedly.

------------------------------------------------------------------------

## 3. Why plain `IO (Either e a)` is annoying

Suppose:

``` haskell
evalA :: IO (Either LispError LispVal)
evalB :: LispVal -> IO (Either LispError LispVal)
```

Then:

``` haskell
do
    result <- evalA
```

only unwraps the outer `IO`.

Therefore:

``` haskell
result :: Either LispError LispVal
```

We still need:

``` haskell
case result of
    Left err ->
        return (Left err)

    Right value ->
        evalB value
```

With many evaluation steps this becomes:

``` text
perform IO
→ inspect Either
→ propagate Left or continue with Right
→ perform IO
→ inspect Either
→ propagate Left or continue with Right
→ ...
```

This repetitive plumbing is the problem `ExceptT` solves.

------------------------------------------------------------------------

## 4. What `ExceptT` ACTUALLY is

This is the important concrete definition:

``` haskell
newtype ExceptT e m a =
    ExceptT (m (Either e a))
```

So `ExceptT` really does use `Either`.

For this interpreter:

``` haskell
ExceptT LispError IO LispVal
```

wraps:

``` haskell
IO (Either LispError LispVal)
```

The parameter order is:

``` text
ExceptT error baseMonad result
```

so:

``` haskell
ExceptT LispError IO LispVal
        ───────── ── ───────
          error   IO result
```

Do not mentally think that:

``` text
IO (Either Error a)
```

is somehow magically rearranged into:

``` text
ExceptT Error IO a
```

Instead think:

``` text
ExceptT has three configuration/type slots:
    error type
    underlying monad
    result type
```

and its implementation stores those as:

``` haskell
m (Either e a)
```

------------------------------------------------------------------------

## 5. The aliases in this interpreter

The interpreter defines:

``` haskell
type ThrowsError = Either LispError

type IOThrowsError = ExceptT LispError IO
```

These names are easy to misread.

Mentally, something clearer would be:

``` haskell
type Result = Either LispError
type Eval   = ExceptT LispError IO
```

Therefore:

``` haskell
ThrowsError LispVal
```

means:

``` haskell
Either LispError LispVal
```

and:

``` haskell
IOThrowsError LispVal
```

means:

``` haskell
ExceptT LispError IO LispVal
```

which wraps:

``` haskell
IO (Either LispError LispVal)
```

------------------------------------------------------------------------

## 6. How `ExceptT` sequencing actually works

There is no magic here.

The `Monad` instance for `ExceptT` contains the repetitive
error-propagation logic.

Very roughly, its bind behaves like:

``` haskell
ExceptT action >>= f =
    ExceptT $ do
        result <- action

        case result of
            Left err ->
                return (Left err)

            Right value ->
                runExceptT (f value)
```

Suppose:

``` haskell
first  :: ExceptT String IO Int
second :: Int -> ExceptT String IO Int
```

Then:

``` haskell
first >>= second
```

means approximately:

``` text
run first's underlying IO
→ get Either String Int

Left err
→ stop and propagate the error

Right 10
→ call second 10
→ continue with its computation
```

The library wrote this plumbing once so the interpreter does not have to
write the `case Left/Right` everywhere.

------------------------------------------------------------------------

## 7. Which monad does a `do` block use?

This is important when reading the implementation of `ExceptT` itself.

Consider:

``` haskell
ExceptT $ do
    result <- action
    ...
```

From:

``` haskell
newtype ExceptT e m a =
    ExceptT (m (Either e a))
```

the `ExceptT` constructor needs:

``` haskell
m (Either e a)
```

Therefore the `do` block inside:

``` haskell
ExceptT $ do ...
```

is operating in the underlying `m`, NOT in `ExceptT`.

For this interpreter:

``` haskell
m = IO
```

so:

``` haskell
ExceptT $ do
    result <- action
```

has an inner `do` block operating in `IO`.

If:

``` haskell
action :: IO (Either e a)
```

then:

``` haskell
result <- action
```

uses IO's bind and gives:

``` haskell
result :: Either e a
```

There are therefore two contexts to distinguish:

``` text
ExceptT bind
    ↓ implemented using
underlying monad's bind
    ↓
IO in this interpreter
```

------------------------------------------------------------------------

## 8. Why `runExceptT` appears inside bind

Suppose:

``` haskell
f :: a -> ExceptT e m b
```

Then:

``` haskell
f value :: ExceptT e m b
```

But inside:

``` haskell
ExceptT $ do ...
```

we need the inner representation:

``` haskell
m (Either e b)
```

So:

``` haskell
runExceptT (f value)
```

converts:

``` text
ExceptT e m b
    ↓
m (Either e b)
```

Then the outer:

``` haskell
ExceptT $ ...
```

wraps the whole computation back into:

``` haskell
ExceptT e m b
```

Important:

`runExceptT` does NOT extract the final `b`.

It only removes the `ExceptT` wrapper.

``` text
ExceptT e m b
    ↓ runExceptT
m (Either e b)
```

For this interpreter:

``` text
ExceptT LispError IO LispVal
    ↓ runExceptT
IO (Either LispError LispVal)
```

The `LispVal` is still inside `Either`, which is still inside `IO`.

------------------------------------------------------------------------

## 9. What `liftIO` really does

Inside evaluation we are working in:

``` haskell
ExceptT LispError IO a
```

but sometimes we have an ordinary IO operation:

``` haskell
readIORef envRef :: IO a
```

We need:

``` text
IO a
    ↓
ExceptT LispError IO a
```

That is what `liftIO` does.

The crucial detail:

`liftIO` is NOT a clever universal function that looks at an arbitrary
transformer and figures out how to wrap IO.

It is a method of the `MonadIO` typeclass:

``` haskell
class Monad m => MonadIO m where
    liftIO :: IO a -> m a
```

Compatible monads/transformers provide their own `MonadIO` instances.

So `ExceptT` has an implementation that knows how to lift IO into
`ExceptT`.

For our concrete shape, the transformation can be understood as:

``` text
IO a
    ↓ put successful value in Right
IO (Either e a)
    ↓ wrap
ExceptT e IO a
```

A simplified implementation for this specific case could look like:

``` haskell
myLiftIO :: IO a -> ExceptT e IO a
myLiftIO ioAction =
    ExceptT (Right <$> ioAction)
```

The important general reading strategy is:

``` text
mysterious polymorphic function
→ check its typeclass
→ check the instance for the concrete type
→ inspect what that implementation actually does
```

------------------------------------------------------------------------

## 10. What `liftThrows` really does

This function was written directly in the interpreter:

``` haskell
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val
```

Expand the aliases:

``` haskell
liftThrows
    :: Either LispError a
    -> ExceptT LispError IO a
```

So its job is simply:

``` text
Either LispError a
        ↓
ExceptT LispError IO a
```

This is useful because parsing and many pure primitive operations return
`Either`, while `eval` operates in `ExceptT`.

The name `liftThrows` is misleading if read as English.

It does NOT mean:

``` text
lift something and then throw it
```

It means:

``` text
lift a value of the type named ThrowsError
```

where:

``` haskell
type ThrowsError = Either LispError
```

A clearer mental name would be:

``` text
liftResult
```

------------------------------------------------------------------------

## 11. The common evaluator context

At this point the evaluator can receive computations from two different
worlds:

``` text
Either LispError a
        │
        │ liftThrows
        ▼
ExceptT LispError IO a
        ▲
        │ liftIO
        │
       IO a
```

This is the main architectural reason for the transformer.

During evaluation, everything can speak one common language:

``` haskell
ExceptT LispError IO a
```

and therefore participate in the same `do` / `>>=` chain.

------------------------------------------------------------------------

## 12. `trapError`

The interpreter defines roughly:

``` haskell
trapError action =
    action `catchError` (return . show)
```

Despite the name, it does not merely "trap" and preserve an error.

It catches a `LispError`, converts it to text with `show`, and returns
that text as a SUCCESSFUL value.

Conceptually:

``` text
success "42"
→ success "42"

error SomeLispError
→ success "SomeLispError"
```

A clearer mental name would be something like:

``` text
errorToString
```

or:

``` text
renderError
```

This matters because the next function relies on the error having been
converted into a normal value.

------------------------------------------------------------------------

## 13. `runIOThrows`

The interpreter has:

``` haskell
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action =
    extractValue <$> runExceptT (trapError action)
```

Expand the alias:

``` haskell
runIOThrows
    :: ExceptT LispError IO String
    -> IO String
```

It is basically the exit door from the evaluator.

Step by step:

``` text
ExceptT LispError IO String

        ↓ trapError

ExceptT LispError IO String
(errors have been converted to successful Strings)

        ↓ runExceptT

IO (Either LispError String)

        ↓ extractValue <$>

IO String
```

`extractValue` itself only handles `Right`:

``` haskell
extractValue (Right val) = val
```

That would normally be unsafe.

It is used here because `trapError` is expected to convert interpreter
errors into successful strings before `extractValue` runs.

A useful mental name for `runIOThrows` is simply:

``` text
runEval
```

------------------------------------------------------------------------

## 14. Reading `evalString`

The original expression:

``` haskell
evalString env expr =
    runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env
```

is easier to understand with explicit parentheses:

``` haskell
evalString env expr =
    runIOThrows
        (fmap show
            (liftThrows (readExpr expr) >>= eval env))
```

The type flow is:

``` text
expr
:: String

↓ readExpr

Either LispError LispVal

↓ liftThrows

ExceptT LispError IO LispVal

↓ >>= eval env

ExceptT LispError IO LispVal

↓ fmap show

ExceptT LispError IO String

↓ runIOThrows

IO String
```

Or mechanically:

``` haskell
step1 = readExpr expr
-- Either LispError LispVal

step2 = liftThrows step1
-- ExceptT LispError IO LispVal

step3 = step2 >>= eval env
-- ExceptT LispError IO LispVal

step4 = fmap show step3
-- ExceptT LispError IO String

step5 = runIOThrows step4
-- IO String
```

------------------------------------------------------------------------

## 15. Example: `(+ 1 2)`

Parsing:

``` scheme
(+ 1 2)
```

produces approximately:

``` haskell
List [Atom "+", Number 1, Number 2]
```

It reaches:

``` haskell
eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
```

with:

``` haskell
function = Atom "+"
args     = [Number 1, Number 2]
```

First:

``` haskell
func <- eval env (Atom "+")
```

The atom is looked up in the environment:

``` haskell
eval env (Atom id) = getVar env id
```

which produces the primitive function for `+`.

Then:

``` haskell
argVals <- mapM (eval env) args
```

evaluates:

``` text
Number 1 → Number 1
Number 2 → Number 2
```

giving:

``` haskell
[Number 1, Number 2]
```

Finally:

``` haskell
apply func argVals
```

reaches:

``` haskell
apply (PrimitiveFunc func) args =
    liftThrows $ func args
```

The primitive function returns a pure:

``` haskell
Either LispError LispVal
```

so `liftThrows` brings that result into the evaluator's:

``` haskell
ExceptT LispError IO LispVal
```

context.

The successful result is:

``` haskell
Number 3
```

------------------------------------------------------------------------

## 16. `mapM`, `traverse`, and `sequence`

A recurring source of confusion was:

``` haskell
argVals <- mapM (eval env) args
```

Given:

``` haskell
eval env :: LispVal -> IOThrowsError LispVal
```

then:

``` haskell
mapM (eval env)
    :: [LispVal]
    -> IOThrowsError [LispVal]
```

Conceptually:

``` text
evaluate each LispVal
→ if all succeed, collect the resulting values
→ if one fails, the combined computation fails
```

Modern Haskell can express this with `traverse`:

``` haskell
argVals <- traverse (eval env) args
```

The related `sequence` operation can be understood first with `Maybe`:

``` haskell
sequence [Just 1, Just 2, Just 3]
-- Just [1,2,3]

sequence [Just 1, Nothing, Just 3]
-- Nothing
```

Its list-specialized shape is:

``` haskell
sequence :: [m a] -> m [a]
```

So:

``` haskell
map f xs
```

first gives:

``` haskell
[m b]
```

and `sequence` combines those into:

``` haskell
m [b]
```

`traverse` packages the mapping and collecting together.

------------------------------------------------------------------------

## 17. Similar names that are NOT the same thing

### `>>`

``` haskell
(>>) :: Monad m => m a -> m b -> m b
```

Run the first computation, ignore its result, then run the second.

Think:

``` text
thenIgnore
```

### `>>=`

``` haskell
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

Run the first computation and give its successful result to the next
computation.

Think:

``` text
thenWith
```

### `sequence`

Combines a collection of effectful results:

``` text
[m a] → m [a]
```

### `seq`

Completely different concern.

``` haskell
seq :: a -> b -> b
```

It is about forcing evaluation in lazy Haskell, not monadic sequencing.

The naming overlap is unfortunate.

------------------------------------------------------------------------

## 18. Practical reading rules for future me

When Haskell starts feeling magical again, do NOT settle for statements
like:

> "It lifts the action into the monadic context."

Instead ask:

1.  What is the exact type?
2.  Expand all type aliases.
3.  Substitute the concrete types for the generic type variables.
4.  Is this function a typeclass method?
5.  If yes, which instance is being used here?
6.  What does that instance actually implement?
7.  If it is a transformer, what is its actual underlying
    representation?
8.  What does each `<-` unwrap in this particular `do` block?
9.  What is the type of the entire `do` block?
10. What does the wrapper constructor require?

For this interpreter, always remember the concrete anchor:

``` haskell
newtype ExceptT e m a =
    ExceptT (m (Either e a))
```

Therefore:

``` haskell
ExceptT LispError IO LispVal
```

means a wrapper around:

``` haskell
IO (Either LispError LispVal)
```

Everything else can be derived from there.

------------------------------------------------------------------------

## 19. Short cheat sheet

``` text
ThrowsError a
= Either LispError a

IOThrowsError a
= ExceptT LispError IO a
≈ wrapper around IO (Either LispError a)


liftThrows
Either LispError a
→ ExceptT LispError IO a


liftIO
IO a
→ ExceptT LispError IO a

Implemented according to the MonadIO instance.
It does not magically discover how to lift arbitrary transformers.


runExceptT
ExceptT e m a
→ m (Either e a)

It removes the ExceptT wrapper.
It does NOT extract the final a.


trapError
interpreter error
→ printable String treated as success


runIOThrows
ExceptT LispError IO String
→ IO String


ExceptT bind
Right value → pass value to next operation
Left error   → stop and propagate error
```

## The main takeaway

The interpreter's transformer plumbing is fundamentally simple:

``` text
Evaluation needs:
    error handling
    +
    IO

Raw representation:
    IO (Either LispError a)

ExceptT representation:
    ExceptT LispError IO a
```

`ExceptT` exists mainly so we do not manually write the same:

``` text
run IO
→ inspect Either
→ propagate Left
→ continue with Right
```

logic after every evaluation step.

The abstraction is useful. The terminology makes it look considerably
more complicated than the underlying mechanism.
