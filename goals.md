# Project 1: A Dependent Type Checker

In this project, you will be implementing a language with a dependent type checker.
The expected functionality will be presented in [Agda](https://agda.readthedocs.io/en/latest/index.html), but remember that you are free to design your language however you find suitable (i.e., you do not have to follow Agda's syntax).
If you are not sure how certain features should work, we recommend playing around with Agda and using it to determine expected behaviour.
You may deviate from this behaviour, if you can explain the reasoning behind your choices.

This project has five phases:
* `[0]` [Recommended Changes](#phase-zero-recommended-changes)
* `[1]` [Dependent Functions and a Universe](#phase-one-dependent-functions-and-a-universe)
* `[2]` [Built-in Types](#phase-two-built-in-types)
* `[3]` [Indexed Datatypes](#phase-three-indexed-datatypes)
* `[4]` (optional) [Wildcards and Unification](#optional-phase-four-wildcards-and-unification)

> **Note:** To get the Agda examples in this project working, add the following lines to the top of your Agda file:
>
> ```haskell
> {-# OPTIONS --type-in-type --no-import-sorts #-}
>
> open import Agda.Primitive renaming (Set to U)
> ```

## Phase Zero: Recommended Changes

We recommend modifying the template project in the following ways before starting with Phase One.


### (1) Convert integers to natural numbers

Natural numbers are more convenient to use with dependent types, so it will help to restrict the type to just non-negative numbers (and rename the type to `nat`).
It is also useful to add a successor function `suc`.

### (2) De Bruijn indices

<details>
<summary>
We recommend using a pre-processing step to convert variable names to [de Bruijn indices](https://www.cs.cornell.edu/courses/cs4110/2018fa/lectures/lecture15.pdf).
</summary>

De Bruijn indices are a *nameless* representation of variables where each variable is represented by the number (called the *de Bruijn index*) of additional variables that have been bound since the variable that is referred to.

Two useful helper functions on syntax with de Bruijn indices are the following:

* An operation `shift :: Int -> Int -> Expr -> Expr` such that `shift n k` increases all de Bruijn indices larger or equal to `n` by `k`, while leaving indices smaller than `n` unchanged.
* An operation `subst :: Int -> Expr -> Expr -> Expr` such that `subst n u` replaces all occurrences of variable with de Bruijn index `n` with `u`, while decremening indices larger than `n` by one.
</details>

### (3) Bidirectional type checking

<details>
<summary>
We recommend changing the type checker so it uses [bidirectional type checking](https://davidchristiansen.dk/tutorials/bidirectional.pdf).
</summary>

This means that there should be a function `check` that checks if an expression has a given type, and a function `infer` that infers the type of an expression without knowing it in advance.
In particular, the `check` function is useful for cases where the type cannot (easily) be inferred, such as lambda expressions (see below).
</details>

### (4) Lambda expressions

<details>
<summary>
It should be possible to use lambda expressions (i.e., anonymous functions) instead of just named functions.
This means that closures (functions) are also valid values and first-order types, meaning they can be passed along as any other value.
</summary>

The examples below make use of the syntax `\x -> u` like in Haskell, but you are free to choose a different syntax if you prefer.

```haskell
lam1 : int -> bool
lam1 = \ x -> x < 5

lam2 : int -> int
lam2 = \x -> x * x
```
</details>


## Phase One: Dependent Functions and a Universe

A minimal version of a dependently-typed language should include the following features:

- A dependent function type `(x : a) -> b` where `x` is a variable of type `a` that can appear inside `b`.
  Values of this type are lambda expressions `\x -> u` where `u` has type `b` in a context where `x` has type `a`.
- A universe type `U` where
  - all basic types such as `nat` and `bool` are expressions of type `U`,
  - `U` has type `U`,
  - `(x : a) -> b` has type `U` whenever both `a` has type `U` and `b` has type `U`.

The type checker should evaluate expressions during type checking.
For expressions that involve free variables, it should evaluate them as far as possible.

<details><summary>This should be sufficient to write some examples involving Church encoding.</summary>

<br>

> These example assumes some syntactic sugar:
> * `a -> b` is `(x : a) -> b` for an unused variable `x`,
> * `a -> b -> c` is `a -> (b -> c)`, and
> * `\x y z -> u` is `\x -> (\y -> (\z -> u))`.

<details><summary><b>Booleans</b></summary>

```haskell
CBool : U
CBool = (a : U) -> a -> a -> a

ctrue : CBool
ctrue = \a x y -> x

cfalse : CBool
cfalse = \a x y -> y
```

</details>
<details><summary><b>Boolean Operations</b></summary>

```haskell
cnot : CBool -> CBool
cnot = \b -> b CBool cfalse ctrue

cand : CBool -> CBool -> CBool
cand = \b1 b2 -> b1 CBool b2 cfalse

cor : CBool -> CBool -> CBool
cor = \b1 b2 -> b1 CBool ctrue b2

cif : (a : U) -> CBool -> a -> a -> a
cif = \a b -> b a
```

</details>
<details><summary><b>Natural Numbers</b></summary>

```haskell
CNat : U
CNat = (a : U) -> (a -> a) -> a -> a

czero : CNat
czero = \a s z -> z

csuc : CNat -> CNat
csuc = \x -> \a s z -> s (x a s z)
```

</details>
<details><summary><b>Natural Number Examples</b></summary>

```haskell
cone : CNat
cone = csuc czero

ctwo : CNat
ctwo = csuc cone
```

</details>
<details><summary><b>Natural Number Operations</b></summary>

```haskell
cplus : CNat -> CNat -> CNat
cplus = \x y -> x CNat csuc y

cmult : CNat -> CNat -> CNat
cmult = \x y -> x CNat (cplus y) czero
```

</details>

</details>

### [REQUIRED WITH 2] A Hierarchy of Universes

A single universe `U` with `U : U` is known to lead to inconsistency.

<details><summary>It is possible to construct a proof of <b>Hurken's paradox</b> that uses <code>U : U</code> to prove false.</summary>

```haskell
-- the 'empty' type
CBot : U
CBot = (a : U) -> a

P : U -> U
P = \S -> (S -> U)

V : U
V = (X : U) -> (P (P X) -> X) -> P (P X)

tau : P (P V) -> V
tau = \t X f p -> t (\x -> p (f (x X f)))

sigma : V -> P (P V)
sigma = \s -> s V tau

Delta : P V
Delta = \y -> (((p : P V) -> sigma y p -> p (tau (sigma y))) -> CBot)

Omega : V
Omega = tau (\p -> ((x : V) -> sigma x p -> p x))

R : (p : P V) -> ((x : V) -> sigma x p -> p x) -> p Omega
R = \p x1 -> x1 Omega (\x -> x1 (tau (sigma x)))

M : (x : V) -> sigma x Delta -> Delta x
M = \x x2 x3 -> x3 Delta x2 (\p -> x3 (\y -> p (tau (sigma y))))

L : ((p : P V) -> ((x : V) -> sigma x p -> p x) -> p Omega) -> CBot
L = \x0 -> x0 Delta M (\p -> x0 (\y -> p (tau (sigma y))))

-- An element of the 'empty' type can now be constructed
-- essentially proving "false" (which should be impossible)
false : CBot
false = L R
```

</details>

<br>

While it is extremely unlikely that you run into this problem in practice, you can fix the inconsistency by introducing a hierarchy of universes `U0 : U1`, `U1 : U2`, `U2 : U3`, ...
For function types, the universe is determined as the *maximum* of the universe of the domain and the codomain.

## Phase Two: Built-in Types

In this phase you can extend the language with a few basic types and functions.

### Basic types

<details>
<summary>
	Add a unit type <code>Top : U</code> with an element <code>tt : Top</code>,
</summary>

```haskell
topExample : Top
topExample = tt
```

</details>

<details>
<summary>
	Add an empty type <code>Bot : U</code> with a function <code>magic : Bot -> a</code> for any type <code>a</code>,
</summary>

```haskell
bottomExample : Bot
bottomExample = ? -- impossible to fill
```
</details>

### Eliminators for bool and nat

An *eliminator* (a.k.a. an *induction principle*) allows us to prove facts about elements of an inductive type by case analysis and induction.

<details>
<summary>
	Add an eliminator <code>elimBool</code> for the boolean type.

</summary>

The type of `elimBool` should be the following:
```haskell
elimBool : (P : bool -> U) -> P true -> P false -> (b : bool) -> P b
```

This eliminator is similar to an if/then/else expression with a different order of the arguments, but the type of the branches can depend on the boolean input as specified by the first argument. In particular, `elimBool P t e true` should compute to `t`, and `elimBool P t e false` should compute to `e`.

```haskell
natOrBool : (b : bool) -> if b then nat else bool
natOrBool b = elimBool (\b -> if b then nat else bool) 42 true b

testNatOrBoolTrue : nat
testNatOrBoolTrue = natOrBool true

testNatOrBoolFalse : bool
testNatOrBoolFalse = natOrBool false

bOrNotB : (b : bool) -> IsTrue (b || not b)
bOrNotB = elimBool (\x -> IsTrue (x || not x)) tt tt
```

</details>

<details>
<summary>
[REQUIRED WITH 2] You should also implement the eliminator <code>elimNat</code> for natural numbers.
</summary>

```haskell
elimNat : (P : nat -> U) -> P zero -> ((m : nat) -> P m -> P (suc m)) ->
```

This eliminator should be sufficient to prove for example that `0 + k = k + 0` for any natural number `k`.

```haskell
plusZero : (k : nat) -> IsTrue (0 + k == k + 0)
plusZero = ... -- Implement this yourself!
```
</details>

### Pair types

<details>
<summary>
	You should exten the language with a product type (pair) <code>(a , b) : [ A , B ] </code> with projections <code>fst</code> and <code>snd</code>, returning the first and second element of the pair respectively.
</summary>

```haskell
constructPair : A -> B -> [ A , B ]
constructPair a b = ( a , b )
```

```haskell
swap : [ A , B ] -> [ B , A ]
swap p = ( snd p , fst p )
```
</details>

### [REQUIRED WITH TWO] Sum types

<details>
<summary>
If you are working with two, you should also add <b>sum types</b>, a.k.a. the Haskell <code>Either</code> type.
</summary>

```haskell
both : [ Either Int bool , Either Int bool ]
both = ( left 42 , right true )
```

It should also have the corresponding eliminator:

```haskell
elimEither :  (A B : U) -> (P : Either A B -> U)
           -> ((x : A) -> P (left x))
					 -> ((y : B) -> P (right y))
					 -> (z : Either A B) -> P z
```
</details>

### [OPTIONAL] General user-defined types

You could also add a *general mechanism* for letting the user define their own data types.
For each type, you should automatically generate the eliminator (a.k.a. the induction principle).

Instead of eliminators, you could also add a general syntax for *pattern matching* on data types.

## Phase Three: Indexed Datatypes

This phase will add indexed datatypes to your language, specifically the identity type and vectors.

If you are working alone, you are only required to implement one of these two types.

### The Identity Type

The identity type `Id A x y` (sometimes also written `x = y` in case the type is obvious) expresses equality between `x` and `y`.

<details>
<summary>
	We can already define a weak version of the identity type using Church encoding, which has reflexivity, symmetry, transitivity, congruence, and substitution.
</summary>

```haskell
id : (a : U) -> a -> a
id = \a x -> x

CId : (a : U) -> a -> a -> U
CId = \a x y -> (p : a -> U) -> p x -> p y

crefl : (a : U) -> (x : a) -> CId a x x
crefl = \a x p -> id (p x)

csym : (a : U) -> (x y : a) -> CId a x y -> CId a y x
csym = \a x y p -> p (\z -> CId a z x) (crefl a x)

ctrans : (a : U) -> (x y z : a) -> CId a x y -> CId a y z -> CId a x z
ctrans = \a x y z p q -> q (\w -> CId a x w) p

ccong : (a b : U) -> (f : a -> b) -> (x y : a) -> CId a x y -> CId b (f x) (f y)
ccong = \a b f x y eq -> eq (\z -> CId b (f x) (f z)) (crefl b (f x))

csubst : (a : U) -> (p : a -> U) -> (x y : a) -> CId a x y -> p x -> p y
csubst = \a p x y eq -> eq p

J : (a : U) -> (x : a) -> (p : (y : a) -> CId a x y -> U) ->
    (p0 : p x (crefl a x)) -> (y : a) -> (eq : CId a x y) -> p y eq
J = {!   !} -- cannot be implemented for Church encoding!
```

</details>

<br>

However, a proper identity type should also support the so-called J rule (a.k.a. the "path induction principle"):

```haskell
J : (a : U) -> (x : a) -> (p : (y : a) -> Id a x y -> U) ->
    (p0 : p x (refl a x)) -> (y : a) -> (eq : Id a x y) -> p y eq
J a x p p0 x (refl a x) = p0
```

For this assignment, you should implement the identity type `Id` as a built-in type with the `refl` constructor as well as the `J` rule for it.
Then, using `J`, you can derive the other rules for symmetry, transitivity, congruence, and substitution.

### Vectors

<details>
<summary>
Add a type of vectors, i.e. lists indexed by natural numbers which represent their length.
</summary>

Create such a type with constructor for the empty list (e.g., `[]`) and for an element prepended to the list (e.g., `a :: []`).

```haskell
empty : Vector bool zero
empty = []

singleton : bool -> Vector bool (suc zero)
singleton b = b :: []

fail : Vector bool (suc zero)
fail = [] -- error! 0 != 1
```

The `Vector` type should support the following operations:

```haskell
head : (A : U) -> (n : nat) -> Vec A (suc n) -> A
tail : (A : U) -> (n : nat) -> Vec A (suc n) -> Vec A n
append : (A : U) -> (m n : nat) -> Vec A m -> Vec A n -> Vec A (m + n)
```

[OPTIONAL] You can also add the eliminator for `Vector`, which should have the following type:

```haskell
elimVec : (A : U) -> (P : (n : nat) -> Vec A n -> U)
        -> P zero []
				-> ((m : nat) (y : A) (ys : Vec A m) -> P m ys -> P (suc m) (y :: ys))
				-> (n : nat) (xs : Vec A n) -> P n xs
```

The `elimVec` function should be sufficient to implement the other functions `head`, `tail`, and `append` without having them as primitives.
</details>


### [REQUIRED WITH 2] The Fin type

<details>
<summary>
Extend your language further with the <code>Fin</code> type with constructors <code>fzero</code> and <code>fsuc</code>.
</summary>

```haskell
Fin : nat -> U
fzero : (m : nat) -> Fin (suc m)
fsuc  : (m : nat) -> Fin m -> Fin (suc m)
```
</details>

<details>
<summary>
You should also add the safe lookup operation on vectors.
</summary>

```haskell
lookup : (A : U) (n : nat) -> Vector A n -> Fin n -> A
```
</details>

<details>
<summary>
[OPTIONAL] You can also define the eliminator for <code>Fin</code> with the following type:
</summary>

```haskell
elimFin :  (P : (n : nat) -> Fin n -> U)
        -> ((m : nat) -> P (suc m) (fzero m))
				-> ((m : nat) (g : Fin m) -> P m g -> P (suc m) (fsuc g))
				-> (n : nat) (f : Fin n) -> P n f
```

This eliminator should be sufficient to implement `lookup` as a function (instead of defining it as a primitive).
</details>

### [OPTIONAL] Other Indexed Datatypes

Extend the language with other indexed datatypes, or even with a general mechanism for defining indexed datatypes.
Each indexed datatype should come with its own eliminator.

## [OPTIONAL] Phase Four: Wildcards and Unification

<details>
<summary>
The final (stretch) goal for this project is to extend the language with wildcards.
These should have syntax <code>_</code> (underscore) and are placeholders for arguments that can be inferred by the type checker.
</summary>

Internally, a wildcard should be represented as a *metavariable* (or "meta" for short), and your type checker should have a global state that keeps track of the current status of each metavariable (unassigned or assigned to a specific expression).

Things to take into account:
- The conversion checker that checks whether two types/terms are equal now becomes a *unification algorithm* that can solve metavariables in the process.
- Metavariables can depend on variables that are bound at the point where they were created.
  Thus, their syntax should carry a substitution.
- Take care to check that you do not solve a metavariable with a solution that contains the metavariable itself ("occurs check").
- The proper algorithm for solving equations between expressions with metavariables that can depend on bound variables is called [*higher-order pattern unification*](https://adam.gundry.co.uk/pub/pattern-unify/pattern-unification-2012-07-10.pdf).
  It is highly recommended to not attempt to implement this full algorithm, but to first implement a more restricted version.
  Here are some different versions you can try, in increasing order of difficulty:
	1. Only solve metas with solutions that do not contain any free variables.
	2. Only solve metas when applied to the identity substitution.
	3. Only solve metas when applied to a weakening substitution.
	4. Solve metas applied to an arbitrary permutation.
	5. Do full, higher-order pattern unification by adding a pruning phase during the occurs check.
	6. Make the unification *dynamic* by allowing problems to be postponed if they are blocked on an unsolved metavariable, and resumed when that metavariable is solved.

Please ask for help if you need assistance with understanding what each of these phases mean!
</details>

