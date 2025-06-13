# DeBruijn Index Representation with Lenses and Traversals

## Overview

This document explains how **Control.Lens** optics (lenses and traversals) are used in the `DeBruijn.hs` module to elegantly manipulate De Bruijn indexed expressions. The module converts named variables to De Bruijn indices and provides operations for working with these indexed expressions.

## What are De Bruijn Indices?

De Bruijn indices are a way to represent variables in lambda calculus and type theory without using names. Instead of names, variables are represented by numbers indicating how many lambda abstractions separate the variable from its binding site.

For example:

- `λx. x` becomes `λ. 0` (variable refers to the immediately enclosing lambda)
- `λx. λy. x` becomes `λ. λ. 1` (variable refers to the lambda 1 level up)
- `λx. λy. y` becomes `λ. λ. 0` (variable refers to the immediately enclosing lambda)

## Lenses and Traversals: A Brief Introduction

### Lenses

A **lens** is a functional programming construct that allows you to focus on a specific part of a data structure. It provides:

- **Getting**: Extract a value from a structure
- **Setting**: Update a value in a structure immutably

```haskell
type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s
```

### Traversals

A **traversal** generalizes lenses to focus on multiple parts of a data structure simultaneously. It can:

- **View all**: Extract all focused values as a list
- **Transform all**: Apply a function to all focused values
- **Count**: Count the number of focused elements

```haskell
type Traversal' s a = forall f. Applicative f => (a -> f a) -> s -> f s
```

## The DBExp Data Type

```haskell
data DBExp
  = DBVar Int              -- De Bruijn variable
  | DBLam DBExp           -- Lambda abstraction
  | DBLamAnn DBType DBExp -- Type-annotated lambda
  | DBLet DBExp DBExp     -- Let expression
  | DBApp DBExp DBExp     -- Function application
  -- ... many more constructors for arithmetic, booleans, etc.
```

## Core Optics Implementation

### 1. The `dbVars` Traversal

**Purpose**: Focus on all `DBVar` indices throughout an expression tree.

```haskell
dbVars :: Traversal' DBExp Int
dbVars f (DBVar i) = DBVar <$> f i
dbVars f (DBLam e) = DBLam <$> dbVars f e
dbVars f (DBLamAnn t e) = DBLamAnn t <$> dbVars f e
-- ... cases for all constructors that contain sub-expressions
dbVars _ e = pure e  -- Base cases (constants, etc.)
```

#### Understanding the Syntax

The syntax can be confusing at first, so let's break it down step by step:

**Basic Variable Case:**

```haskell
dbVars f (DBVar i) = DBVar <$> f i
```

- When we encounter a `DBVar i` (a De Bruijn variable with index `i`)
- Apply function `f` to the index `i`
- Wrap the result back in `DBVar` using `<$>`
- Example: If `f` adds 1, then `DBVar 3` becomes `DBVar 4`

**Composite Expression Case:**

```haskell
dbVars f (DBAdd e1 e2) = DBAdd <$> dbVars f e1 <*> dbVars f e2
```

- For addition `DBAdd e1 e2`, we need to process both sub-expressions
- Apply `dbVars f` recursively to `e1` and to `e2`
- The `<*>` combines multiple results back into `DBAdd`

**Base Cases:**

```haskell
dbVars _ e = pure e
```

- For constants (like `DBTrue`, `DBZero`), do nothing
- `pure e` means "return `e` unchanged"

#### Step-by-Step Example

Let's trace through a complete example:

```haskell
-- Original expression: let x = 0 in x + 1
-- In De Bruijn form: DBLet DBZero (DBAdd (DBVar 0) (DBSuc DBZero))

-- Apply: dbVars (+10) to this expression
dbVars (+10) (DBLet DBZero (DBAdd (DBVar 0) (DBSuc DBZero)))

-- Step by step:
-- 1. DBLet case: dbVars f (DBLet e body) = DBLet <$> dbVars f e <*> dbVars f body
-- 2. Process DBZero: dbVars (+10) DBZero = pure DBZero (unchanged)
-- 3. Process (DBAdd (DBVar 0) (DBSuc DBZero)):
--    - DBAdd case: DBAdd <$> dbVars f e1 <*> dbVars f e2
--    - Process DBVar 0: DBVar <$> (+10) 0 = DBVar 10
--    - Process DBSuc DBZero: DBSuc <$> dbVars f DBZero = DBSuc DBZero (unchanged)
-- 4. Result: DBLet DBZero (DBAdd (DBVar 10) (DBSuc DBZero))
```

#### Visual Mental Model

Think of `dbVars` as a robot walking through your expression tree:

```
DBLet DBZero (DBAdd (DBVar 0) (DBSuc DBZero))
├── DBZero           ← Robot: "Not a variable, skip"
└── DBAdd            ← Robot: "Go deeper"
    ├── DBVar 0      ← Robot: "Found variable! Apply function!"
    └── DBSuc        ← Robot: "Go deeper"
        └── DBZero   ← Robot: "Not a variable, skip"
```

#### The Magic Behind `<$>` and `<*>`

These operators come from **Applicative Functors**:

- `<$>` = "apply this function to the value inside a wrapper"
- `<*>` = "combine multiple wrapped values"

```haskell
-- This pattern:
DBAdd <$> dbVars f e1 <*> dbVars f e2

-- Is equivalent to:
do
  new_e1 <- dbVars f e1
  new_e2 <- dbVars f e2
  return (DBAdd new_e1 new_e2)
```

#### Powerful Usage Examples

Once defined, `dbVars` enables many operations:

```haskell
-- Extract all variable indices
getAllVarIndices :: DBExp -> [Int]
getAllVarIndices = toListOf dbVars

-- Count variables in an expression
countVars :: DBExp -> Int
countVars = lengthOf dbVars

-- Check if expression has any variables
hasVars :: DBExp -> Bool
hasVars = has dbVars

-- Find maximum variable index
maxIndex :: DBExp -> Maybe Int
maxIndex = maximumOf dbVars

-- Transform all variable indices
incrementAllVars :: DBExp -> DBExp
incrementAllVars = dbVars %~ (+1)

-- Set all variables to 0
zeroAllVars :: DBExp -> DBExp
zeroAllVars = dbVars .~ 0
```

#### Why Better Than Manual Pattern Matching?

1. **Less Error-Prone**: Add a new expression type? Just add one line to `dbVars`
2. **More Readable**: `expr & dbVars %~ (+1)` clearly says "increment all variables"
3. **Mathematically Sound**: Follows lens laws, guarantees consistent behavior
4. **Performance**: Often faster than manual recursion due to fusion optimizations

### 2. The `shiftLens` Function

**Purpose**: Implement De Bruijn index shifting with proper scoping rules.

```haskell
shiftLens :: Int -> Int -> DBExp -> DBExp
shiftLens n k (DBVar i)
  | i >= n = DBVar (i + k)
  | otherwise = DBVar i
shiftLens n k (DBLam e) = DBLam (shiftLens (n + 1) k e)
shiftLens n k (DBLamAnn t e) = DBLamAnn (shiftType n k t) (shiftLens (n + 1) k e)
shiftLens n k (DBLet e body) = DBLet (shiftLens n k e) (shiftLens (n + 1) k body)
shiftLens n k e = e & dbVars %~ (\i -> if i >= n then i + k else i)
```

#### Understanding De Bruijn Scoping

First, let's understand why scoping matters with a concrete example:

```haskell
-- Original: λx. λy. x + y
-- De Bruijn: λ. λ. 1 + 0
--            ^    ^  ^   ^
--            |    |  |   └─ y (0 steps to binding)
--            |    |  └───── x (1 step to binding)
--            |    └──────── binds y
--            └─────────────── binds x
```

If we want to shift this by 1 starting from cutoff 0:

- Variables ≥ 0 should be shifted
- BUT we need to be careful about **which variables are bound vs free**

#### Breaking Down the Code

**Direct Variable Case:**

```haskell
shiftLens n k (DBVar i)
  | i >= n = DBVar (i + k)
  | otherwise = DBVar i
```

- If we find a variable with index `i`
- If `i >= n` (at or above cutoff), shift it by `k`
- Otherwise, leave it alone

**Lambda Cases (The Tricky Part):**

```haskell
shiftLens n k (DBLam e) = DBLam (shiftLens (n + 1) k e)
shiftLens n k (DBLamAnn t e) = DBLamAnn (shiftType n k t) (shiftLens (n + 1) k e)
```

**Why `n + 1`?** Because lambda introduces a new binding! When you go "inside" a lambda, you're one level deeper in the binding stack.

**Let Expression:**

```haskell
shiftLens n k (DBLet e body) = DBLet (shiftLens n k e) (shiftLens (n + 1) k body)
```

**Why different treatment?**

- `e` (the value): use original cutoff `n`
- `body`: use `n + 1` because `let` introduces a new binding

**Everything Else (The Lens Magic):**

```haskell
shiftLens n k e = e & dbVars %~ (\i -> if i >= n then i + k else i)
```

- For all other cases (arithmetic, booleans, etc.)
- Use our `dbVars` traversal to find all variables
- Apply the shifting logic to each one

#### Complete Example

Let's trace through a complex example:

```haskell
-- Expression: λ. (1 + 0)  (lambda with free var 1, bound var 0)
-- Goal: shift with cutoff 0, amount 2

shiftLens 0 2 (DBLam (DBAdd (DBVar 1) (DBVar 0)))

-- Step 1: Hit the lambda case
-- shiftLens n k (DBLam e) = DBLam (shiftLens (n + 1) k e)
-- Becomes: DBLam (shiftLens 1 2 (DBAdd (DBVar 1) (DBVar 0)))

-- Step 2: Process the body with NEW cutoff (1)
-- shiftLens 1 2 (DBAdd (DBVar 1) (DBVar 0))
-- This hits the "everything else" case

-- Step 3: dbVars finds both variables and applies the function:
-- - DBVar 1: 1 >= 1, so 1 + 2 = 3 → DBVar 3
-- - DBVar 0: 0 < 1, so unchanged → DBVar 0

-- Final result: DBLam (DBAdd (DBVar 3) (DBVar 0))
```

#### Why This Hybrid Approach?

**Pure Pattern Matching (Old Way):**

```haskell
-- Would need 50+ lines like this:
shift n k (DBAdd e1 e2) = DBAdd (shift n k e1) (shift n k e2)
shift n k (DBMul e1 e2) = DBMul (shift n k e1) (shift n k e2)
-- ... endless boilerplate
```

**Pure Lens Approach (Too Simple):**

```haskell
-- This would be WRONG - doesn't handle scoping!
badShift n k = dbVars %~ (\i -> if i >= n then i + k else i)
```

**Hybrid Approach (Best of Both):**

- **Pattern matching** for cases that need special scoping logic (`DBLam`, `DBLet`)
- **Lenses** for cases that don't need special logic (arithmetic, etc.)

#### Visual Mental Model

Think of the shifting process like walking through a building:

```
DBLam (DBAdd (DBVar 1) (DBVar 0))
│
├─ "I'm entering a lambda - security level increased!"
│  (cutoff goes from 0 to 1)
│
└─ DBAdd (DBVar 1) (DBVar 0)
   │
   ├─ DBVar 1: "Index 1 >= cutoff 1? YES! Shift to 3"
   │
   └─ DBVar 0: "Index 0 >= cutoff 1? NO! Leave alone"
```

#### Syntax Breakdown

```haskell
e & dbVars %~ (\i -> if i >= n then i + k else i)
```

**Reading left to right:**

1. `e` - start with expression `e`
2. `&` - pipe it into the next operation
3. `dbVars` - focus on all variables using our traversal
4. `%~` - modify each focused value with the following function
5. `(\i -> if i >= n then i + k else i)` - anonymous function that shifts if needed

**This is equivalent to:**

```haskell
mapAllVariables (\i -> if i >= n then i + k else i) e
```

#### Why This is Brilliant

1. **Correctness**: Handles De Bruijn scoping rules properly
2. **Efficiency**: Only pattern matches on the cases that matter
3. **Maintainability**: Adding new expression types only requires updating `dbVars`
4. **Readability**: The intent is clear from the structure

**Before (46 lines):** Every case manually handled with repetitive pattern matching
**After (5 lines):** Scoping cases handled explicitly, everything else handled generically

### 3. Component Lenses

**Purpose**: Provide safe access to parts of composite expressions.

```haskell
lamAnnType :: Lens' DBExp DBType
lamAnnType = lens get set
  where
    get (DBLamAnn t _) = t
    get _ = error "Not a DBLamAnn"
    set (DBLamAnn _ e) t = DBLamAnn t e
    set _ _ = error "Not a DBLamAnn"
```

**Usage**:

```haskell
-- Extract type from type-annotated lambda
getType :: DBExp -> DBType
getType expr = expr ^. lamAnnType

-- Update type in type-annotated lambda
updateType :: DBType -> DBExp -> DBExp
updateType newType expr = expr & lamAnnType .~ newType
```

### 4. The `typeVars` Traversal

**Purpose**: Focus on type variables within type expressions.

```haskell
typeVars :: Traversal' DBType Int
typeVars f (DBTVar i) = DBTVar <$> f i
typeVars f (DBTFun a b) = DBTFun <$> typeVars f a <*> typeVars f b
-- ... other type constructors
typeVars _ t = pure t
```

## Lens Operations and Combinators

### Viewing (`^.`, `toListOf`)

```haskell
-- Get a single value through a lens
expr ^. lamAnnType

-- Get all values through a traversal
toListOf dbVars expr
```

### Setting (`&`, `.~`, `%~`)

```haskell
-- Set a value through a lens
expr & lamAnnType .~ newType

-- Modify a value through a lens/traversal
expr & dbVars %~ (+1)
```

### Querying (`lengthOf`, `has`)

```haskell
-- Count elements focused by a traversal
lengthOf dbVars expr

-- Check if traversal focuses on anything
has dbVars expr
```

## Benefits of the Lens-Based Approach

### 1. **Dramatic Code Reduction**

The original `shift` function was 46 lines of repetitive pattern matching. The lens-based version is effectively 5 lines for the core logic.

**Before**:

```haskell
shift n k (DBAdd e1 e2) = DBAdd (shift n k e1) (shift n k e2)
shift n k (DBMul e1 e2) = DBMul (shift n k e1) (shift n k e2)
shift n k (DBNot e) = DBNot (shift n k e)
-- ... 40+ more lines of similar boilerplate
```

**After**:

```haskell
shiftLens n k e = e & dbVars %~ (\i -> if i >= n then i + k else i)
```

### 2. **Better Maintainability**

Adding new expression constructors requires updating only the `dbVars` traversal, not multiple functions.

### 3. **Composability**

Lenses and traversals compose naturally:

```haskell
-- Focus on variables in the body of type-annotated lambdas
lamAnnBody . dbVars :: Traversal' DBExp Int

-- Transform variables in lambda bodies
expr & lamAnnBody . dbVars %~ (+1)
```

### 4. **Reusability**

The `dbVars` traversal can be used for many different operations:

- Shifting indices
- Substitution
- Counting variables
- Extracting all variables
- Checking if an expression is closed (no free variables)

### 5. **Type Safety**

Lenses provide compile-time guarantees about the structure you're accessing, and lens laws ensure consistent behavior.

## Advanced Examples

### Variable Substitution Using Lenses

```haskell
-- Count free variables in an expression
countFreeVars :: DBExp -> Int
countFreeVars = lengthOf dbVars

-- Check if expression is closed (no free variables)
isClosed :: DBExp -> Bool
isClosed = hasn't dbVars  -- hasn't = not . has

-- Get maximum variable index (useful for generating fresh variables)
maxVarIndex :: DBExp -> Maybe Int
maxVarIndex expr = maximumOf dbVars expr
```

### Combining Traversals

```haskell
-- Focus on all variables in all type annotations
allTypeVars :: Traversal' DBExp Int
allTypeVars = cosmos . _DBLamAnn . _1 . typeVars
  where
    _DBLamAnn = prism DBLamAnn $ \case
      DBLamAnn t e -> Right (t, e)
      other -> Left other
```

## Performance Considerations

### When to Use Lenses vs. Pattern Matching

**Use lenses when**:

- You need to focus on many similar parts of a structure
- You want composable operations
- The operation is naturally expressed as a transformation

**Use pattern matching when**:

- You need precise control over scoping (like in `shiftLens`)
- You're handling fundamentally different cases
- Performance is absolutely critical (though lens overhead is usually minimal)

### Memory and Time Complexity

- **Traversals**: O(n) time where n is the size of the focused data
- **Lenses**: O(1) time for getting/setting
- **Memory**: Lenses don't add significant memory overhead

## Conclusion

The lens-based approach in `DeBruijn.hs` demonstrates how optics can dramatically simplify code while maintaining correctness and improving maintainability. Key takeaways:

1. **Traversals** like `dbVars` eliminate boilerplate by focusing on similar elements across complex data structures
2. **Lenses** like `lamAnnType` provide safe, composable access to data components
3. **Hybrid approaches** combine the power of lenses with explicit logic where needed
4. **The lens ecosystem** provides powerful combinators for querying, transforming, and composing operations

This approach transforms what was previously verbose, error-prone boilerplate into concise, reusable, and mathematically principled code.

## Key Insights

The lens-based DeBruijn implementation demonstrates several important principles:

1. **Know When to Use Each Tool**: Pattern matching for complex logic (scoping), lenses for repetitive operations (variable transformation)

2. **Traversals Eliminate Boilerplate**: Instead of writing 50+ lines of identical pattern matching, define the traversal once and reuse it everywhere

3. **Hybrid Approaches Work**: You don't have to choose between lenses OR pattern matching - combine them intelligently

4. **Mathematical Foundations Matter**: Lens laws ensure consistent behavior and enable powerful optimizations

5. **Readability Through Abstractions**: `expr & dbVars %~ (+1)` is more readable than recursive pattern matching

This implementation serves as an excellent example of how functional programming abstractions can make complex, domain-specific code both simpler and more powerful.
