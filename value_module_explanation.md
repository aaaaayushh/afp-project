# Understanding the Value.hs Module

## Overview

The `Value.hs` module is **absolutely crucial** for the dependent type checker because it defines the **runtime representation** of values - essentially, what expressions evaluate to when the program actually runs. This module bridges the gap between syntax (what you write) and semantics (what it means when executed).

## The Core Problem This Module Solves

Without `Value.hs`, you'd have:

- ✅ **Syntax**: Expressions like `DBAdd (DBVar 0) (DBSuc DBZero)`
- ❌ **No Semantics**: No way to actually compute what these expressions mean

With `Value.hs`, you get:

- ✅ **Syntax**: `DBAdd (DBVar 0) (DBSuc DBZero)`
- ✅ **Semantics**: This evaluates to `VNat (Suc (Suc Zero))` (which represents 2)

---

## The `Value` Data Type - Runtime Representations

```haskell
data Value
  = VNat Nat      -- Natural number values
  | VBool Bool    -- Boolean values
  | VLam Closure  -- Function values (Phase 0 Goal 4)
  | VU            -- Universe value (Phase 1)
```

This represents the **four fundamental kinds of values** that can exist at runtime:

### `VNat Nat` - Natural Number Values

**Why needed**: Phase 0 Goal 1 requires converting from integers to natural numbers

**What it represents**: Actual natural number values like 0, 1, 2, 3... that exist during program execution

**Example**:

```haskell
-- Source code: suc (suc zero)
-- Evaluates to: VNat (Suc (Suc Zero))
-- Represents: The number 2
```

### `VBool Bool` - Boolean Values

**Why needed**: The language supports boolean operations and conditionals

**What it represents**: Runtime boolean values `true` and `false`

**Example**:

```haskell
-- Source code: true && false
-- Evaluates to: VBool False
-- Represents: The boolean value false
```

### `VLam Closure` - Function Values

**Why needed**: Phase 0 Goal 4 requires lambda expressions as first-class values

**What it represents**: Functions that can be:

- Passed as arguments to other functions
- Returned from functions
- Stored in variables and data structures

**Critical insight**: This makes functions "first-class citizens" - they're values just like numbers and booleans.

**Example**:

```haskell
-- Source code: \x -> x + 1
-- Evaluates to: VLam (DBFun (DBAdd (DBVar 0) (DBSuc DBZero)))
-- Represents: A function that adds 1 to its argument
```

### `VU` - Universe Value

**Why needed**: Phase 1 requires a universe type where types are values

**What it represents**: The result of evaluating type expressions like `nat`, `bool`, `U`, `(nat -> bool)`

**Example**:

```haskell
-- Source code: nat (used as a type expression)
-- Evaluates to: VU
-- Represents: The natural number type as a value
```

---

## The `Nat` Data Type - Peano Arithmetic

```haskell
data Nat
  = Zero      -- Represents 0
  | Suc Nat   -- Represents n+1
```

### Why This Specific Representation?

1. **Theoretical Foundation**: This is Peano arithmetic - the standard mathematical definition of natural numbers in type theory

2. **Constructive**: Every natural number is built explicitly from `Zero` and `Suc` operations

   - `Zero` = 0
   - `Suc Zero` = 1
   - `Suc (Suc Zero)` = 2
   - `Suc (Suc (Suc Zero))` = 3
   - And so on...

3. **Dependent Types Compatibility**: This representation works perfectly with dependent types because:
   - **Pattern matching is decidable**: You can always determine the structure
   - **Structural recursion is well-founded**: Recursion always terminates
   - **Type-level computation**: Types can reason about the structure of numbers

### Contrast with Built-in Integers

| Peano Nat            | Built-in Integer         |
| -------------------- | ------------------------ |
| `Zero`               | `0`                      |
| `Suc Zero`           | `1`                      |
| `Suc (Suc Zero)`     | `2`                      |
| Infinite precision   | Fixed precision          |
| Structural           | Atomic                   |
| Type-theory friendly | Implementation dependent |

---

## The `Closure` Data Type - Function Representations

```haskell
data Closure
  = DBFun DBExp    -- Anonymous function closures
  | NamedFun Ident -- Named function references
```

### Why Two Different Kinds of Closures?

#### `DBFun DBExp` - Anonymous Function Closures

**Represents**: Lambda expressions like `\x -> x + 1`

**How it works**:

- Stores the function body as a De Bruijn expression
- No need to store environment - De Bruijn indices handle variable lookup
- When applied, extends the environment and evaluates the body

**Example**:

```haskell
-- Lambda: \x -> x * 2
-- Closure: DBFun (DBMul (DBVar 0) (DBSuc (DBSuc DBZero)))
-- Application: Extend environment with argument, evaluate body
```

#### `NamedFun Ident` - Named Function References

**Represents**: Named functions defined with `fun f(x) = ...`

**Why separate from DBFun**:

- Named functions can be **recursive** - they need to reference themselves by name
- Avoids complex environment manipulation for recursion
- Keeps the implementation simple and clean

**Example**:

```haskell
-- Definition: fun factorial(n) = if n == 0 then 1 else n * factorial(n-1)
-- Closure: NamedFun "factorial"
-- Application: Look up function by name, then apply like DBFun
```

---

## Why This Module is Essential

### 1. Separation of Syntax and Semantics

| Aspect      | Syntax (`DBExp`)                 | Semantics (`Value`)     |
| ----------- | -------------------------------- | ----------------------- |
| **Purpose** | What you write                   | What expressions mean   |
| **Example** | `DBAdd (DBVar 0) (DBSuc DBZero)` | `VNat (Suc (Suc Zero))` |
| **Nature**  | Structural                       | Computational           |
| **Usage**   | Parsing, transformation          | Execution, results      |

### 2. Type Safety at Runtime

Values carry their types implicitly:

```haskell
-- Type safe - can only contain natural numbers
VNat :: Nat -> Value

-- Type safe - can only contain booleans
VBool :: Bool -> Value

-- Type safe - can only contain function closures
VLam :: Closure -> Value
```

This **prevents runtime type errors** - you can't accidentally treat a number as a boolean.

### 3. Higher-Order Functions Support

`VLam` makes functions into first-class values, enabling:

**Functions as arguments**:

```haskell
-- map :: (a -> b) -> [a] -> [b]
-- The first argument is a VLam value
```

**Functions as return values**:

```haskell
-- curry :: ((a, b) -> c) -> a -> b -> c
-- Returns a VLam that returns another VLam
```

**Functions in data structures**:

```haskell
-- [(\x -> x + 1), (\x -> x * 2), (\x -> x - 1)]
-- List containing VLam values
```

### 4. Dependent Types Support

`VU` enables types to be computed values:

```haskell
-- Type expressions evaluate to VU
nat :: Value          -- VU
bool :: Value         -- VU
(nat -> bool) :: Value -- VU

-- This enables dependent function types like:
-- (n : nat) -> Vec A n
-- where the return type depends on the value of n
```

---

## Helper Functions - Practical Convenience

```haskell
toNat :: Integer -> Nat
toNat 0 = Zero
toNat n | n > 0 = Suc (toNat (n - 1))
toNat _ = error "Cannot convert negative number to Nat"

vnat :: Integer -> Value
vnat = VNat . toNat
```

**Purpose**:

- **Bridge**: Converts between Haskell's built-in integers and the custom `Nat` representation
- **Testing**: Makes it easy to create test values: `vnat 5` instead of `VNat (Suc (Suc (Suc (Suc (Suc Zero)))))`
- **Debugging**: Simplifies manual value construction during development
- **Safety**: Prevents negative numbers with runtime checks

---

## Complete Example: From Syntax to Value

Here's how an expression flows through the entire system:

### Step-by-Step Transformation

1. **Source Code**: `\x -> suc x`

2. **Parse Tree**: `ELam "x" (ESuc (EVar "x"))`

3. **De Bruijn Form**: `DBLam (DBSuc (DBVar 0))`

4. **Runtime Value**: `VLam (DBFun (DBSuc (DBVar 0)))`

5. **Function Application** (apply to 2):
   ```haskell
   -- Argument: VNat (Suc (Suc Zero))  -- represents 2
   -- Extend environment: [VNat (Suc (Suc Zero))]
   -- Evaluate body: DBSuc (DBVar 0)
   -- Result: VNat (Suc (Suc (Suc Zero)))  -- represents 3
   ```

### The Role of Value.hs

**Without Value.hs**: You'd stop at step 3 - you'd have syntax but no way to compute

**With Value.hs**: Step 4 enables actual computation and step 5 shows the result

---

## Design Principles Embodied

### 1. **Compositionality**

Each value type corresponds to a syntactic category:

- Natural number expressions → `VNat`
- Boolean expressions → `VBool`
- Lambda expressions → `VLam`
- Type expressions → `VU`

### 2. **Type Safety**

Values are intrinsically typed - impossible to confuse a number with a function

### 3. **Simplicity**

Clean, minimal representation that captures exactly what's needed for computation

### 4. **Extensibility**

Easy to add new value types (like pairs, sums, etc.) following the same pattern

---

## Conclusion

The `Value.hs` module is the **computational heart** of the dependent type checker. Without it:

- ❌ Expressions would be just syntax trees
- ❌ No way to actually run programs
- ❌ No function application or computation
- ❌ No support for dependent types

With it:

- ✅ Expressions become computable values
- ✅ Programs can actually execute and produce results
- ✅ Functions are first-class values (Phase 0 Goal 4)
- ✅ Types can be computed as values (Phase 1)
- ✅ Foundation for dependent type checking

**Bottom line**: `Value.hs` transforms a static syntax tree into a dynamic, executable programming language.
