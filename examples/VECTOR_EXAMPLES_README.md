# Vector Type Examples

This directory contains examples showcasing the vector type implementation in the dependently typed functional programming language. Each example demonstrates different aspects of vector operations and type safety.

## Examples Overview

### 1. `vector_basics.afp` - Basic Vector Construction and Access

```
val nums = zero :: (suc zero) :: (suc (suc zero)) :: [];
head (tail nums)
```

**Demonstrates:**

- Vector construction using cons operator (`::`)
- Smart type inference (no type annotation needed for `[]` in cons)
- Basic operations: `head` and `tail`
- Chaining operations: getting the second element via `head (tail ...)`

**Output:** `VNat (Suc Zero)` (the number 1)

### 2. `vector_smart_inference.afp` - Smart Type Inference

```
val single_zero = zero :: [];
val single_true = True :: [];
head single_zero
```

**Demonstrates:**

- Smart type inference for empty vectors in cons operations
- Different element types (nat and bool) inferred automatically
- No explicit type annotations needed

**Output:** `VNat Zero` (the number 0)

### 3. `vector_append.afp` - Vector Concatenation

```
val first = True :: False :: [];
val second = True :: [];
head (append first second)
```

**Demonstrates:**

- Vector append operation
- Type consistency checking (both vectors must have same element type)
- Length addition at type level: `Vector bool 2 + Vector bool 1 = Vector bool 3`

**Output:** `VBool True` (first element of concatenated vector)

### 4. `vector_complex.afp` - Complex Nested Operations

```
val nums1 = zero :: suc zero :: [];
val nums2 = suc (suc zero) :: suc (suc (suc zero)) :: [];
val combined = append nums1 nums2;
head (tail (tail combined))
```

**Demonstrates:**

- Multiple vector definitions
- Append operation with variable assignment
- Nested head/tail operations to access specific elements
- Complex length expressions from append operations

**Output:** `VNat (Suc (Suc Zero))` (the number 2, third element of combined vector)

### 5. `vector_dependent.afp` - Dependent Types with Variables

```
val singleton = suc zero :: [];
zero :: singleton
```

**Demonstrates:**

- Vector variables in expressions
- Type inference across variable references
- Dependent vector types working with existing values

**Output:** `VVec [VNat Zero,VNat (Suc Zero)]` (vector [0, 1])

### 6. `vector_conditional.afp` - Vectors with Conditional Logic

```
val bool_vec = True :: False :: True :: [];
val result_vec = if head bool_vec then suc zero :: [] else zero :: [];
head result_vec
```

**Demonstrates:**

- Vectors with boolean elements
- Conditional expressions with vectors
- Using vector elements in conditional tests
- Different vector constructions based on conditions

**Output:** `VNat (Suc Zero)` (the number 1, since first element is True)

### 7. `vector_lambda.afp` - Functions and Vectors

```
fun double (n : nat) = n * suc (suc zero);
val vec = suc (suc (suc zero)) :: [];
double (head vec)
```

**Demonstrates:**

- Function definitions working with vector elements
- Vector element extraction for function application
- Combining arithmetic operations with vector access

**Output:** `VNat (Suc (Suc (Suc (Suc (Suc (Suc Zero))))))` (the number 6, which is 3 \* 2)

### 8. `vector_type_annotation.afp` - Type Inference in Context

```
append [] (zero :: [])
```

**Demonstrates:**

- Empty vector type inference in append context
- How the type system infers `[]` type from the second argument
- Contextual type inference working across operations

**Output:** `VVec [VNat Zero]` (vector containing just 0)

## Key Features Demonstrated

### 1. Smart Type Inference

The implementation includes sophisticated type inference that allows:

- `element :: []` to work without explicit type annotation for the empty vector
- Empty vectors in append operations to infer their type from the other argument
- Contextual type inference across variable assignments

### 2. Type Safety

All examples demonstrate compile-time type safety:

- Vector element types must be consistent
- Head/tail operations are checked for non-empty vectors at compile time
- Append operations verify compatible vector types

### 3. Dependent Length Tracking

Vector lengths are tracked at the type level:

- `Vector nat (suc zero)` represents a vector of naturals with length 1
- Append operations produce vectors with computed lengths: `n + m`
- Type checker ensures head/tail safety based on length expressions

### 4. Integration with Language Features

Vectors integrate seamlessly with:

- Function definitions and applications
- Conditional expressions
- Variable assignments and references
- Arithmetic operations on vector elements

## Running the Examples

To run any example:

```bash
cabal run main examples/vector_basics.afp
```

Replace `vector_basics.afp` with any of the example files to see different vector operations in action.

## Error Cases

The type system prevents common errors:

- `head []` would be caught at compile time (when possible to determine)
- `append (True :: []) (zero :: [])` fails due to type mismatch
- Accessing elements beyond vector bounds is prevented through type-level length tracking

These examples showcase how dependent types enable both mathematical rigor and practical usability in vector operations.
