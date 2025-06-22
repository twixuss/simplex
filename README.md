# Simplex
---
# 🚧 This project is unfinished 🚧
## Stuff in this readme that is incomplete or not implemented is marked with 🚧
---

# Syntax
## Comments
They are just like in C:
```simplex
// Single line comment

/* Multiline comment */
```
Except nestable:
```simplex
/* /* Nested multiline comment */ */
```
And aware of strings:
```simplex
/*
const str = "some */ text"
*/
```
## Whitespace, semicolons and separators
Spaces and tabs are not significant, newlines are. Semicolons are optional (required for multiple statements on a single line). Keywords `then` and `do` separate condition and body when both are on single line.

<details><summary><b>Separators example</b></summary>
Here `*` could mean dereference or multiplication:

<!--
"<pre data-role="codeBlock" data-info="simplex" class="language-simplex simplex"><code>if condition  *pointer = 42
</code></pre>"
-->

```simplex
if condition  *pointer = 42
```

To disambiguate you can:
* Use `then` keyword:
  ```simplex
  if condition then *pointer = 42
  ```
* Use braces:
  ```simplex
  if condition { *pointer = 42 }
  ```
* Put body on new line:
  ```simplex
  if condition
      *pointer = 42
  ```
</details>

## Expressions
### Primary
#### Names (Identifiers)
A sequence of characters that starts with `_` or `a-Z`, and continues with `_` or ` ` (space, experimental) or `a-Z` or `0-9`.
*I'm experimenting with spaces in names. For example `hello world` is a single identifier, like `hello_world`. If there are multiple spaces between names, they are not merged and treated as two separate tokens.*
#### None literal
`none`
A value that represents nothing. 
Has type [`None`](#none), which is like `void` in C.
It is implicitly convertible to:
* Pointers
* 🚧 Options

🚧 Explicitly it is convertible to any type, resulting in all bytes being set to zero, equivalent to `memset(&value, 0, sizeof(value))`.

---
#### Boolean Literal
`true`
`false`

Type: [`Bool`](#bool).

---
#### Integer Literal
`69` - decimal
`069` - invalid, use `0o` for octal
`0x45` - hexadecimal
`0o105` - octal
`0b0100_0101` - binary

Use underscore `_` as a separator.

Type: [`UnsizedInteger`](#unsizedinteger), which is implicitly convertible to any sized integer type if there is no data loss.

---
#### String literal
```simplex
"Hello, world!\n"
```
Type: [`String`](#string).
Enclosed in double quotes `"`. Escaped with backslash `\`. Supports inline bytes using hex `\0xFF`, octal `\0o377` or binary `\0b11111111`. Terminates only when encoutering unescaped double quote `"`, meaning that multiline literals just work. Line endings are not changed, so they are whatever is in your source file.
<i>
* 🚧 should they always be \n?
* 🚧 user-defined line endings might be useful?
* 🚧 custom begin/end tokens
</i>

---
#### Character literal
Character literal is a utf8 string decoded into a sequence of utf32 code points that is then compressed into a single integer.
For example:
```simplex
'a'  == 97
'z'  == 122
'az' == 31329 == 122 * 256 + 97 // little endian
'😀' == 0x1f600
```

---
#### Definition
```simplex
var x = 42
const y: Int = 690
let condition = true
```
`: Type` is optional when initial value is provided.
`var` - runtime variable.
`let` - runtime immutable.
`const` - compile time constant.

Definitions are *expressions*, meaning you can have them as function outputs: 
```simplex
let pixels = stbi_load("image.png", &var width: Int, &var height: Int, 0, 4);
// use width and height here
```
<details><summary>Notes</summary>
🚧 Would be cool to be able to omit types here:
<pre><code>
let pixels = stbi_load("image.png", &var width, &var height, 0, 4);
</code></pre>
🚧 You can use let with autocast instead of var:
<pre><code>
let pixels = stbi_load("image.png", @&let width, @&let height, 0, 4);
</code></pre>
</details>

🚧 Or in `if` conditions:
```simplex
if let found = find(array, needle) {
    println(*found)
}
```

##### Shorthands
Definitions that closely represent the AST look like this:
```simplex
const my_function = fn () {}
```
This syntax is a bit cumbersome, so there is an easier way:
```simplex
fn my_function() {}
```
This is transformed into the version above, so they are identical.
This works for:
* `fn`
* `struct`
* `enum`

---
#### Block
Block is a list of statements, last of which can be an expression. Statements are executed in order. Value of last expression is the result.
```simplex
{
    statement1;
    statement2;
    result_expression;
}
```
If last statement of a block is an expression, the block itself becomes an expression:
```simplex
let foo = {
    println("something")
    42
}
```
Here "something" is printed, then `42` is assigned to `foo`.

---
#### If Expression
```simplex
var result = if condition then true_branch() else false_branch()
```
If expression must have both branches, and both of them must be expressions, not statements. Branches' type must be the same.

---
#### Match expression
Table from value to code
```
var color_string = match color {
    0 => "black"
    1 => "white"
    else => "unknown"
}
```
`match` is mostly eqivalent to a series of `if`s:
```
var color_string = {:a
    if color == 0 then break :a "black"
    if color == 1 then break :a "white"
    "unknown"
}
```
In some cases compiler may introduce a jump table.

Match expression yields a value. If there's no matching case, and no default case, `debug_break()` is executed, which will stop the program.

---
#### Lambda Head
```simplex
fn (arg: Type): Ret
```
Lambda head is a type. It has parameters and return type. If you follow LambdaHead with a body it turns into a Lambda.

---
#### Lambda
```simplex
fn (arg: Type): ReturnType => body
fn (arg: Type) => body             // return type is inferred from body
```
Lambda's type is its head. Lambda's body is executed when you call it. Any expression can be the body.
<i>
🚧 What should the result type of `typeof lambda`?
1. Generic function type, to which any lambda with matching signature can be implicitly casted.
1. Unique function type that only that lambda has.

Some examples:
```
fn foo(x: Int): Int => {...}
fn eval(func: typeof foo, x: Int) => func(x)
eval(foo, 42)
```
Here you probably want the first option so eval can accept lambdas other than foo.

```simplex

fn foo(x: Int): Int
fn eval[T](func: T, x: Int) => func(x)
eval(foo, 42)
```
Here you probably want the second option so inlining works
...
though that might be solved by constant propagation.
probably just stick with first one. idk.
</i>

---
#### Unary
##### Autocast
```simplex
@expression
```
Automatically cast expression to the required type. That type should be inferrable from the context, e.g. assigning to a variable with a known type, passing as an argument to a function, etc. Ignored in cases where it does not make sense.
```simplex
var a: Int = @get_uint()
// becomes
var a: Int = get_uint() as Int
```
```simplex
var a = @get_uint()
// here the type of a is unknown yet when performing autocast.
// it is ignored and a gets the type of get_uint()
```
```simplex
fn foo(x: Int) {...}
let f = 1.5

foo(@f)
// becomes
foo(f as Int)
```
---
## Statements
### If Statement
```simplex
if condition {
    true_branch()
} else {
    false_branch()
}
```
Unlike if expression, branches of if statement can be either statements or expressions. `else` branch can be omitted.

---
### Match statement
```simplex
match thing {
    0 => {
        println("Thing is zero")
    }
    1 => {
        // do some stuff
    }
}
```
Match statements are not required to be complete: if a case is missing, nothing will happen.

---
### While loop
```simplex
while condition then body

while true {
    stuff()
}
```

---
### For loop
```simplex
for i in 0..10 {
    println(i)
}
```
For only accepts ranges currently. 🚧 There will be a way to iterate custom types later.

Iterate in reverse by putting contextual keyword `reverse` before the range:
```simplex
for j in reverse 0..10
```

For is desugared into a while loop. Enable `-print-ast` to see that:
```simplex
for i in 0..10 {
    println(i)
}
/////////////////
{
    let __range = 0..10
    let __i = __range.begin
    while __i < __range.end {
        let i = __i
        {
            println(i)
        }
        __i += 1
    }
}
```

# Types
## `Type`
The type of all types.
🚧 Values of type `Type` store information about some type, e.g. name, size, alignment, members, arguments etc.

---
## `None`
The type of `none` literal. It is like `void` in C.
Lambdas that do not return anything have this return type.

---
## `Bool`
May have one of two values: `true` or `false`

---
## `U8` / `U16` / `U32` / `U64` / `S8` / `S16` / `S32` / `S64`
Two's complement integer of N bits.

Implicit conversions are allowed if data can't be lost.

Addition and subtraction overflow are wrapping for both signed and unsigned.

Multiplication overflow is wrapping as well.

Division is euclidean:
```
x / 3    ^
         |     @@@
         |  @@@
---------@@@------>
      @@@|        x
   @@@   |
@@@      |
```
```
x % 3
         ^
  @  @  @| @  @  @
 @  @  @ |@  @  @
@--@--@--@--@--@-->
         |        x
```
If divisor is negative, remainder stays the same, quotient switches sign.
`x/3*3 + x%3 == x`



---
## `String`
A structure like this:
```simplex
const String = struct {
    data: *U8
    count: Int
}
```

---
## `Range`
Currently `Range` only supports Ints.
```simplex
struct Range {
    begin: Int
    end: Int
}
```
Construct it using `..`
```simplex
var range = 0..10 // 0 inclusive, 10 exclusive
```
You can use them in for loops:
```simplex
for i in 0..10 {
    println(i)
}
```
### 🚧 Generic Range
```simplex
const Range = struct [T: Type] {
    begin: T
    end: T
}
```

Ranges can represent a consecutive set of integers:
```simplex
var range: Range[Int] = 0..10  // 10 is not included
for it in range do println(it) // prints 0 1 2 ... 9
```
or a memory span:
```simplex
var string = .["hello", "world"]
var span: Range[*String] = strings
```
```simplex
fn get_count[T: Type](range: Range[T]) {
    range.end - range.begin
}
```
```simplex
const String = Range[*U8]
```

---
## Arrays
```simplex
[count]Int
```
`count` must be `const`. Arrays implicitly convert to [ranges](#range)

---
## Pointers
```simplex
*String
```

---
## Function types
```simplex
fn (a: Arg1, b: Arg2): ReturnType
```

---
## Aliases
`Int` = `S64`
`UInt` = `U64`

## Internal
### `UnsizedInteger`
A signed 64-bit number
# Bag of sugar
## Properties
You can have member-like access for stuff that has to be computed:
1. Define a function with a name that starts with `get_` and accepts wanted type:
```simplex
fn get_length(v: Vector3) => sqrt(v.x*v.x + v.y*v.y + v.z*v.z)
```
2. Access it like a struct member:
```simplex
println(v.length)
```
To write to a property, similarly define a `set_` function that takes a mutable pointer and a value to set:
```simplex
fn set_length(v: *var Vector3, l: Float) {
    *v = normalize(*v) * l;
}
```
Then you can assign to `length`:
```simplex
vector.length *= 2

// Transforms into

set_length(&vector, get_length(vector) * 2)
```

### Multi property access
Suppose `xz` is another property:
```simplex
vector.xz.length *= 2

// Transforms into

var xz = get_xz(vector)
set_length(&xz, get_length(xz) * 2)
set_xz(&vector, xz)
```
## Importing struct members
You can implicitly access members of a structure by prefixing a definition with `use`
```simplex
fn length(use v: Vector2) {
    return x*x + y*y
}
```
Or you can do that later with `use` statement:
```simplex
fn length(v: Vector2) {
    // here you must do v.x and v.y
    use v
    // now members are imported
    return x*x + y*y
}
```
# Unsorted stuff
## Pointer coalescion
When using logical or operator || on pointers, the resulting value is the first one that is not null. The rest is not executed, just like in logical operations on booleans. If all of the pointers are null, the result is null.
```simplex
let a = 1
let b = 2

let x = &a
let y = &b
let z: *Int = 0

x || y  // x
y || x  // y
x || z  // x
z || x  // x
z || z  // 0
```