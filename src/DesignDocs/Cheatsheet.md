# Mountain Cheatsheet

## Whitespace

Whitespace like tabs, newlines, and spaces are insignificant

## Comments

```javascript
// is a line comment
```

```javascript
/*

This is a block comment

*/
```

## Literals

Any [builtin value](#builtin-types).

```javascript
3.0 
```
```javascript
4 
```
```javascript
"hello world" 
```
```javascript
{1,2,3}
```

etc.

## Definitions

Definitions are terms as well, and can be used anywhere in the code where a value can be used. They must end with a value, and can be chained. a semicolon must not be used on the last value. The return keyword is optional

```javascript
  x = 3; 
  y = 4; 
  return (x,y)
```

```javascript
  x = 3; 
  y = 4; 
  (x,y)
```
```javascript
(a = 3; a) + (a=4;a)
```
Both of the above would return 7.

The only place definitions may not appear is on the left side of an arrow.

Definitions can deconstruct labels and tuples

```javascript
(a,b) = ((1,2), 3);
(c,?) = a;
b + c //returns 5
```
```javascript
x:a = x:3;
a + 2 // returns 5
```

## Functions

Functions are first class objects

```javascript
3 -> 4
```

```javascript
x -> x
```

```javascript
(x,y) -> x + y
```

_ can match anything. Whatever is matched with a _ is not useable on the rhs of the arrow
```javascript
(_,_,x) -> x
```

```javascript
x = 3;
x -> x // returns 3 -> 3
```

```javascript
// Only matches objects of type Point:(x:Int, y:Int)
Point:(x:a,y:b) -> (a,b)
```

Functions can be chained multiple times

```javascript
3 -> 4 -> 5 -> 6 
```

This is equivalent to 
```javascript
3 -> (4 -> (5 -> 6))
```

[Type Annotations](#type-annotations) may be used to constrain the values of a input variable. [Usage Annotations](#usage-annotations) must also be followed.

```
(+Int :: x) -> (Int :: y) -> (Int, Int) :: (
  y = x + x;
  return (x, y)
)
```

## Match Statements

Matches are stacks of functions. They are functions themselves. When called the call the first function which matches the argument. 

```javascript
(
       3 -> 4
   ||  4 -> 5
   ||  5 -> 6
)(4) // will return 5

```

```javascript
(
      4       -> 5
   || "hello" -> "world"
)("hello") // will return "world"

```

## Function Call

to call a function you simply write two terms immediately following one another with optional whitespace. 
```javascript
incr3 = 3 -> 4;
incr3(3)
```
-- or -- 
```javascript
incr3 = 3 -> 4;
incr3 3
```
both will call incr3 with 3, returning 4 

## Type Annotations

take the form \<Type> :: \<Term>
```javascript
Int :: 4 
```
```javascript
(Int -> Int) :: 4 -> 5 
```
```javascript
((Float, Int) -> Float) :: (x, _) -> x 
```

If you use a variable more than once in the rhs of a function, you must mark it with a "+" in the type in the lhs of the ->

```
duplicate = +Int -> (Int, Int) :: x -> (x,x)
```

Type annotations can be variables, which make the function polymorphic. It can work with any type "a" <br>
```javascript
a -> a :: x -> x
```
For example the type of the above could be 
```
  Int           -> Int 
| Float         -> Float 
| (Int | Float) -> (Int | Float)
| ...
```
 but could not be 
 ```
 Int -> Float 
 ```
 since those are different types. 

```javascript
+a -> (a,a,Int) :: x -> (x,x,3)
```
The "+" would still need to be added 

types are automatically inferred in the language, but can be helpful to have as documentation, or in ambiguous cases.

## Modules & Intefaces

Modules are groups of data, types and functions.

All files in mountain are modules. Modules are maps of names to values and types. the keywords val and type are specifically only used for modules. 

```javascript
val tupleize = a -> b -> (a,b) :: x -> y -> (x,y);
type Numeric = Float | Int;
type Point = a -> b -> (x:a, y:b); // types can be parameterized
// Ex: Point Int Float would be a type like (x:Int, y:Float)
val a_three = 3.0;
```
Modules are sequential defs
```
val x = 3.0;
val y = x + 2.5;
val tuple = (x,y);
```
Modules are first class objects in Mountain. Nest modules like so
```
val module1 = <{
  val a = "something";
}>;
val module2 = <{
  val a = "something_else";
}>
```
Modules are selectable
```
val module1 = <{
  type TYPE_OF_X = Int
  val x = 2;
}>;
val an_x = module1.TYPE_OF_X :: module1.x
```
Selecting multiple ids returns another module with those ids
```
val module1 = <{
  type TYPE_OF_X = Int
  val x = 2;
  val y = 3;
}>;
val module2 = module1.[TYPE_OF_X, x]
```
Modules are loadable, which puts their contents in current scope of the module
```
val module1 = <{
  type TYPE_OF_X = Int
  val x = 2;
}>;
load module1;
val an_x = TYPE_OF_X :: x;
```
Since files are modules, you can import them which creates a module
object for you to manipulate. Note the dot notation. Its relative to the Repository root which you will specify via a config TBD. 
```
my_module = import path.to.above;
load my_module;
val = some_value_loaded_from_module;
```
```
load (import path.to.above);
val = some_value_loaded_from_module;
```
if you import a directory, it creates a module with names pointing to each of the contents. It will ignore any files not ending with .mtn

```
Dir
  root
    sub1
      a.mtn
      a.md
    sub2
      b.cpp
    c.mtn
```
```javascript
val module = import Dir;
val a = module.root.sub1.a
// val b does not exist
val c = module.root.c
```

Modules have types. The type of a Module is an interface. For example, the type (aka interface) of the first module would be
```javascript
type Above = <{
  type Point;
  type Numeric;
  val tupleize = a -> b -> (a,b);
  val a_three = Int;
}>;
```
It declares what types will be declared, and the types of any values contained within. It is not order dependent. 

Interfaces can also have fewer types than what are declared.
This is also a type for the first module
```javascript
type Above = <{
  type Point;
  val a_three = Int;
}>;
val double_three = (Above :: x) -> (x.a_three * 2)
```

When calling mountain from the commandline
```bash
$ mountain /path/to/my/file.mtn
```
file.mtn is assumed to be a member of the Console interface
```
type Console = <{
  val main = (@Console, Tuple String) -> (@Console, Int);
}>
```

In summary, within a module are a few keywords:
* val \<name> = \<expr> : denotes a concrete value or function
* type \<name> = \<expr>: denotes a type for use in annotations
* load \<expr>: takes a module and opens it in the context
* import \<expr>: takes a file and creates a module from it
* dec \<name> :: \<type> : denotes that a particular name has a particular type within this module

## Builtin Types

These types are included in mountain. k and v represent any type in the language.

* Bool
  * True
  * False
* Int
  * 1
  * 100
* Float
  * 1\. 
  * 2.0
* String
  * "hello"
  * "hello \" w\'o\'rld"
* Char
  * 'c'
  * '\n'
* Set k
  * {1,2,3}
* Map k v
  * TBD
* Tuple k
  * (2.0, 3.0)
  * (2.0, 3, "hello")
  * ()
* ()
  * () is the only term which is both a type and a value
* Sum 
  * Combines any two types in an "or" type
  * Tuple (Int | String)
    * Ex: (3, "hello")
* Label
  * Labels a value, case sensitive
  * Point:(x:Int, y:Int)
    * Ex: Point(x:3,y:4)
* Token
  * Written as $(\<some name here\>)
  * A built in type that cannot be created in the language
  * Represents an ability to use an external resource
* Function
  * k -> v
  * Ex: Int -> (Int, Int)
* Thing
  * Things are syntactic sugar for labels of the unit.
  * \#apple means apple:()
  * \#orange means orange:()
  
## Usage annotations

The lhs of an arrow is called a pattern. In the arrow type, usage must be called out. 

* ? means used 0,1 times
* \+ means used 2 or more
* \* means 0 or more, aka let the compiler figure which of the above

By default the compiler assumes all non-pattern terms have *, but you may
specifically annotate then with these fields. Pattern terms by default are ?and must be marked with + 
 
There are two modes for all data in Mountain: unique or const. 
* unique : there is only ever allowed to be a single ref to that data
* const : there can be more than one ref to that data

unique types are treated mutably by the compiler. That means they are modified in place. 

const types are treated as const by the compiler. THat means many things can see and view them, but 

```

```

? annotated values are mutated in place. There is always a single reference to them. 

+ annotated values are const and may have multiple views looking at them. Any values derived from them are copied into new memory

Functions with usage patterns specify what usage values may be used within them
```
(+a, ?b) -> (a,a,b)
```
This says that a is copied and b is used once

