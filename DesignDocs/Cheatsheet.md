# Mountain Spec

The language is called Mountain

## Whitespace

Whitespace like tabs, newlines, and spaces are insignificant

## Comments

```
// is a line comment
```

```
/*

This is a block comment

*/
```

## Expressions

The core of this language is that it is a simply typed lambda calculus

## Builtin Types

These types are included in mountain. k and v represent any type in the language.

The format of below is
* type
  * example 1
  * example 2

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
  * {k1:v1, k2:v2}
* Tuple k
  * (2.0, 3.0)
  * (2.0, 3, "hello")
  * ()
* Sum 
  * (3, "hello")
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

## Definitions

Definitions are terms as well, and can be used anywhere in the code where a value can be used. They must end with a value, and can be chained. a semicolon must not be used on the last value. The return keyword is optional

```
  x = 3; 
  y = 4; 
  return (x,y)
```

```
  x = 3; 
  y = 4; 
  (x,y)
```
```
(a = 3; a) + (a=4;a)
```
Both of the above would return 7.

The only place definitions may not appear is on the left side of an arrow.

Definitions can deconstruct labels and tuples

```
(a,b) = ((1,2), 3);
(c,?) = a;
b + c //returns 5
```
```
x:a = x:3;
a + 2 // returns 5
```

## Functions

Functions are first class objects

```
3 -> 4
```

```
x -> x
```

```
(x,y) -> x + y
```

_ can match anything. Whatever is matched with a _ is not useable on the rhs of the arrow
```
(_,_,x) -> x
```

```
x = 3;
x -> x // returns 3 -> 3
```

```
// Only matches objects of type Point:(x:Int, y:Int)
Point:(x:a,y:b) -> (a,b)
```

Functions can be chained multiple times

```
3 -> 4 -> 5 -> 6 
```

This is equivalent to 
```
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

```
(
       3 -> 4
   ||  4 -> 5
   ||  5 -> 6
)(4) // will return 5

```

```
(
      4       -> 5
   || "hello" -> "world"
)("hello") // will return "world"

```

## Function Call

to call a function you simply write two terms immediately following one another with optional whitespace. 
```
incr3 = 3 -> 4;
incr3(3)
```
-- or -- 
```
incr3 = 3 -> 4;
incr3 3
```
both will call incr3 with 3, returning 4 

## Type Annotations

take the form \<Type> :: \<Term>
```
Int :: 4 
```
```
(Int -> Int) :: 4 -> 5 
```
```
((Float, Int) -> Float) :: (x, _) -> x 
```

If you use a variable more than once in the rhs of a function, you must mark it with a "+" in the type in the lhs of the ->

```
duplicate = +Int -> (Int, Int) :: x -> (x,x)
```

Type annotations can be variables, which make the function polymorphic. It can work with any type "a" <br>
```
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

```
+a -> (a,a,Int) :: x -> (x,x,3)
```
The "+" would still need to be added 

types are automatically inferred in the language, but can be helpful to have as documentation, or in ambiguous cases.

## Modules & Intefaces

Modules are groups of data, types and functions.

All files in mountain are modules. Modules are maps of names to values and types. the keywords def and type are specifically only used for modules. 

```
struct {
  type Numeric = Float | Int
  type Point = A -> B -> (x:A, y:B) // types can be parameterize
  // Ex: point Int Float would be a type like (x:Int, y:Float)
  decl point :: Point Numeric Numeric
  data point = a -> b -> (x:a, y:b)
  import <file_path>
}>

def a_three = 3.0;
```
Modules are sequential defs
```
def x = 3.0;
def y = x + 2.5;
def tuple = (x,y);
```
Modules are first class objects in Mountain. Nest modules like so
```
def module1 = <{
  def a = "something";
}>;
def module2 = <{
  def a = "something_else";
}>
```
Modules are selectable
```
def module1 = <{
  type TYPE_OF_X = Int
  def x = 2;
}>;
def an_x = module1.TYPE_OF_X :: module1.x
```
Selecting multiple ids returns another module with those ids
```
def module1 = <{
  type TYPE_OF_X = Int
  def x = 2;
  def y = 3;
}>;
def module2 = module1.[TYPE_OF_X, x]
```
Modules are loadable, which puts their contents in current scope of the module
```
def module1 = <{
  type TYPE_OF_X = Int
  def x = 2;
}>;
load module1;
def an_x = TYPE_OF_X :: x;
```
Since files are modules, you can import them which creates a module
object for you to manipulate. Note the dot notation. Its relative to the Repository root which you will specify via a config TBD. 
```
my_module = import path.to.above;
load my_module;
def = some_value_loaded_from_module;
```
```
load (import path.to.above);
def = some_value_loaded_from_module;
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
```
def module = import Dir;
def a = module.root.sub1.a
// def b does not exist
def c = module.root.c
```

Modules have types. The type of a Module is an interface. For example, the type (aka interface) of the first module would be
```
data SomeModule = <{
  type Point = a -> b -> (x:a, y:b);
  decl a_point = Point Int Int
  data a_point = (x:2, y:4);
}>;
```
It declares what types will be declared, and the types of any values contained within. It is not order dependent. 

Interfaces can also have fewer types than what are declared.
This is also a type for the first module
```
type SomeModule = <{
  kind Point = * -> * -> *;
  decl a_point = Point Int Int;
}>;
decl double_x = SomeModule -> Int
data double_x = m -> (m.a_point.x * 2)
```

When calling mountain from the commandline
```bash
$ mountain /path/to/my/file.mtn
```
file.mtn is assumed to be a member of the Console interface
```
type Console = <{
  def main = (@Console, Tuple String) -> (@Console, Int);
}>
```

In summary, within a module are a few keywords:
* def \<name> = \<expr> : denotes a concrete value or function
* type \<name> = \<expr>: denotes a type for use in annotations
* dec \<name> @ \<type> : denotes that a particular name has a particular type within this module
* load \<expr>: takes a module and opens it in the context
* import \<expr>: takes a file and creates a module from it
  
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

