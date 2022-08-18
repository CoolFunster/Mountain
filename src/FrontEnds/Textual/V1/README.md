# Category Parser

Here's the syntax of the textual language              

Name -> "abc_..." | _ <br>
ID -> Name | #\<num> <br>

Thing -> `Name <br>
Label -> Name:\<category> <br>
Element Of -> Name@\<category> <br>
Tuple -> (\<category>, \<category>, ...) <br>
Either -> |\<category>, \<category>, ...| <br>
Function -> \<category> -> \<category> <br>
Composition -> \*(\<category>)* <br>
Match -> \*|\<category>, \<category>, ...| <br>
Refined -> {\<category> | \<category> -> Bool } <br>
Flexible -> (%) <br>
Reference -> $Name <br>
Any -> Any <br>
Call -> \<Function>[\<category>] <br>
Dereference -> \<category>.ID <br>
TypeAnnotation -> \<category> :: \<category> <br>
Definition -> define \<category> <br>
Import -> import \<category> <br>
               


