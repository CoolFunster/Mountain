# Category Parser

Here's the syntax of the textual language              

reserved keywords/symbols:<br>
  * function
  * return
  * import
  * \#
  * :



Name -> "a-zA-Z_" | _ <br>
ID -> Name

Thing -> #ID <br>
Set -> {\<category>}
Unique -> <\<category>>
Label -> Name:\<category> <br>
Variable -> Name@\<category> <br>
Binding -> Variable = \<category>;
Scope -> { Binding ... Binding *return* \<category>}
Tuple -> (\<category>, \<category>, ...) <br>
Either -> |\<category>, \<category>, ...| <br>
Function -> *function* \<category> -> ... -> *return* \<category> <br>
Composition -> (function . function) <br>
Match -> |\<category> . \<category> . ...| <br>
Refined -> {\<category> | \<category> -> Bool } <br>
Reference -> ID <br>
Flexible -> ?ID <br>
_ | Any -> Any <br>
Call -> \<Function> \<category> <br>
Dereference -> 
  * \<category>.ID 
  * \<category>[Int] 
  * \<category>[ID] 
<br>

Import -> import \<category>
TypeAnnotation -> \<category> :: \<category> <br>
<!-- Definition -> define \<category> <br> -->
<!-- Import -> import \<category> <br> -->
               


