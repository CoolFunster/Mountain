# Category Parser

Here's the syntax of the textual language              

Name -> initial letter then anything not conflicting 
	not Any
OptionalName -> optional name
ID -> Name | Idx 

Thing -> `  Name
Tuple -> OptionalName:([categories])
Sumple -> OptionalName:|[categories]|
Group -> OptionalName:^{[categories]}
Refined -> OptionalName:{category | placeholder -> x@{Bool}}
Composition -> OptionalName:*([morphisms])
Sumposition -> OptionalName:+([morphisms])
Morphism -> OptionalName:category -> category
Placeholder -> Name@category or Name<Level>@category
Flexible -> (%)
Reference -> $Name
Universal -> Any
Recursive -> rec Name:category
Call -> Morphism[argument]
SubCall -> Morphism[Name -> argument]
Dereference -> Category.Name
Membership -> big_category :: small_category
let binding -> let [Name:category] in Name:category
			   expr Name:category where [Name:category]
               