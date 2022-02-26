Category functions

has

==
!=

assert

a category has or is equal to other things

want to have an assert statement built on top of that?

what is membership?

(big)::(small) - the small category is a member of the big category or throw an error/flag

big has small - if the small category is a member of the big category return True
    shouldn't this just be a function in the language?
    has: (a@Category, b@Category) -> Bool
    eq: (a@Category, b@Category) -> Bool
    neq: (a@Category, b@Category) -> Bool