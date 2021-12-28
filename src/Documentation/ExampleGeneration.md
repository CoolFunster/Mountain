"NaturalNumber:
    (
        Nat:|
            zero:(),
            nonzero:(
                head:(),
                rest:nat_data:|
                    zero:(),
                    nonzero:(
                        head:(),
                        rest:$nat_data
                    )
                |
            )
        |,
        increment:
            given x@|
                zero:(),
                nonzero:(
                    head:(),
                    rest:nat_data:|
                        zero:(),
                        nonzero:(
                            head:(),
                            rest:$nat_data
                        )
                    |
                )
            | 
            -> return ((),$x),
        decrement:
            given x@(
                head:(),
                rest:nat_data:|
                    zero:(),
                    nonzero:(
                        head:(),
                        rest:$nat_data
                    )
                |
            ) 
            -> return ($x).[1])"
============================================
sample 1:
(
    (),
    ()->((),()),
    ((),())->()
)
=============================================
sample 2:
(
    ((),()),
    ((),())->((),((),())),
    ((),((),()))->((),())
)