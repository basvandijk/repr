This library allows you to render overloaded expressions to their
textual representation. For example:

    *Repr> let r = 1.5 + 2 + (3 + (-4) * (5 - pi / sqrt 6)) :: Repr Double
    *Repr> extract r
    17.281195923884734
    *Repr> show rd
    \"1.5 + 2.0 + (3.0 + negate 4.0 * (5.0 - pi / sqrt 6.0))\"
