# Scala Functional Programming Library

Implementations of functional programming types in Scala.

These implementations differ from most in that they do not use
a type class pattern or any implicit classes/conversions. Each
basic functional type (functor, applicative, monad, monoid) is
a trait which must be extended or mixed in.

This library has been developed as an exercise. Its types are not
as performant as the scala standard library or other functional
scala libraries (compare, for instance, FpList.reverse with
List.reverse for large lists).

