package hungerford.fp

trait Monad [ +T, +A ] {
    def flatMap[ C, B ]( fn : A => Monad[ C, B ] ) : Monad[ C, B ]
    def map[ C, B ]( fn : A => B ) : Monad[ C, B ] = flatMap( ( ele : A ) => unit( fn( ele ) ).asInstanceOf[ Monad[ C, B ] ] )

    def sub[ C >: T, B ]( ele : Monad[ C, B ] ) : Monad[ C, B ] = flatMap( _ => ele )
    def unit[ C >: T, B >: A ]( ele : B ) : Monad[ C, B ]
}
