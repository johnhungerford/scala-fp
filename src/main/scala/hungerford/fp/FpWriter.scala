package hungerford.fp

case class FpWriter[ T, A, B ]( value : T, context : Monoid[ A, B ] ) extends Monad[ ({ type U[X] = FpWriter[ X, A, B ] })#U, T ] {
    override def flatMap[ R, S ]( a : FpWriter[ R, A, B ] )
                                ( fn : R => FpWriter[ S, A, B ] ) : FpWriter[ S, A, B ] = a match {
        case FpWriter( v : R, c : Monoid[ A, B ] ) =>
            val FpWriter( newV, newC : Monoid[ A, B ] ) = fn( v )
            FpWriter( newV, c.combineM( newC ) )
    }

    override def unit[ C ]( ele : C ) : FpWriter[ C, A, B ] = FpWriter( ele, context.emptyM )

    override def get[ C ]( ele : FpWriter[ C, A, B ] ) : Option[ C ] = Some( ele.value )
}

