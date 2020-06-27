package hungerford.fp

trait FpWriter[ T, A[ _ ], B ] extends Monad[ ({ type U[ X ] = FpWriter[ X, A, B ] })#U, T ] {

    def value : T
    def context : Monoid[ A, B ]

    override def flatMap[ R, S ]( a : FpWriter[ R, A, B ] )
                                ( fn : R => FpWriter[ S, A, B ] ) : FpWriter[ S, A, B ] = a match {
        case FpWriter( v : R, c : Monoid[ A, B ] ) =>
            val FpWriter( newV, newC : Monoid[ A, B ] ) = fn( v )
            FpWriter( newV, c.combineM( newC ) )
    }

    override def unit[ C ]( ele : C ) : FpWriter[ C, A, B ] = FpWriter( ele, context.emptyM )

    override def get[ C ]( ele : FpWriter[ C, A, B ] ) : Option[ C ] = Some( ele.value )

    override def toString : String = s"FpWriter( ${value}, ${context} )"
}

object FpWriter {

    def apply[ T, A[ _ ], B ]( v : T, c : Monoid[ A, B ] ) : FpWriter[ T, A, B ] = new FpWriter[ T, A, B ] {
        override def value : T = v
        override def context : Monoid[ A, B ] = c
    }

    def unapply[ T, A[ _ ], B ]( writer: FpWriter[ T, A, B ] ) : Option[ ( T, Monoid[ A, B ] ) ] = Some( writer.value, writer.context )

}

