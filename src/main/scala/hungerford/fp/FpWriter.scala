package hungerford.fp

trait FpWriter[ T, A <: Monoid[ B ], B ] extends Monad[ ({ type U[ X ] = FpWriter[ X, A, B ] })#U, T ] {

    def value : T
    def context : A

    override def flatMap[ C, D ]( a : FpWriter[ C, A, B ] )( fn : C => FpWriter[ D, A, B ] ) : FpWriter[ D, A, B ] = a match {
        case FpWriter( v : C, c : A ) =>
            val FpWriter( newV, newC : A ) = fn( v )
            FpWriter( newV, c.combine( newC.asInstanceOf[ B ] ).asInstanceOf[ A ] )
    }

    override def unit[ X ]( ele : X ) : FpWriter[ X, A, B ] = FpWriter( ele, context.empty.asInstanceOf[ A ] )

    override def get[ X ]( ele : FpWriter[ X, A, B ] ) : Option[ X ] = Some( ele.value )

    override def toString : String = s"FpWriterCovariant( ${value}, ${context} )"

}

object FpWriter {
    def apply[ T, A <: Monoid[ B ], B ]( v : T, c : A ) : FpWriter[ T, A, B ] = new FpWriter[ T, A, B ] {
        override def value : T = v
        override def context : A = c
    }

    def unapply[ T, A <: Monoid[ B ], B ]( writer : FpWriter[ T, A, B ] ) : Option[(T, A)] = Some( (writer.value, writer.context) )
}

trait FpWriterCovariant[ T, A[ _ ], B ] extends MonadCovariant[ ({ type U[ X ] = FpWriterCovariant[ X, A, B ] })#U, T ] {

    def value : T
    def context : MonoidCovariant[ A, B ]

    override def flatMap[ R, S ]( a : FpWriterCovariant[ R, A, B ] )
                                ( fn : R => FpWriterCovariant[ S, A, B ] ) : FpWriterCovariant[ S, A, B ] = a match {
        case FpWriterCovariant( v : R, c : MonoidCovariant[ A, B ] ) =>
            val FpWriterCovariant( newV, newC : MonoidCovariant[ A, B ] ) = fn( v )
            FpWriterCovariant( newV, c.combineM( newC ) )
    }

    override def unit[ C ]( ele : C ) : FpWriterCovariant[ C, A, B ] = FpWriterCovariant( ele, context.emptyM )

    override def get[ C ]( ele : FpWriterCovariant[ C, A, B ] ) : Option[ C ] = Some( ele.value )

    override def toString : String = s"FpWriterCovariant( ${value}, ${context} )"
}

object FpWriterCovariant {

    def apply[ T, A[ _ ], B ]( v : T, c : MonoidCovariant[ A, B ] ) : FpWriterCovariant[ T, A, B ] = new FpWriterCovariant[ T, A, B ] {
        override def value : T = v
        override def context : MonoidCovariant[ A, B ] = c
    }

    def unapply[ T, A[ _ ], B ]( writer: FpWriterCovariant[ T, A, B ] ) : Option[ ( T, MonoidCovariant[ A, B ] ) ] = Some( writer.value, writer.context )

}

