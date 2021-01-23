package org.hungerford.fp.basic

import org.hungerford.fp.types.{Monad, MonadCovariant, MonadStatic, Monoid, MonoidCovariant, MonoidCovariantStatic, MonoidStatic}

trait FpWriter[ T, A <: Monoid[ B ], B ] extends Monad[ ({ type U[ X ] = FpWriter[ X, A, B ] })#U, T ] {

    def value : T
    def context : A

    override def flatMap[ C, D ]( a : FpWriter[ C, A, B ] )( fn : C => FpWriter[ D, A, B ] ) : FpWriter[ D, A, B ] = FpWriterStatic[ A, B ]( context.asInstanceOf[ MonoidStatic[ A ] ]).flatMap( a )( fn )

    override def unit[ X ]( ele : X ) : FpWriter[ X, A, B ] = FpWriter( ele, context.empty.asInstanceOf[ A ] )

    override def toString : String = s"FpWriterCovariant( ${value}, ${context} )"

}

object FpWriter {
    def apply[ T, A <: Monoid[ B ], B ]( v : T, c : A ) : FpWriter[ T, A, B ] = new FpWriter[ T, A, B ] {
        override def value : T = v
        override def context : A = c
    }

    def unapply[ T, A <: Monoid[ B ], B ]( writer : FpWriter[ T, A, B ] ) : Option[(T, A)] = Some( (writer.value, writer.context) )
}

case class FpWriterStatic[ A <: Monoid[ B ], B ]( monoidStatic : MonoidStatic[ A ] ) extends MonadStatic[ ({ type W[ T ] = FpWriter[ T, A, B ] })#W ] {
    override def flatMap[ X, Y ]( a : FpWriter[ X, A, B ] )
                                ( fn : X => FpWriter[ Y, A, B ] ) : FpWriter[ Y, A, B ] = a match {
        case FpWriter( v, c : Monoid[ B ] ) =>
            val FpWriter( newV, newC : Monoid[ B ] ) = fn( v )
            FpWriter[ Y, A, B ]( newV, c.combineM( newC ).asInstanceOf[ A ] )
    }

    override def unit[ X ]( ele : X ) : FpWriter[ X, A, B ] = FpWriter[ X, A, B ]( ele, monoidStatic.empty )
}

trait FpWriterCovariant[ +T, A <: Monoid[ A ] ] extends MonadCovariant[ ({ type U[ X ] = FpWriterCovariant[ X, A ] })#U, T ] {

    def value : T
    def context : Monoid[ A ]

    override def flatMap[ R, S ]( a : FpWriterCovariant[ R, A ] )
                                ( fn : R => FpWriterCovariant[ S, A ] ) : FpWriterCovariant[ S, A ] = FpWriterCovariantStatic[ A ]( context ).flatMap( a )( fn )

    override def unit[ C ]( ele : C ) : FpWriterCovariant[ C, A ] = FpWriterCovariantStatic[ A ]( context ).unit( ele )

    override def toString : String = s"FpWriterCovariant( ${value}, ${context} )"
}

object FpWriterCovariant {

    def apply[ T, A <: Monoid[ A ] ]( v : T, c : Monoid[ A ] ) : FpWriterCovariant[ T, A ] = new FpWriterCovariant[ T, A ] {
        override def value : T = v

        override def context : Monoid[ A ] = c

        override def equals( obj : Any ) : Boolean = obj match {
            case fpw : FpWriterCovariant[ _, _ ] => fpw.value == v && fpw.context == c
            case _ => false
        }
    }

    def unapply[ T, A <: Monoid[ A ] ]( writer : FpWriterCovariant[ T, A ] ) : Option[ (T, Monoid[ A ]) ] = Some( writer.value, writer.context )
}

case class FpWriterCovariantStatic[ X <: Monoid[ X ] ]( monoidStatic : MonoidStatic[ X ] ) extends MonadStatic[ ({ type W[ T ] = FpWriterCovariant[ T, X ] })#W ] {
    override def flatMap[ A, B ]( a : FpWriterCovariant[ A, X ] )
                                ( fn : A => FpWriterCovariant[ B, X ] ) : FpWriterCovariant[ B, X ] = a match {
        case FpWriterCovariant( v : A, c : Monoid[ X ] ) =>
            val FpWriterCovariant( newV, newC : Monoid[ X ] ) = fn( v )
            FpWriterCovariant[ B, X ]( newV, c.combineM( newC ) )
    }

    override def unit[ A ]( ele : A ) : FpWriterCovariant[ A, X ] = FpWriterCovariant[ A, X ]( ele, monoidStatic.emptyM )
}

