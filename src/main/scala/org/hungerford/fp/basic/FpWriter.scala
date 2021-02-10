package org.hungerford.fp.basic

import org.hungerford.fp.types.{Monad, MonadInvariant, MonadStatic, MonadStaticInvariant, Monoid, MonoidStatic, TypedMonoid, TypedMonoidStatic}

trait FpWriter[ +T, A <: Monoid[ _ ] ] extends Monad[ ({ type U[ +X ] = FpWriter[ X, A ] })#U, T ] {
    def value : T
    def context : A

    override val static : MonadStatic[ ( {
        type U[ +X ] = FpWriter[ X, A ]
    } )#U ] = FpWriter.Static[ A ]( context.static.asInstanceOf[ MonoidStatic[ A ] ] )

    override def toString : String = s"FpWriterTyped( ${value}, ${context} )"

}

object FpWriter {
    def apply[ T, A <: Monoid[ _ ] ]( v : T, c : A ) : FpWriter[ T, A ] = new FpWriter[ T, A ] {
        override def value : T = v
        override def context : A = c

        override val static : MonadStatic[ ( {
            type U[ +X ] = FpWriter[ X, A ]
        } )#U ] = Static[ A ]( c.static.asInstanceOf[ MonoidStatic[ A ] ] )

        override def equals( obj : Any ) : Boolean = obj match {
            case FpWriter( thatV, thatC ) => value == thatV && context == thatC
            case _ => false
        }
    }

    def unapply[ T, A <: Monoid[ _ ] ]( writer : FpWriter[ T, A ] ) : Option[(T, A)] = Some( (writer.value, writer.context) )

    def Static[ A <: Monoid[ _ ] ]( monoidStatic : MonoidStatic[ A ] ) : MonadStatic[ ({ type U[ +X ] = FpWriter[ X, A ] })#U ] = new MonadStatic[ ({ type U[ +X ] = FpWriter[ X, A ] })#U ] {
        override def flatMap[ X, Y ]( a : FpWriter[ X, A ] )
                                    ( fn : X => FpWriter[ Y, A ] ) : FpWriter[ Y, A ] = a match {
            case FpWriter( v : X , c : A ) =>
                val newWriter : FpWriter[ Y, A ] = fn( v )
                FpWriter[ Y, A ]( newWriter.value, monoidStatic.combine( c, newWriter.context ) )
        }

        override def unit[ X ]( ele : X ) : FpWriter[ X, A ] = FpWriter[ X, A ]( ele, monoidStatic.empty.asInstanceOf[ A ] )
    }
}

trait FpWriterTyped[ +T, A[ +_ ] <: TypedMonoid[ A, _ ] ]
  extends Monad[ ({ type U[ +X ] = FpWriterTyped[ X, A ] })#U, T ] {

    def value : T
    def context : A[ _ ]

    override val static : MonadStatic[ ( {
        type U[ +X ] = FpWriterTyped[ X, A ]
    } )#U ] = FpWriterTyped.Static[ A ]( context.static )

    override def toString : String = s"FpWriterTyped( ${value}, ${context} )"

}

object FpWriterTyped {

    def apply[ T, A[ +_ ] <: TypedMonoid[ A, _ ] ]( v : T, c : A[ _ ] ) : FpWriterTyped[ T, A ] = new FpWriterTyped[ T, A ] {
        override def value : T = v

        override def context : A[ _ ] = c

        override def equals( obj : Any ) : Boolean = obj match {
            case fpw : FpWriterTyped[ _, A ] => fpw.value == v && fpw.context == c
            case _ => false
        }
    }

    def unapply[ T, A[ +_ ] <: TypedMonoid[ A, _ ] ]( writer : FpWriterTyped[ T, A ] ) : Option[ (T, A[ _ ]) ] = Some( writer.value, writer.context )

    def Static[ A[ +_ ] <: TypedMonoid[ A, _ ] ]( monoidStatic : TypedMonoidStatic[ A ] ) : MonadStatic[ ({ type U[ +X ] = FpWriterTyped[ X, A ] })#U ] = {
        new MonadStatic[ ({ type U[ +X ] = FpWriterTyped[ X, A ] })#U ] {


            override def flatMap[ C, D ]( a : FpWriterTyped[ C, A ] )
                                        ( fn : C => FpWriterTyped[ D, A ] ) : FpWriterTyped[ D, A ] = a match {
                case FpWriterTyped( v : C, c ) =>
                    val FpWriterTyped( newV : D, newC ) = fn( v )
                    FpWriterTyped[ D, A ]( newV, c.combine( newC ) )
            }

            override def unit[ X ]( ele : X ) : FpWriterTyped[ X, A ] = FpWriterTyped[ X, A ]( ele, monoidStatic.empty )
        }
    }
}
