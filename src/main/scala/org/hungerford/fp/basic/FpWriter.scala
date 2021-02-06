package org.hungerford.fp.basic

import org.hungerford.fp.types.{Monad, MonadInvariant, MonadStatic, MonadStaticInvariant, Monoid, MonoidCovariant, MonoidCovariantStatic, MonoidStatic}

trait FpWriter[ T, A <: Monoid[ B ], B ] extends MonadInvariant[ ({ type U[ X ] = FpWriter[ X, A, B ] })#U, T ] {
    def value : T
    def context : A

    override val static : MonadStaticInvariant[ ( {
        type U[ X ] = FpWriter[ X, A, B ]
    } )#U ] = FpWriter.Static[ A, B ]( context.static )

    override def toString : String = s"FpWriterCovariant( ${value}, ${context} )"

}

object FpWriter {
    def apply[ T, A <: Monoid[ B ], B ]( v : T, c : A ) : FpWriter[ T, A, B ] = new FpWriter[ T, A, B ] {
        override def value : T = v
        override def context : A = c

        override val static : MonadStaticInvariant[ ( {
            type U[ X ] = FpWriter[ X, A, B ]
        } )#U ] = Static[ A, B ]( c.static )
    }

    def unapply[ T, A <: Monoid[ B ], B ]( writer : FpWriter[ T, A, B ] ) : Option[(T, A)] = Some( (writer.value, writer.context) )

    def Static[ A <: Monoid[ B ], B ]( monoidStatic : MonoidStatic[ B ] ) : MonadStaticInvariant[ ({ type U[ X ] = FpWriter[ X, A, B ] })#U ] = new MonadStaticInvariant[ ({ type U[ X ] = FpWriter[ X, A, B ] })#U ] {
        override def flatMap[ X, Y ]( a : FpWriter[ X, A, B ] )
                                    ( fn : X => FpWriter[ Y, A, B ] ) : FpWriter[ Y, A, B ] = a match {
            case FpWriter( v, c : Monoid[ B ] ) =>
                val FpWriter( newV, newC : Monoid[ B ] ) = fn( v )
                FpWriter[ Y, A, B ]( newV, c.combineM( newC ).asInstanceOf[ A ] )
        }

        override def unit[ X ]( ele : X ) : FpWriter[ X, A, B ] = FpWriter[ X, A, B ]( ele, monoidStatic.empty.asInstanceOf[ A ] )
    }
}

trait FpWriterCovariant[ +T, A[ +_ ] <: MonoidCovariant[ A, B ], B ]
  extends Monad[ ({ type U[ +X ] = FpWriterCovariant[ X, A, B ] })#U, T ] {

    def value : T
    def context : A[ B ]

    override val static : MonadStatic[ ( {
        type U[ +X ] = FpWriterCovariant[ X, A, B ]
    } )#U ] = FpWriterCovariant.Static[ A, B ]( context.static )

    override def toString : String = s"FpWriterCovariant( ${value}, ${context} )"

}

object FpWriterCovariant {

    def apply[ T, A[ +_ ] <: MonoidCovariant[ A, B ], B ]( v : T, c : A[ B ] ) : FpWriterCovariant[ T, A, B ] = new FpWriterCovariant[ T, A, B ] {
        override def value : T = v

        override def context : A[ B ] = c

        override def equals( obj : Any ) : Boolean = obj match {
            case fpw : FpWriterCovariant[ _, A, B ] => fpw.value == v && fpw.context == c
            case _ => false
        }
    }

    def unapply[ T, A[ +_ ] <: MonoidCovariant[ A, B ], B ]( writer : FpWriterCovariant[ T, A, B ] ) : Option[ (T, A[ B ]) ] = Some( writer.value, writer.context )

    def Static[ A[ +_ ] <: MonoidCovariant[ A, B ], B ]( monoidStatic : MonoidCovariantStatic[ A ] ) : MonadStatic[ ({ type U[ +X ] = FpWriterCovariant[ X, A, B ] })#U ] = {
        new MonadStatic[ ({ type U[ +X ] = FpWriterCovariant[ X, A, B ] })#U ] {


            override def flatMap[ C, D ]( a : FpWriterCovariant[ C, A, B ] )
                                        ( fn : C => FpWriterCovariant[ D, A, B ] ) : FpWriterCovariant[ D, A, B ] = a match {
                case FpWriterCovariant( v : C, c : A[ B ] ) =>
                    val FpWriterCovariant( newV : D, newC : A[ B ] ) = fn( v )
                    FpWriterCovariant[ D, A, B ]( newV, c.combine( newC ) )
            }

            override def unit[ X ]( ele : X ) : FpWriterCovariant[ X, A, B ] = FpWriterCovariant[ X, A, B ]( ele, monoidStatic.empty.asInstanceOf[ A[ B ] ] )
        }
    }
}
