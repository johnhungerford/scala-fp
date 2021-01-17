package org.hungerford.fp.impure

import org.hungerford.fp.types.Monad

// An effects type
trait FpImpure[ T ] extends Monad[ FpImpure, T ] {
    def run : () => T

    override def flatMap[ A, B ]( a : FpImpure[ A ] )( fn : A => FpImpure[ B ] ) : FpImpure[ B ] = FpImpure ( fn( a.run() ).run() )

    override def unit[ A ]( ele : A ) : FpImpure[ A ] = FpImpure ( ele )

    override def get[ A ]( ele : FpImpure[ A ] ) : Option[ A ] = Some( ele.run() )

    def loop( times : Int ) : FpImpure[ T ] = FpImpure.loop( times )( this )

    def loop : FpImpure[ Unit ] = FpImpure.loop( this )
}

object FpImpure {
    def apply[ A ]( fn : => A ) : FpImpure[ A ] = new FpImpure[ A ] {
        override def run : ( ) => A = ( ) => fn
    }

    def loop[ A ]( times : Int )( block : FpImpure[ A ] ) : FpImpure[ A ] =
        if ( times <= 1 ) block
        else for {
            _ <- block
            res <- loop( times - 1 )( block )
        } yield res

    def loop[ A ]( block : FpImpure[ A ] ) : FpImpure[ Unit ] = for {
        _ <- block
        _ <- loop( block )
    } yield ()
}
