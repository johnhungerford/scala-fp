package hungerford.fp

trait FpImpure[ T ] extends Monad[ FpImpure, T ] {
    def run : () => T

    override def flatMap[ A, B ]( a : FpImpure[ A ] )( fn : A => FpImpure[ B ] ) : FpImpure[ B ] = FpImpure ( fn( a.run() ).run() )

    override def unit[ A ]( ele : A ) : FpImpure[ A ] = FpImpure ( ele )

    override def get[ A ]( ele : FpImpure[ A ] ) : Option[ A ] = Some( ele.run() )
}

object FpImpure {
   def apply[ A ]( fn : => A ) : FpImpure[ A ] = new FpImpure[A] {
        override def run : () => A = () => fn
   }
}
