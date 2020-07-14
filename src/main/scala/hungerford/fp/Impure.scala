package hungerford.fp

trait Impure[ A ] extends Monad[ Impure, A ] {
    def run : () => A

    override def flatMap[ A, B ]( a : Impure[ A ] )( fn : A => Impure[ B ] ) : Impure[ B ] = Impure ( fn( a.run() ).run() )

    override def unit[ A ]( ele : A ) : Impure[ A ] = Impure ( ele )

    override def get[ A ]( ele : Impure[ A ] ) : Option[ A ] = Some( ele.run() )
}

object Impure {
   def apply[ A ]( fn : => A ) : Impure[ A ] = new Impure[A] {
        override def run : () => A = () => fn
   }
}
