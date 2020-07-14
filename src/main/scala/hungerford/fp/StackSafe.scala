package hungerford.fp

import hungerford.fp.Monad

sealed trait StackSafe[ T ] extends Monad[ StackSafe, T ] {

    override def flatMap[ A, B ]( a : StackSafe[ A ] )( fn : A => StackSafe[ B ] ) : StackSafe[ B ] = FlatMap( a, fn )

    override def get[ A ]( ele : StackSafe[ A ] ) : Option[ A ] = Some( ele.run )

    override def unit[ A ]( ele : A ) : StackSafe[ A ] = Result( ele )

    def run : T = StackSafe.run( this )
}

final case class Result[ T ]( value : T ) extends StackSafe[ T ]

final case class Call[ T ]( f : () => StackSafe[ T ] ) extends StackSafe[ T ]

object Call {
    def from[ T ]( block : => StackSafe[ T ] ) : Call[ T ] = Call( () => block )
}

final case class FlatMap[ S, T ]( sf : StackSafe[ S ], f : S => StackSafe[ T ] ) extends StackSafe[ T ]

object StackSafe {
    def apply[ T ]( block : => StackSafe[ T ] ) : T = Call( () => block ).run

    def run[ A ]( sf : StackSafe[A] ): A = sf match {
        case Result( a ) => a
        case Call( r ) => run( r() )
        case FlatMap( x : StackSafe[ Any ], f : (Any => StackSafe[ A ] ) ) => x match {
            case Result( a ) => run( f ( a) )
            case Call( r ) => run( FlatMap( r(), f ) )
            case FlatMap( y : StackSafe[ Any ], g : (Any => StackSafe[ Any ]) ) =>
                run( y.flatMap( g( _ ).flatMap( f ) ) )
        }
    }
}