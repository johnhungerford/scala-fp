package org.hungerford.fp.recursion

import org.hungerford.fp.types.Monad

import scala.annotation.tailrec

/**
 * A type class for making recursive functions stack safe, using the
 * "trampoline" method. See the companion object below for implementation
 * of the `run` method, which evaluates a trampolined recursive function.
 *
 * @tparam T The result type of the recursive function
 */
sealed trait StackSafe[ T ] extends Monad[ StackSafe, T ] {

    override def flatMap[ A, B ]( a : StackSafe[ A ] )( fn : A => StackSafe[ B ] ) : StackSafe[ B ] = FlatMap( a, fn )

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
    /**
     *  Pass a block that evaluates to a StackSafe case -- either a Result, a Call,
     *  or a FlatMap -- to StackSafe and it will be evaluated without blowing the stack
     *
     * @param block
     * @tparam T
     * @return
     */
    def apply[ T ]( block : => StackSafe[ T ] ) : T = Call( () => block ).run

    @tailrec
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

    // Define Y-combinators YN for functions of N parameters (1 <= N <= 6)
    private def Y[ A, Z ]( fn : ( A => Z ) => ( A => Z ) ) : A => Z =
        ( a : A ) => fn( Y( fn ) )( a )
    private def Y2[ A, B, Z ]( fn : ( (A, B) => Z ) => ( (A, B) => Z ) ) : (A, B) => Z =
        ( a : A, b : B ) => fn( Y2( fn ) )( a, b )
    private def Y3[ A, B, C, Z ]( fn : ( (A, B, C) => Z ) => ( (A, B, C) => Z ) ) : (A, B, C) => Z =
        ( a : A, b : B, c : C ) => fn( Y3( fn ) )( a, b, c )
    private def Y4[ A, B, C, D, Z ]( fn : ( (A, B, C, D) => Z ) => ( (A, B, C, D) => Z ) ) : (A, B, C, D) => Z =
        ( a : A, b : B, c : C, d : D ) => fn( Y4( fn ) )( a, b, c, d )
    private def Y5[ A, B, C, D, E, Z ]( fn : ( (A, B, C, D, E) => Z ) => ( (A, B, C, D, E) => Z ) ) : (A, B, C, D, E) => Z =
        ( a : A, b : B, c : C, d : D, e : E ) => fn( Y5( fn ) )( a, b, c, d, e )
    private def Y6[ A, B, C, D, E, F, Z ]( fn : ( (A, B, C, D, E, F) => Z ) => ( (A, B, C, D, E, F) => Z ) ) : (A, B, C, D, E, F) => Z =
        ( a : A, b : B, c : C, d : D, e : E, f : F ) => fn( Y6( fn ) )( a, b, c, d, e, f )

    /**
     *  `selfCall` provides a way to construct a self-calling anonymous lambda function to StackSafe
     *  to be run as a trampoline. It's parameter's signature ( fn : (A => StackSafe[Z]) => (A => StackSafe[Z])
     *  seems very strange, but think of it this way: the first A => StackSafe[Z] represents the function you
     *  are defining *as a parameter that this function itself can call*. The second A => StackSafe[Z] is that
     *  self-referring function definition.
     *
     *  It's usage is as follows:
     *
     *      def someMethod : X => Y = StackSafe.selfCall[ X, Y ] { callSelf => ( x : X ) =>
     *          if ( x == testValue ) Result( x ) // non recursive result
     *          else Call.from {  // Wrap all calls to all functions returning a StackSafe in a Call
     *              val newX = someTransformOnX( x )
     *              callSelf( newX ).map( res => someTransformOnRes( res ) ) // callSelf will call this same lambda
     *                                                                       // map allows you to act on result
     *          }
     *      }
     *
     *  Explicitly passing the type parameters to selfCall may be necessary
     *
     *  The same syntax can be used with selfCallN to construct a lambda with N parameters:
     *
     *      def someMethod : (M, N, O, P) => T = StackSafe.selfCall4[ M, N, O, P, T ] {
     *          callSelf => ( m : M, n : N, o : O, p : P ) =>
     *              ...
     *              Call.from( callSelf( newM, newN, newO, newP ) )
     *      }
     *
     * @param fn Function relating the self calling function to itself
     * @tparam A Parameter type of the recursive (self-calling) function being defined
     * @tparam Z Return type of the recursive function being defined
     * @return
     */
    def selfCall[ A, Z ]( fn : ( A => StackSafe[ Z ] ) => ( A => StackSafe[ Z ] ) ) : A => Z =
        ( a : A ) => StackSafe( Y( fn )( a ) )
    def selfCall2[ A, B, Z ]( fn : ( (A, B) => StackSafe[ Z ] ) => ( (A, B) => StackSafe[ Z ] ) ) : (A, B) => Z =
        ( a : A, b : B ) => StackSafe( Y2( fn )( a, b ) )
    def selfCall3[ A, B, C, Z ]( fn : ( (A, B, C) => StackSafe[ Z ] ) => ( (A, B, C) => StackSafe[ Z ] ) ) : (A, B, C) => Z =
        ( a : A, b : B, c : C ) => StackSafe( Y3( fn )( a, b, c ) )
    def selfCall4[ A, B, C, D, Z ]( fn : ( (A, B, C, D) => StackSafe[ Z ] ) => ( (A, B, C, D) => StackSafe[ Z ] ) ) : (A, B, C, D) => Z =
        ( a : A, b : B, c : C, d : D ) => StackSafe( Y4( fn )( a, b, c, d ) )
    def selfCall5[ A, B, C, D, E, Z ]( fn : ( (A, B, C, D, E) => StackSafe[ Z ] ) => ( (A, B, C, D, E) => StackSafe[ Z ] ) ) : (A, B, C, D, E) => Z =
        ( a : A, b : B, c : C, d : D, e : E ) => StackSafe( Y5( fn )( a, b, c, d, e ) )
    def selfCall6[ A, B, C, D, E, F, Z ]( fn : ( (A, B, C, D, E, F) => StackSafe[ Z ] ) => ( (A, B, C, D, E, F) => StackSafe[ Z ] ) ) : (A, B, C, D, E, F) => Z =
        ( a : A, b : B, c : C, d : D, e : E, f : F ) => StackSafe( Y6( fn )( a, b, c, d, e, f ) )

}
