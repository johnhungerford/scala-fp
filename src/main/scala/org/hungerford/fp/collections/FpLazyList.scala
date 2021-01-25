package org.hungerford.fp.collections

import org.hungerford.fp.recursion.{Call, Result, StackSafe}
import org.hungerford.fp.types.{MonadCovariant, MonadStatic}

sealed trait FpLazyList[ +T ] extends MonadCovariant[ FpLazyList, T ] {
    private[ collections ] val evaluate : ( ) => FpEvaluatedLazyList[ T ]

    def toList : FpList[ T ] = StackSafe.selfCall[ FpLazyList[ T ], FpList[ T ] ] {
        thisFn =>
            ll => ll match {
                case FpLazyNil => Result( FpNil )
                case FpLazyListEval( next, head ) => Call.from {
                    thisFn( next ).map( l => l + head )
                }
                case FpUnevaluatedLazyList( evalFn ) => Call.from {
                    thisFn( evalFn() )
                }
            }
    }( this )

    def +[ B >: T ]( that : B ) : FpLazyList[ B ] = FpLazyList.combine( this, that )

    def ++[ B >: T ]( that : FpLazyList[ B ] ) : FpLazyList[ B ] = FpLazyList.combine( this, that )

    override def flatMap[ A, B ]( a : FpLazyList[ A ] )
                                ( fn : A => FpLazyList[ B ] ) : FpLazyList[ B ] = FpLazyList.flatMap( a )( fn )

    override def unit[ A ]( ele : A ) : FpLazyList[ A ] = FpLazyList.unit( ele )

    private def toStringInternal[ A ]( ll : FpLazyList[ A ] ) : String = StackSafe.selfCall[ FpLazyList[ A ], String ] {
        thisFn =>
          {
              case FpLazyNil => Result( FpLazyNil.toString )
              case FpUnevaluatedLazyList( _ ) => Result( "??" )
              case FpLazyListEval( tail, head ) => Call.from {
                  thisFn( tail ).map( tailString => s"${head.toString} + ${tailString}" )
              }
          }
    }( ll )

    override def toString : String = toStringInternal( this )

}

case class FpUnevaluatedLazyList[ +T ]( override val evaluate : ( ) => FpEvaluatedLazyList[ T ] ) extends FpLazyList[ T ] {
    override def equals( obj : Any ) : Boolean = obj match {
        case ll : FpLazyList[ _ ] => this.toList == ll.toList
        case _ => false
    }
}

trait FpEvaluatedLazyList[ +T ] extends FpLazyList[ T ] {
    private[ collections ] val evaluate : ( ) => FpEvaluatedLazyList[ T ] = ( ) => this
}

case object FpLazyNil extends FpEvaluatedLazyList[ Nothing ]

case class FpLazyListEval[ +T ]( tail : FpLazyList[ T ], head : T  ) extends FpEvaluatedLazyList[ T ] {
    override private[ collections ] val evaluate : ( ) => FpEvaluatedLazyList[ T ] = ( ) => this

    override def equals( obj : Any ) : Boolean = obj match {
        case ll : FpLazyList[ _ ] => this.toList == ll.toList
        case _ => false
    }
}

object FpLazyList extends MonadStatic[ FpLazyList ] {

    def combine[ A ]( a : FpLazyList[ A ], b : A ) : FpLazyList[ A ] = FpLazyListEval( a, b )

    def combine[ A ]( a : FpLazyList[ A ], b : FpLazyList[ A ] ) : FpLazyList[ A ] = StackSafe.selfCall2[ FpLazyList[ A ], FpLazyList[ A ], FpLazyList[ A ] ] {
        ( thisFn : (FpLazyList[ A ], FpLazyList[ A ]) => StackSafe[FpLazyList[ A ] ] ) =>
            (listA, listB)  =>
                listA match {
                    case FpLazyNil => Result( listB )
                    case FpUnevaluatedLazyList( evalFn ) =>  listB match {
                        case FpLazyNil => Result( listA )
                        case _ => Result( FpUnevaluatedLazyList { () =>
                            combine( evalFn(), listB ).evaluate()
                        } )
                    }
                    case FpLazyListEval( tail, head ) => Call.from {
                        thisFn( tail, listB ).map( ( ll : FpLazyList[ A ] ) => FpLazyListEval( ll, head ) )
                    }
                }
    }( a, b )



    override def flatMap[ A, B ]( a : FpLazyList[ A ] )
                                ( fn : A => FpLazyList[ B ] ) : FpLazyList[ B ] = StackSafe.selfCall[ FpLazyList[ A ], FpLazyList[ B ] ] {
        ( thisFn : FpLazyList[ A ] => StackSafe[ FpLazyList[ B ] ] ) =>
            ( lazyList : FpLazyList[ A ] ) =>
                lazyList match {
                    case FpLazyNil => Result( FpLazyNil )
                    case FpUnevaluatedLazyList( evalFn ) => Result( FpUnevaluatedLazyList( () => {
                        flatMap( evalFn() )( fn ).evaluate()
                    } ) )
                    case FpLazyListEval( tail : FpLazyList[ A ], head ) => Call.from {
                        thisFn( tail ).map( ( newTail : FpLazyList[ B ] ) => fn( head ) ++ newTail )
                    }
                }
    }( a )

    override def unit[ A ]( ele : A ) : FpLazyList[ A ] = FpLazyListEval( FpLazyNil, ele )
}
