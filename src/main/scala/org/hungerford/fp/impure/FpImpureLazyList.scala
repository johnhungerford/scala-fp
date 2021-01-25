package org.hungerford.fp.impure

import org.hungerford.fp.collections.{FpList, FpNil}
import org.hungerford.fp.recursion.{Call, Result, StackSafe}
import org.hungerford.fp.types.{MonadCovariant, MonadStatic}

sealed trait FpImpureList[ +T ] extends MonadCovariant[ FpImpureList, T ] {
    private[ impure ] val impure : FpImpure[ FpImpureEvaluatedList[ T ] ]

    def toList : FpList[ T ] = StackSafe.selfCall[ FpImpureList[ T ], FpList[ T ] ] {
        thisFn =>
            {
                case FpImpureNil => Result( FpNil )
                case FpImpureListEval( next, head ) => Call.from {
                    thisFn( next ).map( l => l + head )
                }
                case FpImpureUnevaluatedList( imp ) => Call.from {
                    thisFn( imp.run().getOrElse( FpImpureNil ) )
                }
            }
    }( this )

    def +[ B >: T ]( that : B ) : FpImpureList[ B ] = FpImpureList.combine( this, that )

    def ++[ B >: T ]( that : FpImpureList[ B ] ) : FpImpureList[ B ] = FpImpureList.combine( this, that )

    override def flatMap[ A, B ]( a : FpImpureList[ A ] )
                                ( fn : A => FpImpureList[ B ] ) : FpImpureList[ B ] = FpImpureList.flatMap( a )( fn )

    override def unit[ A ]( ele : A ) : FpImpureList[ A ] = FpImpureList.unit( ele )

    private def toStringInternal[ A ]( ll : FpImpureList[ A ] ) : String = StackSafe.selfCall[ FpImpureList[ A ], String ] {
        thisFn =>
        {
            case FpImpureNil => Result( FpImpureNil.toString )
            case FpImpureUnevaluatedList( _ ) => Result( "??" )
            case FpImpureListEval( tail, head ) => Call.from {
                thisFn( tail ).map( tailString => s"${head.toString} + ${tailString}" )
            }
        }
    }( ll )

    override def toString : String = toStringInternal( this )

}

case class FpImpureUnevaluatedList[ +T ]( override private[ impure ] val impure : FpImpure[ FpImpureEvaluatedList[ T ] ] ) extends FpImpureList[ T ] {
    override def equals( obj : Any ) : Boolean = obj match {
        case ll : FpImpureList[ _ ] => this.toList == ll.toList
        case _ => false
    }
}

trait FpImpureEvaluatedList[ +T ] extends FpImpureList[ T ] {
    private[ impure ] val impure : FpImpure[ FpImpureEvaluatedList[ T ] ] = FpImpure( this )
}

case object FpImpureNil extends FpImpureEvaluatedList[ Nothing ]

case class FpImpureListEval[ +T ]( tail : FpImpureList[ T ], head : T  ) extends FpImpureEvaluatedList[ T ] {
    override def equals( obj : Any ) : Boolean = obj match {
        case ll : FpImpureList[ _ ] => this.toList == ll.toList
        case _ => false
    }
}

object FpImpureList extends MonadStatic[ FpImpureList ] {

    def combine[ A ]( a : FpImpureList[ A ], b : A ) : FpImpureList[ A ] = FpImpureListEval( a, b )

    def combine[ A ]( a : FpImpureList[ A ], b : FpImpureList[ A ] ) : FpImpureList[ A ] = StackSafe.selfCall2[ FpImpureList[ A ], FpImpureList[ A ], FpImpureList[ A ] ] {
        ( thisFn : (FpImpureList[ A ], FpImpureList[ A ]) => StackSafe[FpImpureList[ A ] ] ) =>
            (listA, listB)  =>
                listA match {
                    case FpImpureNil => Result( listB )
                    case FpImpureUnevaluatedList( imp ) =>  listB match {
                        case FpImpureNil => Result( listA )
                        case _ => Result( FpImpureUnevaluatedList {
                            FpImpure( combine( imp.run().getOrElse( FpImpureNil ), listB ).impure.run().getOrElse( FpImpureNil ) )
                        } )
                    }
                    case FpImpureListEval( tail, head ) => Call.from {
                        thisFn( tail, listB ).map( ( ll : FpImpureList[ A ] ) => FpImpureListEval( ll, head ) )
                    }
                }
    }( a, b )



    override def flatMap[ A, B ]( a : FpImpureList[ A ] )
                                ( fn : A => FpImpureList[ B ] ) : FpImpureList[ B ] = StackSafe.selfCall[ FpImpureList[ A ], FpImpureList[ B ] ] {
        ( thisFn : FpImpureList[ A ] => StackSafe[ FpImpureList[ B ] ] ) =>
            ( lazyList : FpImpureList[ A ] ) =>
                lazyList match {
                    case FpImpureNil => Result( FpImpureNil )
                    case FpImpureUnevaluatedList( imp ) => Result( FpImpureUnevaluatedList(
                        FpImpure( flatMap( imp.run().getOrElse( FpImpureNil ) )( fn ).impure.run().getOrElse( FpImpureNil ) )
                    ) )
                    case FpImpureListEval( tail : FpImpureList[ A ], head ) => Call.from {
                        thisFn( tail ).map( ( newTail : FpImpureList[ B ] ) => fn( head ) ++ newTail )
                    }
                }
    }( a )

    override def unit[ A ]( ele : A ) : FpImpureList[ A ] = FpImpureListEval( FpImpureNil, ele )
}
