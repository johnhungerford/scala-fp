package org.hungerford.fp.collections

import org.hungerford.fp.types.Monoid

import scala.annotation.tailrec

trait FpString extends Monoid[ FpString ] {
  def toFpList : FpList[ Char ]

  override def empty : FpString = FpString( FpList.empty )

  override def combine[ B ]( a : FpString, b : FpString ) : FpString = FpString( a.toFpList ++ b.toFpList )

  override def toString : String = toFpList.toList.mkString

  def length : Int = toFpList.length

  def reverse : FpString = FpString( toFpList.reverse )

  def flatMap( fn : Char => FpString ) : FpString = FpString( toFpList.flatMap( v => fn( v ).toFpList) )
  def flatMap[ B ]( fn : Char => FpList[ B ] ) : FpList[ B ] = toFpList.flatMap( fn )
  def map[ B ]( fn : Char => B ) : FpList[ B ] = toFpList.map( fn )

  def + ( a : Any ) : FpString = combine( FpString( a.toString ) )

  @tailrec
  private def trimEnd( str : FpList[ Char ] ) : FpList[ Char ] = str match {
    case FpNil => FpNil
    case FpList( next, ' ' ) => trimEnd( next )
    case v : FpList[ Char ] => v
  }

  def trim : FpString = FpString( trimEnd( trimEnd( toFpList ).reverse ).reverse )

}

object FpString {
  def apply( a : FpList[ Char ] ) : FpString = new FpString {
    override def toFpList : FpList[ Char ] = a
  }

  def apply( a : String ) : FpString = apply( FpList( a.toList ) )

  def unapply( fpString : FpString ) : Option[ String ] = Some( fpString.toString )

  implicit def stringToFpString( str : String ) : FpString = FpString( str )
}
