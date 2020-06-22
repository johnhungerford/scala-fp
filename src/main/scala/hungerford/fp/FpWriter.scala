package hungerford.fp

case class FpWriter[ T, A ]( value : T, context : Monoid[ A ] ) extends Monad[ FpWriter[ T, A ], T ] {
    override def flatMap[ C, D ]( fn : T => Monad[ C, D ] ) : Monad[ C, D ] =  {
        val newWriter = fn( value ).asInstanceOf[ FpWriter[ D, A ] ]
        FpWriter[ D, A ]( newWriter.value, context.combine( newWriter.context ).asInstanceOf[ Monoid[ A ] ] ).asInstanceOf[ Monad[ C, D ] ]
    }

    override def unit[ C >: FpWriter[ T, A ], B >: T ]( ele : B ) : Monad[ C, B ] = FpWriter( ele, context.empty.asInstanceOf[ Monoid[ A ] ] ).asInstanceOf[ Monad[ C, B ] ]
}

