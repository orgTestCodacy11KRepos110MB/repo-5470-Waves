package com.wavesplatform.lang.miniev

sealed trait ComplexityLimit {
  def limitExceeded(spentComplexity: Long): Boolean
}

object ComplexityLimit {
  sealed abstract class Limit(maxComplexity: Long) extends ComplexityLimit {
    override def limitExceeded(spentComplexity: Long): Boolean = spentComplexity > maxComplexity
  }

  case class Partial(limit: Int) extends Limit(limit)
  case class Complete(limit: Int) extends Limit(limit)

  case object Unlimited extends ComplexityLimit {
    override def limitExceeded(spentComplexity: Long): Boolean = false
  }
}
