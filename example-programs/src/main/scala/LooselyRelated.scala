package ltbs.uniform.examples

trait LooselyRelatedTC[A]

trait LooselyRelated

object LooselyRelated {
  def instance[A] = new LooselyRelatedTC[A] {}
}

trait LooselyRelatedInstances {

  /** All tuples are automatically loosely related */
  implicit def looseTuple[A <: { def _1: Any }] = LooselyRelated.instance[A]

  /** Anything that extends LooselyRelated is loosely related */  
  implicit def looseExplicit[A <: LooselyRelated] = LooselyRelated.instance[A]
}
