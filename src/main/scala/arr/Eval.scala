package arr

trait Eval[=>:[_, _]] {
  def apply[A, B](ab: A =>: B)(a: A): B
}
object Eval {

  def apply[=>:[_, _]](implicit e: Eval[=>:]) = e

  implicit val fn1 = new Eval[Function1] {
    def apply[A, B](ab: A => B)(a: A) = ab(a)
  }

  object syntax {
    implicit class EvalOps[=>:[_, _]: Eval, A, B](arr: A =>: B) {
      def apply(a: A): B = Eval[=>:].apply(arr)(a)
    }
  }
}
