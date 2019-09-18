package arr

trait Applicative[F[_], =>:[_, _]] {
  implicit val category: Category[=>:]
  implicit val functor: Functor[F, =>:, =>:]
  val smc: SMC[=>:]

  import Category.syntax._
  import Functor.syntax._

  def pure[A]: A =>: F[A]
  def <&>[A, B]: (F[A], F[B]) =>: F[(A, B)]
  def ap[A, B](f: Unit =>: F[A =>: B]): F[A] =>: F[B] =
    smc.carry(smc.terminal[F[A]])
      .andThen(smc.second[Unit, F[A =>: B], F[A]](f))
      .andThen(<&>)
      .andThen(smc.eval[A, B].map[F, =>:])
}
