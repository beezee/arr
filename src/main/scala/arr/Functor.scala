package arr

trait Functor[F[_], =>:[_, _], =>::[_, _]] {
  def map[A, B](f: A =>: B): F[A] =>:: F[B]
}

object Functor {
  type Id[A] = A

  trait Resolve[=>:[_, _], =>::[_, _]] {
    type RF[F[_]] = Functor[F, =>:, =>::]
    def apply[F[_]](implicit f: RF[F]) = f
    trait Map[F[_]] {
      val functor: RF[F]
      def apply[A, B](f: A =>: B): F[A] =>:: F[B] =
        functor.map(f)
    }
    def map[F[_]](implicit f: RF[F]) = new Map[F] { val functor = f }
  }
  def apply[=>:[_, _], =>::[_, _]] = new Resolve[=>:, =>::] {}
  def endo[=>:[_, _]] = new Resolve[=>:, =>:] {}

  val scala = endo[Function1]

  type Scala[F[_]] = Functor[F, Function1, Function1]

  implicit val list = new Functor.Scala[List] {
    def map[A, B](f: A => B) = (_: List[A]).map(f)
  }
  implicit val option = new Functor.Scala[Option] {
    def map[A, B](f: A => B) = (_: Option[A]).map(f)
  }
  implicit val id = new Functor.Scala[Id] {
    def map[A, B](f: A => B) = f(_: A)
  }

  object syntax {
    implicit class FunctorOps[=>:[_, _], A, B](ab: A =>: B) {
      def map[F[_], =>::[_, _]](implicit f: Functor[F, =>:, =>::]): F[A] =>:: F[B]  = f.map(ab)
    }
  }
}

trait FunctorLaws[F[_], =>:[_, _], =>::[_, _]] {
  implicit val domain: Category[=>:]
  implicit val codomain: Category[=>::]
  implicit val eval: Eval[=>::]
  implicit val functor: Functor[F, =>:, =>::]

  import Category.syntax._
  import Eval.syntax._
  import Functor.syntax._

  def id[A]: F[A] => Boolean =
    (fa: F[A]) => codomain.id[F[A]](fa) == functor.map(domain.id[A])(fa)

  def monotone[A, B, C](bc: B =>: C, ab: A =>: B): F[A] => Boolean =
    (fa: F[A]) => bc.compose(ab).map[F, =>::].apply(fa) ==
                  bc.map[F, =>::].compose(ab.map[F, =>::])(fa)
}

object FunctorLaws {
  def apply[F[_], =>:[_, _], =>::[_, _]](implicit
    d: Category[=>:], cd: Category[=>::], e: Eval[=>::],
    f: Functor[F, =>:, =>::]) = new FunctorLaws[F, =>:, =>::] {
      implicit val domain = d
      implicit val codomain = cd
      implicit val eval = e
      implicit val functor = f
    }

  implicit def scala[F[_]: Functor.Scala] = FunctorLaws[F, Function1, Function1]
}
