package arr

trait NT[F[_], G[_], =>:[_, _]] {
  def apply[A]: F[A] =>: G[A]
}

trait Monad[F[_], =>:[_, _]] {
  import Category.syntax._
  import Functor.syntax._
  implicit val category: Category[=>:]
  implicit val functor: Functor[F, =>:, =>:]

  def point[A]: A =>: F[A]
  def join[A]: F[F[A]] =>: F[A]
  def bind[A, B](afb: A =>: F[B]): F[A] =>: F[B] =
    afb.map[F, =>:].andThen(join[B])
}

object Monad {
  type Id[A] = A

  type Scala[F[_]] = Monad[F, Function1]

  type Point[F[_], =>:[_, _]] = NT[Functor.Id, F, =>:]
  type Join[F[_], =>:[_, _]] = NT[Lambda[a => F[F[a]]], F, =>:]
  def apply[F[_], =>:[_, _]](p: Point[F, =>:], j: Join[F, =>:])(
    implicit c: Category[=>:], f: Functor[F, =>:, =>:]) =
    new Monad[F, =>:] {
      implicit val category = c
      implicit val functor = f
      def point[A] = p.apply[A]
      def join[A] = j.apply[A]
    }

  def scala[F[_]: Functor.Scala](p: Point[F, Function1], j: Join[F, Function1]) = 
    Monad[F, Function1](p, j)

  implicit val list = Monad.scala[List](
    new Point[List, Function1] {
      def apply[A]: Functor.Id[A] => List[A] = List(_: Functor.Id[A])
    },
    new Join[List, Function1] {
      def apply[A]: List[List[A]] => List[A] = (_: List[List[A]]).flatten
    })

  implicit val option = Monad.scala[Option](
    new Point[Option, Function1] {
      def apply[A]: Functor.Id[A] => Option[A] = Option(_: Functor.Id[A])
    },
    new Join[Option, Function1] {
      def apply[A]: Option[Option[A]] => Option[A] = (_: Option[Option[A]]).flatten
    })

  implicit val id = Monad.scala[Functor.Id](
    new Point[Functor.Id, Function1] {
      def apply[A]: Functor.Id[A] => Functor.Id[A] = identity[Functor.Id[A]] _
    },
    new Join[Functor.Id, Function1] {
      def apply[A]: Functor.Id[A] => Functor.Id[A] = identity[Functor.Id[A]] _
    })

  object syntax {
    import Category.syntax._
    implicit class MonadOps[F[_], =>:[_, _], A, B](afb: A =>: F[B])(implicit m: Monad[F, =>:]) {
      def bind: F[A] =>: F[B] = m.bind(afb)
      def >=>[C](bfc: B =>: F[C])(implicit c: Category[=>:]): A =>: F[C] =
        bfc.bind.compose(afb)
      def <=<[C](cfa: C =>: F[A])(implicit c: Category[=>:]): C =>: F[B] =
        afb.bind.compose(cfa)
    }
  }
}

trait MonadLaws[F[_], =>:[_, _]] {
  implicit val monad: Monad[F, =>:]
  implicit val category: Category[=>:]
  implicit val eval: Eval[=>:]

  import Category.syntax._
  import Eval.syntax._
  import Monad.syntax._

  def assoc[A, B, C, D](
    ab: A =>: F[B], bc: B =>: F[C], cd: C =>: F[D]): F[A] => Boolean =
      (a: F[A]) => ((cd <=< bc) <=< ab).bind(a) == 
                    (cd <=< (bc <=< ab)).bind(a)

  def id[A, B](ab: A =>: F[B]): F[A] => Boolean =
    (a: F[A]) => (ab <=< monad.point[A]).bind(a) ==
              (monad.point[B] <=< ab).bind(a)

}

object MonadLaws {
  def apply[F[_], =>:[_, _]](implicit
    c: Category[=>:], e: Eval[=>:],
    m: Monad[F, =>:]) = new MonadLaws[F, =>:] {
      implicit val monad = m
      implicit val category = c
      implicit val eval = e
    }

  implicit def scala[F[_]: Monad.Scala] = MonadLaws[F, Function1]
}
