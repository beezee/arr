package arr 

import org.scalacheck.{Arbitrary, Cogen, Properties}
import org.scalacheck.Prop.forAll

trait ArbPoint[F[_]] {
  implicit def arbFA[A](implicit a: Arbitrary[A]): Arbitrary[F[A]]
}

object ArbPoint {
  implicit def monad[F[_], =>:[_, _]](implicit m: Monad[F, =>:], e: Eval[=>:]) =
    new ArbPoint[F] {
      import Eval.syntax._
      implicit def arbFA[A](implicit a: Arbitrary[A]) =
        Arbitrary { a.arbitrary.map(m.point[A](_)) }
    }
}

object MonadProperties {
  def apply[F[_], =>:[_, _]](
    implicit l: MonadLaws[F, =>:], aa: ArbArr[=>:], ap: ArbPoint[F]) = {
    import aa._
    import ap._
    ("id" |: forAll { 
      (fa: Int =>: F[String], in: F[Int]) => l.id(fa)(in)
    }) && 
    ("assoc" |: forAll {
      (li: Int =>: F[List[Int]], is: List[Int] =>: F[String], 
       sos: String =>: F[Option[Int]], in: F[Int]) =>
        l.assoc(li, is, sos)(in)
      })
  }

  def scala[F[_]](
    implicit l: MonadLaws[F, Function1], ap: ArbPoint[F], fa: Arbitrary[F[Int]]) =
      MonadProperties[F, Function1]
}

object ScalaMonadSpec extends Properties("Scala Monads") {
  property("List") = MonadProperties.scala[List]
  property("Option") = MonadProperties.scala[Option]
  property("Id") = MonadProperties.scala[Functor.Id]
}

