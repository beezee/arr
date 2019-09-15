package arr 

import org.scalacheck.{Arbitrary, Cogen, Properties}
import org.scalacheck.Prop.forAll

object FunctorProperties {
  def apply[F[_], =>:[_, _], =>::[_, _]](
    implicit l: FunctorLaws[F, =>:, =>::], aa: ArbArr[=>:],
      fa: Arbitrary[F[Int]]) = {
    import aa._
    ("id" |: forAll { 
      (fa: F[Int]) => l.id[Int].andThen(t => t._1 == t._2)(fa)
    }) && 
    ("monotone" |: forAll {
      (is: (Int, Int) =>: String, li: Int =>: (Int, Int), in: F[Int]) => {
        val (rl, rr) = l.monotone(is, li)(in)
        rl == rr
      }
    })
  }

  def scala[F[_]](
    implicit l: FunctorLaws[F, Function1, Function1], fa: Arbitrary[F[Int]]) =
      FunctorProperties[F, Function1, Function1]
}

object ScalaFunctorSpec extends Properties("Scala Functors") {
  property("List") = FunctorProperties.scala[List]
  property("Option") = FunctorProperties.scala[Option]
  property("Id") = FunctorProperties.scala[Functor.Id]
}
