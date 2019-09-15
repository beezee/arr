package arr 

import org.scalacheck.{Arbitrary, Cogen, Properties}
import org.scalacheck.Prop.forAll


trait ArbArr[=>:[_, _]] {
  implicit def arb[A, B](
    implicit aa: Cogen[A], ab: Arbitrary[B]): Arbitrary[A =>: B]
}

object ArbArr {

  def arbitrary[=>:[_, _]](implicit aa: ArbArr[=>:]) = aa

  implicit val scala = new ArbArr[Function1] {
    implicit def arb[A, B](
      implicit aa: Cogen[A], ab: Arbitrary[B]): Arbitrary[A => B] =
        Arbitrary.arbFunction1[A, B]
  }
}

object CategoryProperties {
  def apply[=>:[_, _]](implicit l: CategoryLaws[=>:], aa: ArbArr[=>:]) = {
    import aa._
    ("assoc" |: forAll { 
      (li: List[Int] =>: Int, is: Int =>: String, 
       sos: String =>: Option[Int], in: List[Int]) =>
        l.assoc(li, is, sos)(in)
    }) && 
    ("id" |: forAll {
      (li: List[Int] =>: Int, in: List[Int]) => l.id(li)(in)
    })
  }
}

object ScalaCategorySpec extends Properties("Scala Category") {
  property("Scala Category") = CategoryProperties[Function1]
}
