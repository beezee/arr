package arr

trait Category[=>:[_, _]] {
  def compose[A, B, C](bc: B =>: C, ab: A =>: B): A =>: C
  def id[A]: A =>: A
}

object Category {

  def apply[=>:[_, _]](implicit c: Category[=>:]) = c

  implicit val scala = new Category[Function1] {
    def compose[A, B, C](bc: B => C, ab: A => B): A => C =
      bc.compose(ab)
    def id[A] = identity[A] _
  }

  object syntax {
    implicit class CategoryOps[=>:[_, _]: Category, A, B](ab: A =>: B) {
      def compose[C](ca: C =>: A): C =>: B =
        Category[=>:].compose(ab, ca)
      def andThen[C](bc: B =>: C): A =>: C =
        Category[=>:].compose(bc, ab)
    }
  }
}

trait CategoryLaws[=>:[_, _]] {
  implicit val category: Category[=>:]
  implicit val eval: Eval[=>:]

  import Category.syntax._
  import Eval.syntax._

  def assoc[A, B, C, D](
    ab: A =>: B, bc: B =>: C, cd: C =>: D): A => Boolean =
      (a: A) => cd.compose(bc).compose(ab)(a) == 
                cd.compose(bc.compose(ab))(a)

  def id[A, B](ab: A =>: B): A => Boolean =
    (a: A) => ab.compose(Category[=>:].id[A])(a) ==
              Category[=>:].id[B].compose(ab)(a)
}

object CategoryLaws {

  def apply[=>:[_, _]](implicit c: Category[=>:], e: Eval[=>:]) = 
    new CategoryLaws[=>:] {
      implicit val category = c
      implicit val eval = e
    }

  implicit val scala = CategoryLaws[Function1]
}
