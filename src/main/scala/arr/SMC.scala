package arr

trait SMC[=>:[_, _]] {
  def fst[A, B]: (A, B) =>: A
  def snd[A, B]: (A, B) =>: B
  def swap[A, B]: (A, B) =>: (B, A)
  def terminal[A]: A =>: Unit
  def carry[A, B](f: A =>: B): A =>: (A, B)
  def first[A, B, C](f: A =>: B): (A, C) =>: (B, C)
  def second[A, B, C](f: A =>: B): (C, A) =>: (C, B)
  def eval[A, B]: (A, A =>: B) =>: B
}
