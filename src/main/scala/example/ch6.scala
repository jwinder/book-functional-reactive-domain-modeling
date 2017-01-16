package example.ch6

import scalaz._
import scalaz.syntax.all._
import scalaz.syntax.std.all._

// q: how to compose multiple effects, e.g. \/ and Future?
// answer: monad transformers

object MonadTransformerIllustration {
  type Valid[A] = String \/ A

  def naiveSum(a: Int, b: Option[Int], c: Valid[Int]): Valid[Option[Int]] = {
    for {
      c <- c
    } yield for {
      b <- b
    } yield a + b + c
  }

  type Response[A] = OptionT[Valid, A]

  object Response {
    def apply[A](a: A): Response[A] = a.point[Response]
    def apply[A](a: Valid[A]): Response[A] = OptionT[Valid, A](a.map(_.some))
    def apply[A](a: Option[A]): Response[A] = OptionT[Valid, A](a.right)
  }

  def sum[A: Semigroup](a: A, b: Option[A], c: Valid[A]): Valid[Option[A]] = {
    // val result: Response[A] =  for {
    //   a <- Response(a)
    //   b <- Response(b)
    //   c <- Response(c)
    // } yield a |+| b |+| c

    // result.run

    (Response(a) |@| Response(b) |@| Response(c))(_ |+| _ |+| _).run
  }
}
