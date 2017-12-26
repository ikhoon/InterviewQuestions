package net.cakesolutions.interview

import org.scalatest.{Matchers, WordSpec}
import examples.MonoidExamples._
import instances._

/**
  * This test will run when you have a working foldable list
  */
class FoldableSpec extends WordSpec with Matchers {

  def foldmap[F[_]: Foldable, M: Monoid, A](fa: F[A])(f: A => M): M =
    implicitly[Foldable[F]].foldMap(fa)(f)

  "Example foldables" should {
    "have have the desired value for standard fold examples" in {
      val ints = List(1, 2, 3)
      val strings = List("lol", "foo", "bar")

      foldmap(ints)(identity) shouldBe 6
      foldmap(ints)(i => (i, i)) shouldBe (6, 6)
      foldmap(strings)(t => t + "lol") shouldBe "lollolfoololbarlol"
    }
  }
}
