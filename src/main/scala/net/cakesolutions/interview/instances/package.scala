package net.cakesolutions.interview

import scala.annotation.tailrec
import scala.collection.immutable

package object instances extends LowPriorityInstance {

  // TODO: This is where you should define your work
  /** TODO: Define Functor for List */
  implicit val listFunctor: Functor[List] = new Functor[List] {
    def fmap[A, B](fa: List[A])(f: A => B): List[B] = fa match {
      case x :: xs => f(x) :: fmap(xs)(f)
      case Nil => Nil
    }
  }

  /** TODO: Define Foldable for List */
  implicit val listFoldable: Foldable[List] = new Foldable[List] {
    def foldr[A, B](fa: List[A])(z: => B)(f: A => B => B): B = fa match {
      case x :: xs => f(x)(foldr(xs)(z)(f))
      case _ => z
    }
  }

  /** TODO: Define Traversable for List. */
  implicit lazy val listTraversable: Traversable[List] = new Traversable[List] {
    def traverse[A, B, F[_] : Applicative](ta: List[A])(k: A => F[B]): F[List[B]] = {
      val F = implicitly[Applicative[F]]
      ta match {
        case x :: xs =>
          F.liftA2(k(x))(traverse(xs)(k))(y => ys => y :: ys)
        case _ => F.pure(Nil)
      }

    }

    def fmap[A, B](fa: List[A])(f: A => B): List[B] = listFunctor.fmap(fa)(f)

    def foldr[A, B](fa: List[A])(z: => B)(f: A => B => B): B = listFoldable.foldr(fa)(z)(f)
  }

}

