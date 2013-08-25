package typeclass

import scalaz.{Coproduct, Free, Functor}, Free.Return
import Inject._

sealed trait Test1Algebra[A]
case class Test1[A](keys: Seq[String], h: Int => A) extends Test1Algebra[A]

sealed trait Test1AlgebraInstances {
  implicit def test1AlgebraAFunctor: Functor[Test1Algebra] =
    new Functor[Test1Algebra] {
      def map[A, B](a: Test1Algebra[A])(f: A => B): Test1Algebra[B] = a match {
        case Test1(k, h) => Test1(k, x => f(h(x)))
      }
    }
}

sealed trait Test1AlgebraFunctions {
  def test1[F[_] : Functor](keys: Seq[String])(implicit I: Inject[Test1Algebra, F]): Free[F, Int] =
    inject[F, Test1Algebra, Int](Test1(keys, Return(_)))
}

object Test1Algebra extends Test1AlgebraInstances with Test1AlgebraFunctions



sealed trait Test2Algebra[A]
case class Test2[A](keys: Seq[String], h: Int => A) extends Test2Algebra[A]

sealed trait Test2AlgebraInstances {
  implicit def test2AlgebraAFunctor: Functor[Test2Algebra] =
    new Functor[Test2Algebra] {
      def map[A, B](a: Test2Algebra[A])(f: A => B): Test2Algebra[B] = a match {
        case Test2(k, h) => Test2(k, x => f(h(x)))
      }
    }
}

sealed trait Test2AlgebraFunctions {
  def test2[F[_] : Functor](keys: Seq[String])(implicit I: Inject[Test2Algebra, F]): Free[F, Int] =
    inject[F, Test2Algebra, Int](Test2(keys, Return(_)))
}

object Test2Algebra extends Test2AlgebraInstances with Test2AlgebraFunctions



sealed trait Test3Algebra[A]
case class Test3[A](keys: Seq[String], h: Int => A) extends Test3Algebra[A]

sealed trait Test3AlgebraInstances {
  implicit def test3AlgebraAFunctor: Functor[Test3Algebra] =
    new Functor[Test3Algebra] {
      def map[A, B](a: Test3Algebra[A])(f: A => B): Test3Algebra[B] = a match {
        case Test3(k, h) => Test3(k, x => f(h(x)))
      }
    }
}

sealed trait Test3AlgebraFunctions {
  def test3[F[_] : Functor](keys: Seq[String])(implicit I: Inject[Test3Algebra, F]): Free[F, Int] =
    inject[F, Test3Algebra, Int](Test3(keys, Return(_)))
}

object Test3Algebra extends Test3AlgebraInstances with Test3AlgebraFunctions



object Test {
  import Test1Algebra._, Test2Algebra._, Test3Algebra._

  type C0[A] = Coproduct[Test1Algebra, Test2Algebra, A]
  type C1[A] = Coproduct[Test3Algebra, C0, A]
  type T[A] = C1[A]

  val res =
    for {
      a <- test1[T]("aa" :: Nil)
      b <- test2[T]("bb" :: Nil)
      c <- test3[T]("cc" :: Nil)
    } yield (a, b, c)
}
