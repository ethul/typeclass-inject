package typeclass

import scalaz.{Coproduct, Free, Functor}, Free.Suspend

sealed abstract class Inject[Sub[_] : Functor, Sup[_] : Functor] {
  def inj[A](sub: Sub[A]): Sup[A]
}

sealed trait InjectInstances {
  implicit def reflexiveInjectInstance[F[_] : Functor] =
    new Inject[F, F] {
      def inj[A](sub: F[A]) = sub
    }

  implicit def leftInjectInstance[F[_] : Functor, G[_] : Functor] =
    new Inject[F, ({type λ[α]=Coproduct[F, G, α]})#λ] {
      def inj[A](sub: F[A]) = Coproduct.leftc(sub)
    }

  implicit def rightInjectInstance[F[_] : Functor, G[_] : Functor, H[_] : Functor]
    (implicit I: Inject[F, G]) =
      new Inject[F, ({type λ[α]=Coproduct[H, G, α]})#λ] {
        def inj[A](sub: F[A]) = Coproduct.rightc(I.inj(sub))
      }
}

sealed trait InjectFunctions {
  def inject[F[_]: Functor, G[_]: Functor, A](g: G[Free[F, A]])(implicit I: Inject[G, F]): Free[F, A] =
    Suspend[F, A](I.inj(g))
}

object Inject extends InjectInstances with InjectFunctions
