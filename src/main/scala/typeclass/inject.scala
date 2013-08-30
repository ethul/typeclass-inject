package typeclass

import scalaz.{Coproduct, Free, Functor}, Free.Suspend
import scalaz.std.option._
import scalaz.syntax.std.option._

sealed abstract class Inject[Sub[_]: Functor, Sup[_]: Functor] {
  def inj[A](sub: Sub[A]): Sup[A]
  def prj[A](sup: Sup[A]): Option[Sub[A]]
}

sealed trait InjectInstances {
  implicit def reflexiveInjectInstance[F[_]: Functor] =
    new Inject[F, F] {
      def inj[A](sub: F[A]) = sub
      def prj[A](sup: F[A]) = sup.some
    }

  implicit def leftInjectInstance[F[_]: Functor, G[_]: Functor] =
    new Inject[F, ({type λ[α] = Coproduct[F, G, α]})#λ] {
      def inj[A](sub: F[A]) = Coproduct.leftc(sub)
      def prj[A](sup: ({type λ[α] = Coproduct[F, G, α]})#λ[A]) = sup.run.fold(_.some, _ => none)
    }

  implicit def rightInjectInstance[F[_]: Functor, G[_]: Functor, H[_]: Functor]
    (implicit I: Inject[F, G]) =
      new Inject[F, ({type λ[α] = Coproduct[H, G, α]})#λ] {
        def inj[A](sub: F[A]) = Coproduct.rightc(I.inj(sub))
        def prj[A](sup: ({type λ[α] = Coproduct[H, G, α]})#λ[A]) = sup.run.fold(_ => none, I.prj(_))
      }
}

sealed trait InjectFunctions {
  def inject[F[_]: Functor, G[_]: Functor, A](g: G[Free[F, A]])(implicit I: Inject[G, F]): Free[F, A] =
    Suspend[F, A](I.inj(g))

  def match_[F[_]: Functor, G[_]: Functor, A](fa: Free[F, A])(implicit I: Inject[G, F]): Option[G[Free[F, A]]] =
    fa.resume.fold(I.prj(_), _ => none)
}

object Inject extends InjectInstances with InjectFunctions
