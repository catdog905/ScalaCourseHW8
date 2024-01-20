package task1.hierarchy

import task1._

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait Apply[F[_]] extends Functor[F] {
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
}

trait Applicative[F[_]] extends Apply[F] {
  def pure[A](a: A): F[A]
}

trait FlatMap[F[_]] extends Apply[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}

trait Monad[F[_]] extends FlatMap[F] with Applicative[F] {
  def pure[A](a: A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}

object TypeClasses {
  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      case Leaf(value)         => Leaf(f(value))
    }
  }

  implicit val treeApply: Apply[Tree] = new Apply[Tree] {
    override def ap[A, B](ff: Tree[A => B])(fa: Tree[A]): Tree[B] = (fa, ff) match {
      case (Branch(left, right), Branch(leftF, rightF)) => Branch(ap(leftF)(left), ap(rightF)(right))
      case (Branch(left, right), f)                     => Branch(ap(f)(left), ap(f)(right))
      case (f, Branch(left, right))                     => Branch(ap(left)(f), ap(right)(f))
      case (Leaf(value), Leaf(f))                       => Leaf(f(value))
    }

    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = treeFunctor.map(fa)(f)
  }

  implicit val treeApplicative: Applicative[Tree] = new Applicative[Tree] {
    override def pure[A](a: A): Tree[A] = Leaf(a)

    override def ap[A, B](ff: Tree[A => B])(fa: Tree[A]): Tree[B] = treeApply.ap(ff)(fa)

    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = treeFunctor.map(fa)(f)
  }

  implicit val treeFlatMap: FlatMap[Tree] = new FlatMap[Tree] {
    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
      case Leaf(value)         => f(value)
    }

    override def ap[A, B](ff: Tree[A => B])(fa: Tree[A]): Tree[B] = treeApply.ap(ff)(fa)

    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = treeFunctor.map(fa)(f)
  }

  implicit val treeMonad: Monad[Tree] = new Monad[Tree] {
    override def pure[A](a: A): Tree[A] = treeApplicative.pure(a)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = treeFlatMap.flatMap(fa)(f)

    override def ap[A, B](ff: Tree[A => B])(fa: Tree[A]): Tree[B] = treeApply.ap(ff)(fa)

    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = treeFunctor.map(fa)(f)
  }
}
