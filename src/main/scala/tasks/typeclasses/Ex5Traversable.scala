package u04lab
import u03.Sequences.*
import Sequence.*
import u03.Optionals.Optional

/*  Exercise 5: 
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A] 
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:

  trait Traversable[A[_]]:
    def traverse[T, R](f: T => R)(x: A[T]): Unit

  given Traversable[Sequence] with
    def traverse[T, R](f: T => R)(x: Sequence[T]): Unit = x match
      case Sequence.Cons(h, t) =>
        f(h)
        traverse(f)(t)
      case _                   => ()

  given Traversable[Optional] with
    def traverse[T, R](f: T => R)(x: Optional[T]): Unit = x match
      case Optional.Just(v) => f(v)
      case _                => ()

  def log[A](a: A): Unit = println("The next element is: "+a)

  def logAll[A[_]](x: A[Any])(using t: Traversable[A]): Unit = t.traverse(y => log(y))(x)

@main def test(): Unit =
  u04lab.Ex5Traversable.logAll(Optional.Just(6))
  u04lab.Ex5Traversable.logAll(Sequence.Cons(6, Sequence.Cons(3, Sequence.Nil())))
