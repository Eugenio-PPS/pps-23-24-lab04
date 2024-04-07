package tasks.adts

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex
    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:

    case class ComplexNumber(re: Double, im: Double)

    // Change assignment below: should probably define a case class and use it?
    opaque type Complex = ComplexNumber
    def complex(re: Double, im: Double): Complex = ComplexNumber(re, im)
    extension (complex: Complex)
      def re(): Double = complex match
        case ComplexNumber(re, im) => re
      def im(): Double = complex match
        case ComplexNumber(re, im) => im
      def sum(other: Complex): Complex = (complex, other) match
        case (ComplexNumber(re1, im1), ComplexNumber(re2, im2)) => ComplexNumber(re1 + re2, im1 + im2)
      def subtract(other: Complex): Complex = (complex, other) match
        case (ComplexNumber(re1, im1), ComplexNumber(re2, im2)) => ComplexNumber(re1 - re2, im1 - im2)
      def asString(): String = complex match
        case ComplexNumber(re, _) if im == 0  => s"$re"
        case ComplexNumber(_, im) if re == 0  => s"${im}i"
        case ComplexNumber(re, im) if im > 0  => s"$re + ${im}i"
        case ComplexNumber(re, im)            => s"$re - ${im.abs}i"
package tasks.adts
import u03.Sequences.*
import u03.Optionals.*
import u02.AlgebraicDataTypes.Person
import u03.Sequences.Sequence.filter

/*  Exercise 2: 
 *  Implement the below trait, and write a meaningful test.
 *  Suggestion: 
 *  - reuse Sequences and Optionals as imported above
 *  - Course is a simple case classes with just the name
 *  - Teacher is a case class with name and sequence of courses
 *  - School is a case class with (sequences of) teachers and courses
 *  - add/set methods below create the new school 
 */

object SchoolModel:

  trait SchoolModule:
    type School
    type Teacher
    type Course
    extension (school: School)
      def addTeacher(name: String): School
      def addCourse(name: String): School
      def teacherByName(name: String): Optional[Teacher]
      def courseByName(name: String): Optional[Course]
      def nameOfTeacher(teacher: Teacher): String
      def nameOfCourse(teacher: Course): String
      def setTeacherToCourse(teacher: Teacher, course: Course): School
      def coursesOfATeacher(teacher: Teacher): Sequence[Course]

  case class Course(name: String)
  case class Teacher(name: String, courses: Sequence[Course])
  case class School(teachers: Sequence[Teacher], courses: Sequence[Course])

  object SchoolImpl extends SchoolModule:
    type School = tasks.adts.SchoolModel.School
    type Teacher = tasks.adts.SchoolModel.Teacher
    type Course = tasks.adts.SchoolModel.Course

    extension (school: School)
      def addTeacher(name: String) = school match
        case School(teachers, courses) => School(Sequence.Cons(Teacher(name, Sequence.Nil()), teachers), courses)
      
      def addCourse(name: String) = school match
        case School(teachers, courses) => School(teachers, Sequence.Cons(Course(name), courses))
      
      def teacherByName(name: String) = 
        filter(school.teachers)(_.name == name) match
          case Sequence.Cons(found, Sequence.Nil()) => Optional.Just(found)
          case _                                    => Optional.Empty()
        
      def courseByName(name: String): Optional[Course] = 
        filter(school.courses)(_.name == name) match
          case Sequence.Cons(found, Sequence.Nil()) => Optional.Just(found)
          case _                                    => Optional.Empty()
      def nameOfTeacher(teacher: Teacher) = teacher.name
      def nameOfCourse(course: Course) = course.name
      def setTeacherToCourse(teacher: Teacher, course: Course) =
        def map[A, B](mapping: A => B)(sequence: Sequence[A]): Sequence[B] = sequence match
          case Sequence.Cons(h, t)  => Sequence.Cons(mapping(h), map(mapping)(t))
          case _                    => Sequence.Nil()
        
        School(
          map[Teacher, Teacher](x => if x == teacher then Teacher(x.name, Sequence.Cons(course, x.courses)) else x)(school.teachers),
          school.courses
        )
        
      def coursesOfATeacher(teacher: Teacher) = teacher.courses
package tasks.adts

import u03.Sequences.*
import u03.Optionals.*

/*  Exercise 3: 
 *  Implement a Stack ADT
 *  Suggestion: 
 *  - push adds an element and returns the new stack
 *  - pop returns:
 *  -- empty optional is stack is empty
 *  -- a pair of top of the stack and the new stack after removal if not empty
 */
object Ex3Stacks:

  trait StackADT:
    type Stack[A]
    def empty[A]: Stack[A] // factory
    extension [A](stack: Stack[A])
      def push(a: A): Stack[A]
      def pop(a: A): Optional[(A, Stack[A])]
      def asSequence(): Sequence[A]
  
  object StackImpl extends StackADT:
    opaque type Stack[A] = Sequence[A]
    def empty[A]: Stack[A] = Sequence.Nil()
    extension [A](stack: Stack[A])
      def push(a: A): Stack[A] =
        Sequence.Cons(a, stack)
      def pop(a: A): Optional[(A, Stack[A])] = stack match
        case Sequence.Cons(h, t)   => Optional.Just(h, t)
        case _                     => Optional.Empty()
      def asSequence(): Sequence[A] = stackpackage tasks.monads

import u04.monads.Monads.Monad
import u04.monads.Monads.Monad

/**
  * Exercise 6: 
    This module contains the implementation of a Try monad, which is a monad that 
    represents a computation that may fail. 
    Try to follow these steps:
    - Look at the implementation of Try, that is similar to the one of Optional
    - Try go define the Monad instance for Try
      - flatMap should consider only the Success case
      - in case of Failure, it should return the exception (fail fast)
    - Verify that the main works as expected
  */
object Ex6TryModel:
  private enum TryImpl[A]:
    case Success(value: A)
    case Failure(exception: Throwable)

  opaque type Try[A] = TryImpl[A]

  def success[A](value: A): Try[A] = TryImpl.Success(value)
  def failure[A](exception: Throwable): Try[A] = TryImpl.Failure(exception)
  def exec[A](expression: => A): Try[A] = try success(expression) catch failure(_)

  extension [A](m: Try[A]) 
    def getOrElse[B >: A](other: B): B = m match
      case TryImpl.Success(value) => value
      case TryImpl.Failure(_) => other

  given Monad[Try] with
    override def unit[A](value: A): Try[A] = TryImpl.Success(value)
    extension [A](m: Try[A]) 

      override def flatMap[B](f: A => Try[B]): Try[B] = m match
        case TryImpl.Success(value)     => f(value)
        case TryImpl.Failure(exception) => TryImpl.Failure(exception)
      
@main def main: Unit = 
  import Ex6TryModel.*

  val result = for 
    a <- success(10)
    b <- success(30)
  yield a + b

  assert(result.getOrElse(-1) == 40)

  val result2 = for 
    a <- success(10)
    b <- failure(new RuntimeException("error"))
    c <- success(30)
  yield a + c

  assert(success(20).map(_ + 10).getOrElse(-1) == 30)
  assert(result2.getOrElse(-1) == -1)

  val result3 = for
    a <- exec(10)
    b <- exec(throw new RuntimeException("error"))
    c <- exec(30)
  yield a + cpackage u04lab
import u03.Sequences.* 
import Sequence.*

/*  Exercise 4: 
 *  - Complete the implementation of ad-hoc polymorphic sumAll, using summable.sum and summable.zero
 *  - Write givens also for Summable[Double], Summable[String]
 *  - Uncomment in the main and check if everything works
 */

object Ex4Summables:

  def sumAllInt(seq: Sequence[Int]): Int = seq match
    case Cons(h, t) => h + sumAllInt(t)
    case _ => 0

  trait Summable[A]:
    def sum(a1: A, a2: A): A
    def zero: A

  def sumAll[A: Summable](seq: Sequence[A]): A =
    val summable = summon[Summable[A]]
    seq match
      case Sequence.Cons(head, tail) => summable.sum(head, sumAll[A](tail))
      case Sequence.Nil() => summable.zero

  given Summable[Int] with
    def sum(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0

  given Summable[Double] with
    def sum(a1: Double, a2: Double): Double = a1 + a2

    def zero: Double = 0

  given Summable[String] with
    def sum(a1: String, a2: String): String = a1 + a2

    def zero: String = ""

  // write givens for Summable[Double] and Summable[String]

  @main def trySummables =
    val si = Cons(10, Cons(20, Cons(30, Nil())))  
    println:
      sumAllInt(si) // 60

    /* uncomment from here   */

    println:
      sumAll(si) // 60

    val sd = Cons(10.0, Cons(20.0, Cons(30.0, Nil())))  
    println:
      sumAll(sd) // 60.0

    val ss = Cons("10", Cons("20", Cons("30", Nil())))  
    println:
      sumAll(ss) // "102030"

    /**/

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
