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
