package tasks.adts


import org.junit.*
import org.junit.Assert.*
import tasks.adts.SchoolModel.*
import u03.Sequences.Sequence
import u03.Optionals.Optional
import tasks.adts.SchoolModel.SchoolImpl.addTeacher
import tasks.adts.SchoolModel.SchoolImpl.setTeacherToCourse
import tasks.adts.SchoolModel.SchoolImpl.addCourse
import tasks.adts.SchoolModel.SchoolImpl.teacherByName
import java.{util => ju}
import tasks.adts.SchoolModel.SchoolImpl.courseByName

/* Tests should be clear, but note they are expressed independently of the 
   specific implementation
*/

class SchoolTest:

  def unwrapOptional[A](optional: Optional[A]): A = optional match
    case Optional.Just(a) => a
    case _                => throw ju.NoSuchElementException("Empty optional can't be unwrapped!")
  

  val school = School(Sequence.Nil(), Sequence.Nil())

  @Test def testEmpty() =
    assertEquals(Sequence.Nil(), school.courses)
    assertEquals(Sequence.Nil(), school.teachers)

  @Test def testAddTeacher() =
    val schoolWithTeacher = school.addTeacher("Viroli")
    
    assertEquals(Sequence.Cons(Teacher("Viroli", Sequence.Nil()), Sequence.Nil()), schoolWithTeacher.teachers)

  @Test def testAddCourse() =
    val schoolWithCourse = school.addCourse("PPS")
    
    assertEquals(Sequence.Cons(Course("PPS"), Sequence.Nil()), schoolWithCourse.courses)

  @Test def testAddCourseToTeacher() =
    val teacherName = "Viroli"
    val courseName = "PPS"

    val schoolWithTeacher = school.addTeacher(teacherName)
      .addCourse(courseName)
    
    val teacher = unwrapOptional(schoolWithTeacher.teacherByName(teacherName))
    val course = unwrapOptional(schoolWithTeacher.courseByName(courseName))
        
    assertEquals(Sequence.Cons(Teacher(teacherName, Sequence.Cons(course, Sequence.Nil())), Sequence.Nil()), schoolWithTeacher.setTeacherToCourse(teacher, course).teachers)

