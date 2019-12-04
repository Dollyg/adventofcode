package day2

trait Opcode {
  def check(index: Int): Boolean
  def execute(index: Int): (IntCode, Int)
}

case class GenericOpcode(intCode: IntCode,
                         number: Int,
                         action: (Int, Int) => Int)
    extends Opcode {

  override def check(index: Int): Boolean = intCode.at(index) == number

  override def execute(index: Int): (IntCode, Int) = {
    val index1 = intCode.at(index + 1)
    val index2 = intCode.at(index + 2)
    val outputIndex: Int = intCode.at(index + 3)
    val nextIndex = index + 4
    val result = action(intCode.at(index1), intCode.at(index2))
    (intCode.updated(outputIndex, result), nextIndex)
  }
}

case class Opcode99(intCode: IntCode) extends Opcode {
  override def check(index: Int): Boolean = intCode.at(index) == 99
  override def execute(index: Int): (IntCode, Int) =
    throw new RuntimeException("Terminated")
}

case class IntCode(code: List[Int]) {

  private val opCodes: List[Opcode] =
    List(
      GenericOpcode(this, 1, _ + _),
      GenericOpcode(this, 2, _ * _),
      Opcode99(this)
    )

  def at(index: Int): Int = code(index)
  def updated(index: Int, elem: Int): IntCode =
    IntCode(code.updated(index, elem))

  def calculate(index: Int): (IntCode, Int) = {
    val (updatedCode, nextIndex) = opCodes
      .collectFirst {
        case opcode if opcode.check(index) => opcode.execute(index)
      }
      .getOrElse((this, index + 1))
    println(
      s"At index $index, calculateCode= ${updatedCode}, NextIndex=${nextIndex}"
    )
    (updatedCode, nextIndex)
  }

}

object IntCode extends App {

  val code: List[Int] =
    List(1, 12, 2, 3, 1, 1, 2, 3, 1, 3, 4, 3, 1, 5, 0, 3, 2, 9, 1, 19, 1, 19, 5,
      23, 1, 9, 23, 27, 2, 27, 6, 31, 1, 5, 31, 35, 2, 9, 35, 39, 2, 6, 39, 43,
      2, 43, 13, 47, 2, 13, 47, 51, 1, 10, 51, 55, 1, 9, 55, 59, 1, 6, 59, 63,
      2, 63, 9, 67, 1, 67, 6, 71, 1, 71, 13, 75, 1, 6, 75, 79, 1, 9, 79, 83, 2,
      9, 83, 87, 1, 87, 6, 91, 1, 91, 13, 95, 2, 6, 95, 99, 1, 10, 99, 103, 2,
      103, 9, 107, 1, 6, 107, 111, 1, 10, 111, 115, 2, 6, 115, 119, 1, 5, 119,
      123, 1, 123, 13, 127, 1, 127, 5, 131, 1, 6, 131, 135, 2, 135, 13, 139, 1,
      139, 2, 143, 1, 143, 10, 0, 99, 2, 0, 14, 0)

  @scala.annotation.tailrec
  def calculate(intCode: IntCode, index: Int = 0): IntCode = {
    println(s"IntCode = ${intCode.code}, Index=$index")
    intCode.code match {
      case Nil                                 => intCode
      case _ if Opcode99(intCode).check(index) => intCode // terminate
      case _ =>
        val (updatedCode, nextIndex) = intCode.calculate(index)
        calculate(updatedCode, nextIndex)
    }
  }

  println(
    "Final=" + calculate(
      IntCode(List(1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50))
    )
  )
//  println("Final=" + calculate(IntCode(List(1, 0, 0, 0, 99))))
//  println("Final=" + calculate(IntCode(List(2, 3, 0, 3, 99))))
//  println(
//    "Final=" + calculate(IntCode(List(1, 1, 1, 4, 99, 5, 6, 0, 99)))
//  )
//  println("Final=" + calculate(IntCode(code)))

}
