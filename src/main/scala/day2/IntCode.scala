package day2

case class IntCode(code: List[Int]) {

  def opcode1(index1: Int, index2: Int, outputIndex: Int): IntCode = {
    val sum = code(index1) + code(index2)
    IntCode(code.updated(outputIndex, sum))
  }

  def opcode2(index1: Int, index2: Int, outputIndex: Int): IntCode = {
    val product = code(index1) * code(index2)
    IntCode(code.updated(outputIndex, product))
  }

  def isOpcode1(index: Int): Boolean = code(index) == 1
  def isOpcode2(index: Int): Boolean = code(index) == 2
  def isOpcode99(index: Int): Boolean = code(index) == 99
  def at(index: Int): Int = code(index)

  def calculate(index: Int): (IntCode, Int) = {
    val dd = this match {
      case _ if isOpcode1(index) =>
        (opcode1(at(index + 1), at(index + 2), at(index + 3)), index + 4)

      case _ if isOpcode2(index) =>
        (opcode2(at(index + 1), at(index + 2), at(index + 3)), index + 4)

      case _ => (this, index + 1)
    }
    println(
      s"At index $index, calculateCode= ${dd._1.code}, NextIndex=${dd._2}"
    )
    dd
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
      case Nil                       => intCode
      case code if code(index) == 99 => intCode // terminate
      case _ =>
        val (updatedCode, nextIndex) = intCode.calculate(index)
        calculate(updatedCode, nextIndex)
    }
  }

//  println("****************" + calculate(IntCode(List(1, 0, 0, 0, 99))))
//  println("****************" + calculate(IntCode(List(2, 3, 0, 3, 99))))
//  println("****************" + calculate(IntCode(List(2, 4, 4, 5, 99, 0))))
//  println(
//    "****************" + calculate(IntCode(List(1, 1, 1, 4, 99, 5, 6, 0, 99)))
//  )
  println(calculate(IntCode(code)))

}
