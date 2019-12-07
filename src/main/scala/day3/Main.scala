package day3

sealed trait Direction
object Direction {
  case object R extends Direction
  case object L extends Direction
  case object U extends Direction
  case object D extends Direction
}

case class Move(direction: Direction, step: Int)

case class Coordinate(x: Int, y: Int) {
  def after(move: Move): Coordinate = move.direction match {
    case Direction.L => Coordinate(x - move.step, y)
    case Direction.R => Coordinate(x + move.step, y)
    case Direction.U => Coordinate(x, y + move.step)
    case Direction.D => Coordinate(x, y - move.step)
  }
}

case class Line(coordinate1: Coordinate, coordinate2: Coordinate) {

  def intersection(line: Line): Coordinate = {
    val (x1, y1) = (coordinate1.x, coordinate1.y)
    val (x2, y2) = (coordinate2.x, coordinate2.y)
    val (x3, y3) = (line.coordinate1.x, line.coordinate1.y)
    val (x4, y4) = (line.coordinate2.x, line.coordinate2.y)

    val px = ((((x1 * y2) - (y1 * x2)) * (x3 - x4)) - ((x1 - x2) * ((x3 * y4) - (y3 * x4)))) /
      (((x1 - x2) * (y3 - y4)) - ((y1 - y2) * (x3 - x4)))
    val py = ((((x1 * y2) - (y1 * x2)) * (y3 - y4)) - ((y1 - y2) * ((x3 * y4) - (y3 * x4)))) /
      (((x1 - x2) * (y3 - y4)) - ((y1 - y2) * (x3 - x4)))

    Coordinate(px, py)
  }

}

case class WireLine(lines: List[Line]) {
  def intersection(other: WireLine): List[Coordinate] = {
    for {
      line1 <- lines
      line2 <- other.lines
    } yield {
      line1.intersection(line2)
    }
  }
}

case class Wire(path: List[Move]) {
  def line: WireLine = {
    var lastCoordinate = Coordinate(0, 0)
    WireLine(path.map { move =>
      val p1 = lastCoordinate
      val p2 = lastCoordinate.after(move)
      lastCoordinate = p2
      Line(p1, p2)
    })
  }
}

case class Grid(wires: List[Wire]) {

  val wireLines: List[WireLine] = wires.map(_.line)
}

object Main extends App {
  private val wire1 = Wire(
    List(
      Move(Direction.R, 8),
      Move(Direction.U, 5),
      Move(Direction.L, 5),
      Move(Direction.D, 3)
    )
  )
  private val wire2 = Wire(
    List(
      Move(Direction.U, 7),
      Move(Direction.R, 6),
      Move(Direction.D, 4),
      Move(Direction.L, 4)
    )
  )
  val wires = List(wire1, wire2)
  Grid(wires).wireLines.foreach(println)

}
