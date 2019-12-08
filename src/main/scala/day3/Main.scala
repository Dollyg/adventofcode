package day3

import scala.io.Source
import scala.util.control.NonFatal

sealed trait Direction
object Direction {
  case object R extends Direction
  case object L extends Direction
  case object U extends Direction
  case object D extends Direction

  def from(str: String): Direction = str match {
    case "R" => R
    case "L" => L
    case "U" => U
    case "D" => D
  }
}

case class Move(direction: Direction, step: Int)
object Move {
  def from(str: String): Move = {
    val (dir, step) = str.splitAt(1)
    Move(Direction.from(dir), step.toInt)
  }
}

case class Coordinate(x: Int, y: Int) {
  def after(move: Move): Coordinate = move.direction match {
    case Direction.L => Coordinate(x - move.step, y)
    case Direction.R => Coordinate(x + move.step, y)
    case Direction.U => Coordinate(x, y + move.step)
    case Direction.D => Coordinate(x, y - move.step)
  }

  def distance(p2: Coordinate): Double =
    Math.sqrt(Math.pow(x - p2.x, 2) + Math.pow(y - p2.y, 2))

  def manhattanDistance(coordinate: Coordinate): Int =
    Math.abs(x - coordinate.x) + Math.abs(y - coordinate.y)

  def isNot0_0: Boolean = x != 0 && y != 0
}

object Coordinate {
  def closestTo0_0(coords: List[Coordinate]): Int = {
    coords.foldLeft(Int.MaxValue) { (dist, coord) =>
      val newDist = coord.manhattanDistance(_0_0)
      println("Coordinate=" + coord + " dist=" + newDist)
      if (newDist < dist) newDist else dist
    }
  }

  def _0_0: Coordinate = Coordinate(0, 0)
}

case class Line(coordinate1: Coordinate, coordinate2: Coordinate) {

  def contains(point: Coordinate): Boolean =
    coordinate1.distance(point) + coordinate2.distance(point) ==
      coordinate1.distance(coordinate2)

  def intersection(line: Line): Option[Coordinate] = {
    val (x1, y1) = (coordinate1.x, coordinate1.y)
    val (x2, y2) = (coordinate2.x, coordinate2.y)
    val (x3, y3) = (line.coordinate1.x, line.coordinate1.y)
    val (x4, y4) = (line.coordinate2.x, line.coordinate2.y)

    try {
      val px = ((((x1 * y2) - (y1 * x2)) * (x3 - x4)) - ((x1 - x2) * ((x3 * y4) - (y3 * x4)))) /
        (((x1 - x2) * (y3 - y4)) - ((y1 - y2) * (x3 - x4)))
      val py = ((((x1 * y2) - (y1 * x2)) * (y3 - y4)) - ((y1 - y2) * ((x3 * y4) - (y3 * x4)))) /
        (((x1 - x2) * (y3 - y4)) - ((y1 - y2) * (x3 - x4)))

      val coordinate = Coordinate(px, py)
      if (this.contains(coordinate) && line.contains(coordinate))
        Some(coordinate)
      else None
    } catch {
      case NonFatal(_: ArithmeticException) => None
    }
  }

}

case class WireLine(lines: List[Line]) {
  def intersection(other: WireLine): List[Coordinate] = {
    val d = for {
      line1 <- lines
      line2 <- other.lines
    } yield {
      line1.intersection(line2)
    }

    d.collect {
      case Some(x) if x.isNot0_0 => x
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

case class Grid(wire1: Wire, wire2: Wire) {
  def distanceToClosest(): Int = {
    val intersections = wire1.line.intersection(wire2.line)
    Coordinate.closestTo0_0(intersections)
  }
}

object Main extends App {

  def parseData(): (Wire, Wire) = {
    val wires = Source
      .fromResource("moves.txt")
      .getLines()
      .map { row =>
        val moveStrs = row.split(",")
        moveStrs.map(Move.from)
      }
      .map(moves => Wire(moves.toList))
      .toList

    (wires.head, wires(1))
  }

  private val (wire1, wire2) = parseData()
  println(Grid(wire1, wire2).distanceToClosest())
}
