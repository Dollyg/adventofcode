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
  def closestTo0(coords: List[Coordinate]): Int = {
    coords.foldLeft(Int.MaxValue) { (dist, current) =>
      val newDist = current.manhattanDistance(Coordinate(0, 0))
      println("Coord=" + current + " dist=" + newDist)
      if (newDist < dist)
        newDist
      else dist
    }
  }
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

case class Grid(wires: List[Wire]) {
  val wireLines: List[WireLine] = wires.map(_.line)

  def mm(): Int = {
    val x = for {
      line1 <- wireLines
      line2 <- wireLines
      if line1 != line2
    } yield {
      println(line1)
      println(line2)

      val i = line1.intersection(line2)
      println("Intersection=" + i)
      i
    }

    println("x=" + x)
    Coordinate.closestTo0(x.flatten.distinct)
  }
}

object Main extends App {

  val data: List[Wire] = Source
    .fromResource("moves.txt")
    .getLines()
    .map { row =>
      val moveStrs = row.split(",")
      moveStrs.map(str => {
        val (dir, step) = str.splitAt(1)
        Move(Direction.from(dir), step.toInt)
      })
    }
    .map(moves => Wire(moves.toList))
    .toList

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
  println(Grid(data))
  println(Grid(data).mm())
}
