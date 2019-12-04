package day1

import scala.io.Source

object FuelCalculator extends App {

  def readData(filename: String): List[Long] =
    Source
      .fromResource(filename)
      .getLines()
      .map(_.toLong)
      .toList

  private def calculateFuel(mass: Long): Long = {
    val fuel = mass / 3 - 2
    if (fuel < 0) 0
    else fuel + calculateFuel(fuel)
  }

  def calculate(data: List[Long]): Long = {
    data.map(calculateFuel).sum
  }

  println(calculate(List(14)))
  println(calculate(List(100756)))
  println(calculate(readData("masses.txt")))
}
