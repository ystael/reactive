package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  def randomPercentIsBelow(p: Int): Boolean = randomBelow(100) < p

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    val prevalencePercent: Int = 1
    val transmissionPercent: Int = 40
    val fatalityPercent: Int = 25
    val maxMoveDelay: Int = 5

    def sickDelay: Int = 6
    def dieDelay: Int = 14
    def immuneDelay: Int = 16
    def recoverDelay: Int = 18
  }

  import SimConfig._

  val persons: List[Person] = (for (i <- 0 until population) yield new Person(i)).toList

  type Cell = Pair[Int, Int]

  def wrapRow(row: Int): Int = {
    val rawOffset = row % roomRows
    if (rawOffset >= 0) rawOffset else rawOffset + roomRows
  }

  def wrapCol(col: Int): Int = {
    val rawOffset = col % roomColumns
    if (rawOffset >= 0) rawOffset else rawOffset + roomColumns
  }

  def neighbors(c: Cell): List[Cell] = c match {
    case (row, col) => List((wrapRow(row - 1), col),
                            (wrapRow(row + 1), col),
                            (row, wrapCol(col - 1)),
                            (row, wrapCol(col + 1)))
  }

  type RoomMap = Vector[Vector[List[Person]]]

  val emptyMap : RoomMap = Vector.fill(roomRows, roomColumns)(Nil)

  def roomMap : RoomMap =
    persons.foldLeft(emptyMap)((m, p) => {
      val oldPop = m(p.row)(p.col)
      m.updated(p.row, m(p.row).updated(p.col, p::oldPop))
    })

  def eligibleNeighbors(c: Cell, m: RoomMap): List[Cell] =
    neighbors(c).filter(_ match {
      case (row, col) => m(row)(col).forall((p) => !p.sick)
    })

  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)
    def cell: Cell = (row, col)

    def isInfectable : Boolean = !infected && !immune && !dead

    def becomeInfected { infected = true }
    def getSick { sick = true }
    def maybeDie { if (randomPercentIsBelow(fatalityPercent)) dead = true }
    def becomeImmune { if (!dead) { immune = true; sick = false } }
    def recover { if (!dead) { infected = false; immune = false } }

    if (infected) contractAction(this)
    afterDelay(1 + randomBelow(maxMoveDelay)) { moveAction(this) }
  }

  def moveTarget(p: Person, m: RoomMap): Option[Cell] = {
    eligibleNeighbors(p.cell, m) match {
      case List() => None
      case ns => Some(ns(randomBelow(ns.length)))
    }
  }

  def hasInfectious(m: RoomMap, row: Int, col: Int) = m(row)(col).exists(_.infected)

  def moveAction(p: Person) {
    if (!p.dead) {
      val m = roomMap
      moveTarget(p, m) match {
        case None => () // move canceled, no destination
        case Some((targetRow, targetCol)) => {
          // move, then see whether we get sick
          val danger = hasInfectious(m, targetRow, targetCol)
          p.row = targetRow
          p.col = targetCol
          if (p.isInfectable & danger & randomPercentIsBelow(transmissionPercent))
            contractAction(p)
        }
      }
      // finally enqueue the next move
      afterDelay(1 + randomBelow(maxMoveDelay)) { moveAction(p) }
    }
  }

  def contractAction(p: Person) {
    p.becomeInfected
    afterDelay(sickDelay) { p.getSick }
    afterDelay(dieDelay) { p.maybeDie }
    afterDelay(immuneDelay) { p.becomeImmune }
    afterDelay(recoverDelay) { p.recover }
  }

  def infectByPrevalence {
    val toInfect = persons.length * prevalencePercent / 100
    var infectedSoFar = 0
    while (infectedSoFar < toInfect) {
      val victim = persons(randomBelow(persons.length))
      if (!victim.infected) {
        contractAction(victim)
        infectedSoFar += 1
      }
    }
  }

  infectByPrevalence
}
