package com.chess.pieces.paths

import com.chess.pieces.Piece

import scala.annotation.tailrec

sealed trait Path {
  protected val leftBorder = (0 to 7).map(_ * 8)
  protected val rightBorder = (0 to 7).map(_ * 8 + 7)
  private val borders = leftBorder zip rightBorder

  def apply(length: Int, location: Int, attack: Boolean = false)(implicit board: Vector[Option[Piece]], piece: Piece): Seq[Int]

  protected def findRowBorder(location: Int): (Option[Int], Option[Int]) = borders.find {
    case (left, right) => left <= location && location <= right
  }.unzip

  protected def locationWithinBoardSize(location: Int)(implicit board: Vector[Option[Piece]]): Boolean =
    0 <= location && location < board.size

  protected def locationEmpty(location: Int)(implicit board: Vector[Option[Piece]]): Boolean =
    board(location).isEmpty

  protected def locationTakenByEnemy(location: Int, piece:Piece)(implicit board: Vector[Option[Piece]]): Boolean =
    board(location).map{_.isWhite}.contains(!piece.isWhite)


  protected def locationInBoundaries(location: Int, leftEdge: Option[Int], rightEdge: Option[Int]): Boolean =
    leftEdge.exists(_ <= location) && rightEdge.exists(_ >= location)
}

object ForwardPath extends Path {
  def apply(length: Int, location: Int, attack: Boolean = false)(implicit board: Vector[Option[Piece]], piece: Piece): Seq[Int] = {
    @tailrec
    def checkPath(length: Int, location: Int, acc: Seq[Int] = Seq.empty)(implicit board: Vector[Option[Piece]], piece: Piece): Seq[Int] = {
      val endLocation = location + (8 * length.sign)
      if (length != 0 && locationWithinBoardSize(endLocation) && locationEmpty(endLocation)) {
        checkPath(length - (1 * length.sign), endLocation, acc :+ endLocation)
      }
      else if (attack && length != 0 && locationWithinBoardSize(endLocation) && locationTakenByEnemy(endLocation, piece)) Seq(endLocation)
      else if (attack) Seq()
      else acc
    }

    checkPath(length, location)
  }
}

object HorizontalPath extends Path {

  def apply(length: Int, location: Int, attack: Boolean = false)(implicit board: Vector[Option[Piece]], piece: Piece): Seq[Int] = {
      val (leftEdge, rightEdge): (Option[Int], Option[Int]) = findRowBorder(location)

    checkPath(length, toLeft = true, leftEdge, rightEdge, location, attack, Seq.empty) ++
      checkPath(length, toLeft = false, leftEdge, rightEdge, location, attack, Seq.empty)
  }

  @tailrec
  private def checkPath(length: Int, toLeft: Boolean, leftEdge: Option[Int], rightEdge: Option[Int], location: Int, attack: Boolean, acc: Seq[Int] = Seq.empty)(implicit board: Vector[Option[Piece]], piece: Piece): Seq[Int] = {
    val sideFactor = if (toLeft) -1 else 1
    val endLocation = location + sideFactor
    if (length > 0 && locationInBoundaries(endLocation, leftEdge, rightEdge) && locationEmpty(endLocation))
      checkPath(length - (1 * length.sign), toLeft, leftEdge: Option[Int], rightEdge: Option[Int], endLocation, attack, acc :+ endLocation)
    else if (attack && length > 0 && locationInBoundaries(endLocation, leftEdge, rightEdge) && locationTakenByEnemy(endLocation, piece)) Seq(endLocation)
    else if (attack) Seq()
    else acc
  }
}

object DiagonalPath extends Path {
  def apply(length: Int, location: Int, attack: Boolean = false)(implicit board: Vector[Option[Piece]], piece: Piece): Seq[Int] =
    checkPath(length, true, leftBorder, location, attack, Seq.empty) ++
      checkPath(length, false, rightBorder, location, attack, Seq.empty)

  @tailrec
  private def checkPath(length: Int, toLeft: Boolean, borderIndex: Seq[Int], location: Int, attack: Boolean, acc: Seq[Int] = Seq.empty)(implicit board: Vector[Option[Piece]], piece: Piece): Seq[Int] = {
    val sideFactor = if (toLeft) 1 else -1
    val step = 8 + ((-length.sign) * sideFactor)
    val endLocation = location + (step * length.sign)
    if (length != 0 && !borderIndex.contains(location) && locationWithinBoardSize(endLocation) && locationEmpty(endLocation))
      checkPath(length - (1 * length.sign), toLeft, borderIndex, endLocation, attack, acc :+ endLocation)
    else if (attack && length != 0 && !borderIndex.contains(location) && 0 <= endLocation && endLocation < board.size && locationTakenByEnemy(endLocation, piece)) Seq(endLocation)
    else if (attack) Seq.empty
    else acc
  }
}

object KnightPath extends Path {
  def apply(length: Int, location: Int, attack: Boolean = false)(implicit board: Vector[Option[Piece]], piece: Piece): Seq[Int] = {
    checkPath(toLeft = true, direction = 1, location = location, attack) ++
      checkPath(toLeft = false, direction = 1, location = location, attack) ++
      checkPath(toLeft = true, direction = -1, location = location, attack) ++
      checkPath(toLeft = false, direction = -1, location = location, attack)
  }

  private def checkPath(toLeft: Boolean, direction: Int, location: Int, attack: Boolean)(implicit board: Vector[Option[Piece]], piece: Piece): Seq[Int] = {
    def validate(endLocation: Int, leftEdge: Option[Int], rightEdge: Option[Int]): Seq[Int] = {
      if (locationInBoundaries(endLocation, leftEdge, rightEdge) &&
        ((attack && locationTakenByEnemy(endLocation, piece)) || !attack && locationEmpty(endLocation))) {
        Seq(endLocation)
      } else Seq.empty
    }

    val sideFactor = if (toLeft) 1 else -1
    val endPosition1 = location - (8 + (2 * sideFactor)) * direction
    val row1Location = location - 8 * direction
    val (leftEdgeRow1, rightEdgeRow1): (Option[Int], Option[Int]) = findRowBorder(row1Location)

    val endPosition2 = location - (16 + (1 * sideFactor)) * direction
    val row2Location = location - 16 * direction
    val (leftEdgeRow2, rightEdgeRow2): (Option[Int], Option[Int]) = findRowBorder(row2Location)

    validate(endPosition1, leftEdgeRow1, rightEdgeRow1) ++ validate(endPosition2, leftEdgeRow2, rightEdgeRow2)
  }
}