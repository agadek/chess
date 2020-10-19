package com.chess.pieces.paths

import com.chess.pieces.Piece

import scala.annotation.tailrec

sealed trait Path {
  protected val leftBorder = (0 to 7).map(_ * 8)
  protected val rightBorder = (0 to 7).map(_ * 8 + 7)
  private val borders = leftBorder zip rightBorder

  def apply(length: Int)(board: Vector[Option[Piece]], location: Int): Seq[Int]

  protected def findRowBorder(location: Int): (Option[Int], Option[Int]) = borders.find {
    case (left, right) => left <= location && location <= right
  }.unzip
}

object ForwardPath extends Path {
  def apply(length: Int)(board: Vector[Option[Piece]], location: Int): Seq[Int] = {
    @tailrec
    def checkPath(length: Int)(board: Vector[Option[Piece]], location: Int, acc: Seq[Int] = Seq.empty): Seq[Int] = {
      val endLocation = location + (8 * length.sign)
      if (length != 0 && 0 <= endLocation && endLocation < board.size && board(endLocation).isEmpty) {
        checkPath(length - (1 * length.sign))(board, endLocation, acc :+ endLocation)
      } else acc
    }

    checkPath(length)(board, location)
  }
}

object HorizontalPath extends Path {

  def apply(length: Int)(board: Vector[Option[Piece]], location: Int): Seq[Int] = {
    val (leftEdge, rightEdge): (Option[Int], Option[Int]) = findRowBorder(location)

    checkPath(length, toLeft = true, board, leftEdge, rightEdge, location, Seq.empty) ++
      checkPath(length, toLeft = false, board, leftEdge, rightEdge, location, Seq.empty)
  }

  @tailrec
  private def checkPath(length: Int, toLeft: Boolean, board: Vector[Option[Piece]], leftEdge: Option[Int], rightEdge: Option[Int], location: Int, acc: Seq[Int] = Seq.empty): Seq[Int] = {
    val sideFactor = if (toLeft) -1 else 1
    val endLocation = location + sideFactor
    if (length > 0 && leftEdge.exists(_ <= endLocation) && rightEdge.exists(_ >= endLocation) && board(endLocation).isEmpty) {
      checkPath((length - (1 * length.sign)), toLeft, board, leftEdge: Option[Int], rightEdge: Option[Int], endLocation, acc :+ endLocation)
    } else acc
  }
}

object DiagonalPath extends Path {
  def apply(length: Int)(board: Vector[Option[Piece]], location: Int): Seq[Int] = {
    checkPath(length, true, board, leftBorder, location, Seq.empty) ++
      checkPath(length, false, board, rightBorder, location, Seq.empty)
  }

  @tailrec
  private def checkPath(length: Int, toLeft: Boolean, board: Vector[Option[Piece]], borderIndex: Seq[Int], location: Int, acc: Seq[Int] = Seq.empty): Seq[Int] = {
    val sideFactor = if (toLeft) 1 else -1
    val step = 8 + ((-length.sign) * sideFactor)
    val endLocation = location + (step * length.sign)
    if (length != 0 && !borderIndex.contains(location) && 0 <= endLocation && endLocation < board.size && board(endLocation).isEmpty) {
      checkPath((length - (1 * length.sign)), toLeft, board, borderIndex, endLocation, acc :+ endLocation)

    } else acc
  }
}

object KnightPath extends Path {

  def apply(length: Int)(board: Vector[Option[Piece]], location: Int): Seq[Int] = {
    checkPath(toLeft = true, direction = 1, board = board, location = location) ++
      checkPath(toLeft = false, direction = 1, board = board, location = location) ++
      checkPath(toLeft = true, direction = -1, board = board, location = location) ++
      checkPath(toLeft = false, direction = -1, board = board, location = location)
  }

  private def checkPath(toLeft: Boolean, direction: Int, board: Vector[Option[Piece]], location: Int): Seq[Int] = {
    def validate(endLocation: Int, leftEdge: Option[Int], rightEdge: Option[Int]) = {
      if (leftEdge.exists(_ <= endLocation) && rightEdge.exists(_ >= endLocation)) {
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