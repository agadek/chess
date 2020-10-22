package com.chess.pieces

import com.chess.pieces.paths.{DiagonalPath, ForwardPath, HorizontalPath, KnightPath}

sealed trait Piece {
  implicit val self: Piece = this

  def isWhite: Boolean

  def wasMoved: Boolean

  def availableMoves(location: Int)(implicit board: Vector[Option[Piece]]): Set[Int] =
    canMoveTo(location) ++ canAttack(location)

  protected def canMoveTo(location: Int)(implicit board: Vector[Option[Piece]]): Set[Int]

  def canAttack(location: Int)(implicit board: Vector[Option[Piece]]): Set[Int]

  def setMoved = this
}

case class Pawn(isWhite: Boolean, wasMoved: Boolean = false) extends Piece {
  private val direction = if (isWhite) -1 else 1

  override def setMoved: Piece = copy(wasMoved = true)

  override def canMoveTo(location: Int)(implicit board: Vector[Option[Piece]]): Set[Int] =
    if (wasMoved) ForwardPath(direction, location).toSet
    else ForwardPath(2 * direction, location).toSet

  override def canAttack(location: Int)(implicit board: Vector[Option[Piece]]): Set[Int] =
    DiagonalPath(direction, location, attack = true).toSet

}

case class King(isWhite: Boolean, wasMoved: Boolean = false) extends Piece {
  override def setMoved: Piece = copy(wasMoved = true)

  override def canMoveTo(location: Int)(implicit board: Vector[Option[Piece]]): Set[Int] =
    DiagonalPath(1, location).toSet ++
      DiagonalPath(-1, location) ++
      ForwardPath(1, location).toSet ++
      ForwardPath(-1, location).toSet ++
      HorizontalPath(1, location) ++
      HorizontalPath(-1, location)

  override def canAttack(location: Int)(implicit board: Vector[Option[Piece]]): Set[Int] =
    DiagonalPath(1, location, attack = true).toSet ++
      DiagonalPath(-1, location, attack = true) ++
      ForwardPath(1, location, attack = true).toSet ++
      ForwardPath(-1, location, attack = true).toSet ++
      HorizontalPath(1, location, attack = true) ++
      HorizontalPath(-1, location, attack = true)
}

case class Queen(isWhite: Boolean, wasMoved: Boolean = false) extends Piece {
  override def canMoveTo(location: Int)(implicit board: Vector[Option[Piece]]): Set[Int] =
    DiagonalPath(8, location).toSet ++
      DiagonalPath(-8, location) ++
      ForwardPath(8, location) ++
      ForwardPath(-8, location) ++
      HorizontalPath(8, location) ++
      HorizontalPath(-8, location)

  override def canAttack(location: Int)(implicit board: Vector[Option[Piece]]): Set[Int] =
    DiagonalPath(8, location, attack = true).toSet ++
      DiagonalPath(-8, location, attack = true) ++
      ForwardPath(8, location, attack = true) ++
      ForwardPath(-8, location, attack = true) ++
      HorizontalPath(8, location, attack = true) ++
      HorizontalPath(-8, location, attack = true)
}

case class Rook(isWhite: Boolean, wasMoved: Boolean = false) extends Piece {
  override def canMoveTo(location: Int)(implicit board: Vector[Option[Piece]]): Set[Int] =
    HorizontalPath(8, location).toSet ++
      HorizontalPath(-8, location) ++
      ForwardPath(8, location) ++
      ForwardPath(-8, location)


  override def canAttack(location: Int)(implicit board: Vector[Option[Piece]]): Set[Int] =
    HorizontalPath(8, location, attack = true).toSet ++
      HorizontalPath(-8, location, attack = true) ++
      ForwardPath(8, location, attack = true) ++
      ForwardPath(-8, location, attack = true)

}

case class Knight(isWhite: Boolean, wasMoved: Boolean = false) extends Piece {
  override def canMoveTo(location: Int)(implicit board: Vector[Option[Piece]]): Set[Int] =
    KnightPath(1, location).toSet ++
      KnightPath(-1, location)

  override def canAttack(location: Int)(implicit board: Vector[Option[Piece]]): Set[Int] =
    KnightPath(1, location, attack = true).toSet ++
      KnightPath(-1, location, attack = true)
}

case class Bishop(isWhite: Boolean, wasMoved: Boolean = false) extends Piece {
  override def canMoveTo(location: Int)(implicit board: Vector[Option[Piece]]): Set[Int] =
    DiagonalPath(8, location).toSet ++
      DiagonalPath(-8, location)

  override def canAttack(location: Int)(implicit board: Vector[Option[Piece]]): Set[Int] =
    DiagonalPath(8, location, attack = true).toSet ++
      DiagonalPath(-8, location, attack = true)
}
