package com.chess.pieces

sealed trait Piece {
  def isWhite: Boolean

  def wasMoved: Boolean

  def possibleMoves(board: Vector[Option[Piece]], location: Int): Set[Int] =
    canMoveTo(board, location) ++ canAttack(board, location)

  protected def canMoveTo(board: Vector[Option[Piece]], location: Int): Set[Int]

  protected def canAttack(board: Vector[Option[Piece]], location: Int): Set[Int]

  protected def fieldAvailable(board: Vector[Option[Piece]], location: Int): Boolean = ???

  protected def pathAvailable(board: Vector[Option[Piece]], location: Int*): Seq[Int] = ???
}

case class Pawn(isWhite: Boolean, wasMoved: Boolean = false) extends Piece {
  override def canMoveTo(board: Vector[Option[Piece]], location: Int): Set[Int] = ???

  override def canAttack(board: Vector[Option[Piece]], location: Int): Set[Int] = ???
}

case class King(isWhite: Boolean, wasMoved: Boolean = false) extends Piece {
  override def canMoveTo(board: Vector[Option[Piece]], location: Int): Set[Int] = ???

  override def canAttack(board: Vector[Option[Piece]], location: Int): Set[Int] = ???
}

case class Queen(isWhite: Boolean, wasMoved: Boolean = false) extends Piece {
  override def canMoveTo(board: Vector[Option[Piece]], location: Int): Set[Int] = ???

  override def canAttack(board: Vector[Option[Piece]], location: Int): Set[Int] = ???
}

case class Rook(isWhite: Boolean, wasMoved: Boolean = false) extends Piece {
  override def canMoveTo(board: Vector[Option[Piece]], location: Int): Set[Int] = ???

  override def canAttack(board: Vector[Option[Piece]], location: Int): Set[Int] = ???
}

case class Knight(isWhite: Boolean, wasMoved: Boolean = false) extends Piece {
  override def canMoveTo(board: Vector[Option[Piece]], location: Int): Set[Int] = ???

  override def canAttack(board: Vector[Option[Piece]], location: Int): Set[Int] = ???
}

case class Bishop(isWhite: Boolean, wasMoved: Boolean = false) extends Piece {
  override def canMoveTo(board: Vector[Option[Piece]], location: Int): Set[Int] = ???

  override def canAttack(board: Vector[Option[Piece]], location: Int): Set[Int] = ???
}

