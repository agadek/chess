package com.chess.pieces

sealed trait Piece {
  val isWhite: Boolean
}

case class Pawn(isWhite: Boolean) extends Piece {
}

case class King(isWhite: Boolean) extends Piece {
}

case class Queen(isWhite: Boolean) extends Piece {
}

case class Rook(isWhite: Boolean) extends Piece {
}

case class Knight(isWhite: Boolean) extends Piece {
}

case class Bishop(isWhite: Boolean) extends Piece {
}

