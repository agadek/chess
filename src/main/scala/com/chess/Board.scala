package com.chess

import com.chess.pieces._

case class Board(board: Vector[Option[Piece]] = Vector.fill(64)(Option.empty[Piece])) {

  def set(address: Address, piece: Piece): Either[AlreadyTakenAddress, Board] = {
    if (board(address.filedIndex).isEmpty) Right(this.copy(board.updated(address.filedIndex, Some(piece))))
    else Left(AlreadyTakenAddress(address.toString))
  }



}

object Board {

  private def pawns(white: Boolean): Vector[Option[Piece]] = Vector.fill(8)(Pawn(white)).map(Option(_))

  private def figures(white: Boolean): Vector[Option[Piece]] = Vector(Rook(white), Bishop(white), Knight(white), Queen(white), King(white), Knight(white), Bishop(white), Rook(white)).map(Option(_))

  def build() =
    Board(board =
      figures(false) ++
        pawns(false) ++
        Vector.fill(32)(Option.empty[Piece]) ++
        pawns(true) ++
        figures(true)
    )
}


sealed trait BoardError

case class InvalidAddress(address: String) extends BoardError

case class AlreadyTakenAddress(address: String) extends BoardError