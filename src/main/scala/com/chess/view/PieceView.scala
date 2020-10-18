package com.chess.view

import com.chess.pieces._

object PieceView {
  def apply(piece: Piece): Char = {

    val name = piece match {
      case Pawn(_, _) => 'P'
      case King(_, _) => 'K'
      case Queen(_, _) => 'Q'
      case Rook(_, _) => 'R'
      case Knight(_, _) => 'N'
      case Bishop(_, _) => 'B'
    }
    if (piece.isWhite)
      name.toUpper
    else
      name.toLower
  }
}
