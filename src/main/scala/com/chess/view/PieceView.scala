package com.chess.view

import com.chess.pieces._

object PieceView {
  def apply(piece: Piece): Char = {

    val name = piece match {
      case Pawn(_) => 'P'
      case King(_) => 'K'
      case Queen(_) => 'Q'
      case Rook(_) => 'R'
      case Knight(_) => 'N'
      case Bishop(_) => 'B'
    }
    if (piece.isWhite)
      name.toUpper
    else
      name.toLower
  }
}
