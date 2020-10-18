package com.chess.pieces


case class Board(board: Vector[Option[Piece]] = Vector.fill(64)(Option.empty[Piece])){


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
