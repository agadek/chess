package com.chess.pieces.paths

import com.chess.pieces.Piece

import scala.annotation.tailrec

object ForwardPath {
  @tailrec
  def apply(length: Int)(board: Vector[Option[Piece]], location: Int, acc: Seq[Int] = Seq.empty): Seq[Int] = {
    val endLocation = location + (8 * length.sign)
    if (length != 0 && 0 <= endLocation && endLocation < board.size && board(endLocation).isEmpty) {
      val result = Seq(endLocation)
      apply(length - (1 * length.sign))(board, endLocation, acc ++ result)
    } else acc
  }
}
