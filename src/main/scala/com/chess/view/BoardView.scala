package com.chess.view

import com.chess.Board
import com.chess.pieces.Piece

import scala.Console.{BOLD, RESET, REVERSED}

object BoardView {
  def apply(board: Board): String = {
    val view = new BoardView
    board.board.grouped(8).zipWithIndex.foreach { case (pawns, row) => view.setRow(pawns, row) }
    view.print
  }
}

class BoardView {
  private val ABC = "-ABCDEFGH-" zip (0 to 9)
  private val buffer = {
    val array = Array.fill(10, 10)(" ")
    ABC.foreach { case (abc, int) =>

      if (0 < int && int < 9) {
        array(int)(0) = borderBox((9 - int).toString)
        array(int)(9) = borderBox((9 - int).toString)
      }
      array(0)(int) = borderBox(abc.toString)
      array(9)(int) = borderBox(abc.toString)
    }
    array
  }

  def setRow(pawns: Vector[Option[Piece]], row: Int): Unit = {
    for (i <- 0 to 7) {
      val value = pawns(i).map(PieceView(_)).getOrElse(' ').toString
      if ((row + i) % 2 == 0)
        buffer(row + 1)(i + 1) = blackBox(value)
      else
        buffer(row + 1)(i + 1) = value
    }
  }

  def print: String = buffer.map(_.mkString("")).mkString("\n")

  private def borderBox(value: String) = s"$value"

  private def blackBox(value: String) = s"${REVERSED}${BOLD}$value${RESET}"
}
