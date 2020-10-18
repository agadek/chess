package com.chess

import com.chess.pieces.Board

object aaa extends App {

  import com.chess.view.BoardView

  import scala.Console.{BOLD, RESET, REVERSED}

  println(BoardView(Board.build()))
  println("")
  println("")
  println("")
  println("")
  println(BoardView(Board.build()))

}
