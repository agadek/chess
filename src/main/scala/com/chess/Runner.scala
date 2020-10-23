package com.chess

import cats.syntax.either._
import com.chess.game._
import com.chess.view.BoardView

object Runner extends App {

  var game = GameController()

  println(s"""Enter your move in format like "A1 H8"" or hit F to use file input""")

  while (!game.isDone) {
    println(game.printBoardState())
    println(s"${game.game.currentPlayer}: enter your move:")
    game.next(scala.io.StdIn.readLine()).bimap(
      error   => println(error.toString),
      newGame => game = newGame
    )
  }

  println("Game Over")
}
