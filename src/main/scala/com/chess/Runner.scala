package com.chess

import cats.syntax.either._

object Runner extends App {

  var game = GameController(inputF = scala.io.StdIn.readLine)

  println(s"""Enter your move in format like "A1 H8"" or hit F to use file input""")

  while (!game.isDone) {
    game.next(None).bimap(
      error   => println(error.toString),
      newGame => game = newGame
    )
  }

  println(game.printBoardState())
  println("Game Over")
}
