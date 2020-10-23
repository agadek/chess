package com.chess

import cats.syntax.either._
import com.chess.game._
import com.chess.view.BoardView

case class GameController(game: Game = Game(), fileMode: Boolean = false, inputF: () => String) {

  def isDone: Boolean = !game.currentPlayerHasLegalMoves

  def next(in:Option[String] = None): Either[GameError, GameController] = {
    println(printBoardState())
    if (!fileMode)
      println(s"${game.currentPlayer}: enter next command:")
    else
      println(s"${game.currentPlayer}: enter file path or F to revert:")
    val userInput = in.getOrElse(inputF())
    next(userInput)
  }


  private def next(input: String): Either[GameError, GameController] = {
    if (!fileMode && input.toLowerCase == "f")
      Right(this.copy(fileMode = !fileMode))
    else if (fileMode)
      for {
        moves   <- FileManager(input)
        newGame <- moves.foldLeft(game.asRight[GameError]){(game, move) => game.flatMap{game =>
                                                                              println(printBoardState(game))
                                                                              game.move(move._1, move._2)}
                                                                            }
      } yield GameController(newGame, inputF = inputF)

    else {
      for {
        move       <- Address.parseMoveInput(input).leftMap(BoardErrorWrapper(_))
        (from, to) = move
        newGame    <- game.move(from, to)
      } yield GameController(newGame, inputF = inputF)
    }
  }

  def printBoardState(game: Game = game): String =
    s"""
       |${BoardView.kiaView(game.board, Black)}\n
       |${BoardView(game.board)}\n
       |${BoardView.kiaView(game.board, White)}\n
       |--------------------------
       |${checkMsg(game)}""".stripMargin

  private def checkMsg(game: Game) =
    game.currentPlayerInCheck.collect { case check: PossibleCheck =>
      s"You are in check! From:${check.attackers.map {
        case (piece, address) => piece.getClass.getSimpleName -> address.toString
      }.mkString(",")}"
    }.getOrElse("")
}

