package com.chess

import cats.syntax.either._
import com.chess.game._
import com.chess.view.BoardView
import com.whitehatgaming.UserInputFile


case class GameController(game: Game = Game(), fileMode: Boolean = false) {

  def isDone: Boolean = !game.currentPlayerHasLegalMoves

  def next(input: String): Either[GameError, GameController] = {
    if (!fileMode && input.toLowerCase == "f")
      Right(this.copy(fileMode = true))
    else if (fileMode)
    ???
    else {
      for {
        move       <- Address.parseMoveInput(input).leftMap(BoardErrorWrapper(_))
        (from, to) = move
        newGame    <- game.move(from, to)
      } yield GameController(newGame)
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
      s"You are in check! From:${check.attackers.map { case (piece, address) => piece.getClass.getSimpleName -> address.toString }.mkString(",")}"
    }.getOrElse("")
}

