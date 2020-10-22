package com.chess.game

import cats.syntax.either._

case class Game(currentPlayer: Player = White, board: Board = Board.build()) {
  def currentPlayerInCheck: Option[BoardError] = board.checkKingStatus(currentPlayer).swap.toOption

  def move(from: Address, to: Address): Either[BoardErrorWrapper, Game] =
    board.move(from, to)(player = currentPlayer)
      .leftMap(BoardErrorWrapper)
      .map(newBoard => this.copy(currentPlayer = currentPlayer.switch, newBoard))

  def currentPlayerHasLegalMoves: Boolean = true
}


sealed trait GameError

case class BoardErrorWrapper(boardError: BoardError) extends GameError


sealed trait Player {
  def switch: Player

  def isWhite: Boolean
}

case object White extends Player {
  def switch: Player = Black

  override def isWhite: Boolean = true
}

case object Black extends Player {
  def switch: Player = White

  override def isWhite: Boolean = false
}
