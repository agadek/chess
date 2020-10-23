package com.chess.game

import cats.syntax.either._

case class Game(currentPlayer: Player = White, board: Board = Board.build()) {
  def currentPlayerInCheck: Option[BoardError] = board.checkKingStatus(currentPlayer).swap.toOption

  def move(from: Address, to: Address): Either[GameError, Game] =
    board.move(from, to)(player = currentPlayer)
      .leftMap(BoardErrorWrapper)
      .map(newBoard => this.copy(currentPlayer = currentPlayer.switch, newBoard))

  def currentPlayerHasLegalMoves: Boolean = {
    val playersPieces = board.board.zipWithIndex.collect {
      case (Some(piece), index) if piece.isWhite == currentPlayer.isWhite => piece -> index
    }
    playersPieces.exists { case (piece, index) =>
      val availableMoves = piece.availableMoves(index)(board.board)
      availableMoves.exists(move =>
        (for {
          from     <- Address(index)
          to       <- Address(move)
          newBoard <- board.move(from, to)(currentPlayer)
        } yield newBoard).isRight
      )
    }
  }
}


sealed trait GameError

case class BoardErrorWrapper(boardError: BoardError) extends GameError {
  override def toString = boardError.msg
}

case class InputFileError(file: String, e: Throwable) extends GameError {
  override def toString = s"inputFile error: ${e.toString}"
}


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
