package com.chess

case class Game(currentPlayer: Player = White, board: Board = Board.build()) {

  def move(from: Address, to: Address): Either[GameError, Game] = ???

}


sealed trait GameError


sealed trait Player {
  def isWhite: Boolean
}

case object White extends Player {
  override def isWhite: Boolean = true
}

case object Black extends Player {
  override def isWhite: Boolean = false
}
