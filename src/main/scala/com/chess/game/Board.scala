package com.chess.game

import cats.instances.either._
import cats.instances.vector._
import cats.syntax.traverse._
import com.chess.pieces._


case class Board(
                  board: Vector[Option[Piece]] = Vector.fill(64)(Option.empty[Piece]),
                  kiaWhites: Seq[Piece] = Seq.empty,
                  kiaBlack: Seq[Piece] = Seq.empty) {

  def set(address: Address, piece: Piece): Either[AlreadyTakenAddress, Board] =
    if (board(address.filedIndex).isEmpty) Right(this.copy(board.updated(address.filedIndex, Some(piece))))
    else Left(AlreadyTakenAddress(address))

  def clear(address: Address): Either[EmptyAddress, Board] =
    board(address.filedIndex).toRight(EmptyAddress(address))
      .map(_ => this.copy(board = board.updated(address.filedIndex, None)))

  def kill(address: Address): Board = board(address.filedIndex) match {
      case Some(piece) if piece.isWhite => this.copy(board = board.updated(address.filedIndex, None), kiaWhites = kiaWhites :+ piece)
      case Some(piece)                  => this.copy(board = board.updated(address.filedIndex, None), kiaBlack = kiaBlack :+ piece)
      case _                            => this
    }

  def checkKingStatus(implicit player: Player): Either[BoardError, Unit] = {
    case class PossibleAttacks(piece: Piece, from:Int, to:Set[Int]){
      def contains(kingsLocation:Int): Boolean = to.contains(kingsLocation)
    }
    val boardWithIndex = board.zipWithIndex.map{ case (piece, index) => piece.map(_ -> index)}

    val kingLocation = boardWithIndex.collectFirst {
      case Some((king:King, location)) if king.isWhite == player.isWhite => king -> location
    }.toRight(KingNotFound(player))

    val enemiesCanAttack = boardWithIndex.collect{case Some((piece, index)) if piece.isWhite != player.isWhite => piece -> index}
      .map{ case (piece, location) => PossibleAttacks(piece, location, piece.canAttack(location)(board))}

    for {
      kingLocation     <- kingLocation
      (king, location) = kingLocation
      attackedForm     <- enemiesCanAttack
                          .filter(_.contains(location))
                          .traverse(elem => Address(elem.from).map(elem.piece -> _))
      result           <- Either.cond(attackedForm.isEmpty, (), PossibleCheck(player, attackedForm.map(elem => elem._1 -> elem._2)))

    } yield result
  }

  private def getPieceFrom(address: Address)(implicit player: Player): Either[BoardError, Piece] =
    board(address.filedIndex).toRight(EmptyAddress(address))
      .flatMap {
        case piece if piece.isWhite == player.isWhite => Right(piece)
        case piece                                    => Left(NotYoursPiece(address, player, piece))
      }



  def move(from: Address, to: Address)(implicit player: Player): Either[BoardError, Board] =
      for {
        piece   <- getPieceFrom(from)
        _       <- Either.cond(from != to, (), InvalidMove(from, to))
        _       <- Either.cond(piece.availableMoves(from.filedIndex)(board = this.board).contains(to.filedIndex), (), InvalidMove(from, to))
        board   <- clear(from)
        board2  = board.kill(to)
        board3  <- board2.set(to, piece.setMoved)
        _       <- board3.checkKingStatus
      } yield board3

}

object Board {

  private def pawns(white: Boolean): Vector[Option[Piece]] = Vector.fill(8)(Pawn(white)).map(Option(_))

  private def figures(white: Boolean): Vector[Option[Piece]] = Vector(Rook(white), Bishop(white), Knight(white), Queen(white), King(white), Knight(white), Bishop(white), Rook(white)).map(Option(_))

  def build() =
    Board(board =
      figures(false) ++
        pawns(false) ++
        Vector.fill(32)(Option.empty[Piece]) ++
        pawns(true) ++
        figures(true)
    )
}


sealed trait BoardError {
  def msg: String
}

case class InvalidAddress(address: String) extends BoardError {
  override def msg: String = s"invalid address: $address"
}

case class EmptyAddress(address: Address) extends BoardError {
  override def msg: String = s"address is empty: $address"
}

case class NotYoursPiece(address: Address, player: Player, piece: Piece) extends BoardError {
  override def msg: String = s"can't move that piece, address:$address taken by $player ${piece.getClass.getSimpleName}"
}

case class InvalidMove(from: Address, to: Address) extends BoardError {
  override def msg: String = s"invalid move from:${from.toString} to:${to.toString}"
}

case class AlreadyTakenAddress(address: Address) extends BoardError {
  override def msg: String = s"${address.toString} already taken, incorrect move"
}

case class KingNotFound(player: Player) extends BoardError {
  override def msg: String = s"$player King was not found"
}

case class PossibleCheck(player: Player, attackers: Seq[(Piece, Address)]) extends BoardError {
  override def msg: String = s"$player King can checked by: $attackers"
}
