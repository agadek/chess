package com.chess.pieces

import com.chess.view.BoardView
import com.chess.{Address, Board}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class PieceSpec extends AnyFlatSpec with EitherValues{

  val emptyBoard: Board = Board()
  behavior of "pawn"

  it should "give one and two steps of pawn on clear board" in {
    val pawn = Pawn(isWhite = true)
    val address = Address("D2").toOption.get
    implicit val board = emptyBoard.set(address, pawn).toOption.get.board

    pawn.availableMoves(address.filedIndex) shouldBe Set(43, 35)
  }

  it should "pawn attack when enemy at the gate " in {
    val pawn = Pawn(isWhite = true, wasMoved = true)
    val address = Address("D2").toOption.get
    val enemy = Pawn(isWhite = false, wasMoved = true)
    val enemyAddress = Address("E3").toOption.get
    implicit val board = emptyBoard.set(address, pawn).toOption.get
                                    .set(enemyAddress, enemy).toOption.get.board


    pawn.availableMoves(address.filedIndex) shouldBe Set(43, 44)
  }

  behavior of "rook"

  it should "give one and two steps of pawn on clear board" in {
    val rook = Rook(isWhite = true)
    val address = Address("B5").toOption.get
    implicit val board = emptyBoard.set(address, rook).toOption.get.board

    rook.availableMoves(address.filedIndex) shouldBe Set(24, 17, 9, 1, 33, 41, 49, 57, 26, 27, 28, 29, 30, 31)
  }

  it should "pawn attack when enemy at the gate " in {
    val rook = Rook(isWhite = false)
    val address = Address("B5").toOption.get
    val enemy1 = Pawn(isWhite = true, wasMoved = true)
    val enemyAddress1 = Address("F5").toOption.get

    val enemy2 = Pawn(isWhite = true, wasMoved = true)
    val enemyAddress2 = Address("B7").toOption.get


    implicit val board = emptyBoard.set(address, rook).toOption.get
                                      .set(enemyAddress1, enemy1).toOption.get
                                      .set(enemyAddress2, enemy2).toOption.get
                                      .board

//    println(BoardView(Board((board))))

    rook.availableMoves(address.filedIndex).toList.sortBy(identity) shouldBe Set(24, 17, 9, 33, 41, 49, 57, 26, 27, 28, 29).toList.sortBy(identity)
  }

  behavior of "Knight"

  it should "give all jumps steps of knight on clear board" in {
    val knight = Knight(isWhite = true)
    val address = Address("B3").toOption.get
    implicit val board = emptyBoard.set(address, knight).toOption.get.board

    knight.availableMoves(address.filedIndex) shouldBe Set(24, 26, 35, 51, 58, 56)
  }

  it should "knight can attack only enemy, can jum over pawns,  can't jump" in {
    val knight = Knight(isWhite = true)
    val address = Address("B3").toOption.get
    val friend = Pawn(isWhite = true, wasMoved = true)
    val friendAddress = Address("A5").toOption.get

    val enemy1 = Pawn(isWhite = true, wasMoved = true)
    val enemyAddress1 = Address("C4").toOption.get

    val enemy2 = Pawn(isWhite = false, wasMoved = true)
    val enemyAddress2 = Address("C5").toOption.get

    implicit val board = emptyBoard.set(address, knight).toOption.get
                                    .set(friendAddress, friend).toOption.get
                                    .set(enemyAddress1, enemy1).toOption.get
                                    .set(enemyAddress2, enemy2).toOption.get
                                    .board

//    println(BoardView(Board((board))))

    knight.availableMoves(address.filedIndex) shouldBe Set(26, 35, 51, 58, 56)
  }

  behavior of "Bishop"

  it should "can move unlimited on diagonals " in {
    val bishop = Bishop(isWhite = true)
    val address = Address("D5").toOption.get
    implicit val board = emptyBoard.set(address, bishop).toOption.get.board

    bishop.availableMoves(address.filedIndex).toList.sortBy(identity) shouldBe Set(0, 9, 18, 36, 45, 54, 63, 48, 41, 34, 20, 13, 6).toList.sortBy(identity)
  }

  it should "bishop can attack only enemy" in {
    val bishop = Bishop(isWhite = true)
    val address = Address("D5").toOption.get
    val friend = Pawn(isWhite = true, wasMoved = true)
    val friendAddress = Address("B7").toOption.get

    val enemy1 = Pawn(isWhite = false, wasMoved = true)
    val enemyAddress1 = Address("B3").toOption.get

    val enemy2 = Pawn(isWhite = false, wasMoved = true)
    val enemyAddress2 = Address("E4").toOption.get

    implicit val board = emptyBoard.set(address, bishop).toOption.get
                                      .set(friendAddress, friend).toOption.get
                                      .set(enemyAddress1, enemy1).toOption.get
                                      .set(enemyAddress2, enemy2).toOption.get
                                      .board

    //    println(BoardView(Board((board))))
    bishop.availableMoves(address.filedIndex).toList.sortBy(identity) shouldBe Set(18, 36, 41, 34, 20, 13, 6).toList.sortBy(identity)
  }

  behavior of "Queen"

  it should "can move unlimited any direction" in {
    val queen = Queen(isWhite = false)
    val address = Address("D6").toOption.get
    implicit val board = emptyBoard.set(address, queen).toOption.get.board

    queen.availableMoves(address.filedIndex).toList.sortBy(identity) shouldBe Set(3, 11, 27, 35, 43, 51, 59, 16, 17, 18, 20, 21, 22, 23, 1, 10, 28, 37, 46, 55, 40, 33, 26, 12, 5).toList.sortBy(identity)
  }

  it should "can attack only enemy" in {
    val queen = Queen(isWhite = false)
    val address = Address("D6").toOption.get
    val friend = Pawn(isWhite = false, wasMoved = true)
    val friendAddress = Address("C7").toOption.get

    val enemy1 = Pawn(isWhite = true, wasMoved = true)
    val enemyAddress1 = Address("B6").toOption.get

    val enemy2 = Pawn(isWhite = true, wasMoved = true)
    val enemyAddress2 = Address("D4").toOption.get

    implicit val board = emptyBoard.set(address, queen).toOption.get
                                      .set(friendAddress, friend).toOption.get
                                      .set(enemyAddress1, enemy1).toOption.get
                                      .set(enemyAddress2, enemy2).toOption.get
                                      .board

    //        println(BoardView(Board((board))))
    queen.availableMoves(address.filedIndex).toList.sortBy(identity) shouldBe Set(3, 11, 12, 5, 18, 17, 27, 35, 20, 21, 22, 23, 40, 33, 26, 28, 37, 46, 55).toList.sortBy(identity)
  }


}
