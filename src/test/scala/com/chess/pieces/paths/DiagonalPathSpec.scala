package com.chess.pieces.paths

import com.chess.pieces.Pawn
import com.chess.{Address, Board}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class DiagonalPathSpec extends AnyFlatSpec with EitherValues{
  behavior of "DiagonalPath"
  val emptyBoard: Board = Board()
  implicit val board = emptyBoard.board
  implicit val piece = Pawn(isWhite = true)

  it should "return one step down-right" in {
    DiagonalPath(1, 0) shouldBe Seq(9)
  }

  it should "return one step down-left" in {
    DiagonalPath(1, 7) shouldBe Seq(14)
  }

  it should "return long diagonal path down-left and down-right" in {
    DiagonalPath(8, 3).toSet shouldBe Seq(24, 17, 10, 12, 21, 30, 39).toSet
  }

  it should "return down-right and down-left" in {
    DiagonalPath(1, 1) shouldBe Seq(8, 10)
  }

  it should "return one step up-right" in {
    DiagonalPath(-1, 56) shouldBe Seq(49)
  }

  it should "return one step up-left" in {
    DiagonalPath(-1, 55) shouldBe Seq(46)
  }

  it should "return steps up-right and up-left" in {
    DiagonalPath(-1, 57).toSet shouldBe Seq(50, 48).toSet
  }

  it should "return long diagonal path up-left and up-right" in {
    DiagonalPath(-8, 43).toSet shouldBe Seq(34, 25, 16, 36, 29, 22, 15).toSet
  }

  behavior of "DiagonalPath with obstacles"
  it should "return break long diagonal path down-left and down-right if blocked" in {
    implicit val board = emptyBoard.set(Address("B6").toOption.get, Pawn(isWhite = true)).toOption.get.board

    DiagonalPath(length = 8, location = 3, attack = false).toSet shouldBe Seq(10, 12, 21, 30, 39).toSet
  }

  it should "return diagonal down-left and down-right attack when possible" in {
    implicit val board = emptyBoard.set(Address("B6").toOption.get, Pawn(isWhite = false)).toOption.get.board

    DiagonalPath(length = 8, location = 3, attack = true) shouldBe Seq(17)
  }

  it should "return empty set if nothing to attack" in {
    implicit val board = emptyBoard.board

    DiagonalPath(length = 1, location = 0, attack = true).toSet shouldBe Seq().toSet
  }

  it should "return move if something to attack" in {
    implicit val board = emptyBoard.set(Address("B7").toOption.get, Pawn(isWhite = false)).toOption.get.board

    DiagonalPath(length = 1, location = 0, attack = true).toSet shouldBe Seq(9).toSet
  }

}
