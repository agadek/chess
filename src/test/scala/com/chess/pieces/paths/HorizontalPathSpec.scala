package com.chess.pieces.paths

import com.chess.game.{Address, Board}
import com.chess.pieces.Pawn
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class HorizontalPathSpec extends AnyFlatSpec {
  behavior of "HorizontalPath"
  val emptyBoard: Board = Board()
  implicit val board = emptyBoard.board
  implicit val piece = Pawn(isWhite = true)

  it should "return one step horizontally" in {
    HorizontalPath(1, 35) shouldBe Seq(34, 36)
  }

  it should "return one step horizontally right" in {
    HorizontalPath(1, 0) shouldBe Seq(1)
  }

  it should "return one step horizontally left" in {
    HorizontalPath(1, 7) shouldBe Seq(6)
  }

  it should "return whole row move 1" in {
    HorizontalPath(8, 35).toSet shouldBe Seq(32, 33, 34, 36, 37, 38, 39).toSet
  }

  it should "return whole row move 2" in {
    HorizontalPath(8, 32).toSet shouldBe Seq(33, 34, 35, 36, 37, 38, 39).toSet
  }

  it should "return whole row move 3" in {
    HorizontalPath(8, 39).toSet shouldBe Seq(32, 33, 34, 35, 36, 37, 38).toSet
  }

  behavior of "HorizontalPath with obstacles"
  it should "return break long path down if blocked by same player pawn" in {
    implicit val board = emptyBoard.set(Address("F4").toOption.get, Pawn(isWhite = true)).toOption.get.board

    HorizontalPath(length = 8, location = 35).toSet shouldBe Seq(32, 33, 34, 36).toSet
  }

  it should "return break long path down if blocked and allow attack when possible" in {
    implicit val board = emptyBoard.set(Address("F4").toOption.get, Pawn(isWhite = false)).toOption.get.board

    HorizontalPath(8, 35, attack = true).toSet shouldBe Seq(37).toSet
  }
}
