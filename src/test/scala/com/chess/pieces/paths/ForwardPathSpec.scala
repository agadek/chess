package com.chess.pieces.paths

import com.chess.pieces.Pawn
import com.chess.view.BoardView
import com.chess.{Address, Board}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class ForwardPathSpec extends AnyFlatSpec {
  behavior of "ForwardPath"
  val emptyBoard: Board = Board()
  implicit val board = emptyBoard.board
  implicit val piece = Pawn(isWhite = true)

  it should "return one step down" in {
    ForwardPath(1, 0) shouldBe Seq(8)
  }

  it should "return two steps down" in {
    ForwardPath(2, 0) shouldBe Seq(8, 16)
  }

  it should "return long path down" in {
    ForwardPath(10, 0).toSet shouldBe Seq(8, 16, 24, 32, 40, 48, 56).toSet
  }

  it should "return one step up" in {
    ForwardPath(-1, 56) shouldBe Seq(48)
  }

  it should "return two steps up" in {
    ForwardPath(-2, 56) shouldBe Seq(48, 40)
  }

  it should "return long path up" in {
    ForwardPath(-10, 56).toSet shouldBe Seq(0, 8, 16, 24, 32, 40, 48).toSet
  }

  behavior of "ForwardPath with obstacles"
  it should "return break long path down if blocked" in {
    implicit val board = emptyBoard.set(Address("A4").toOption.get, Pawn(isWhite = true)).toOption.get.board

    ForwardPath(length = 10, location = 0, attack = false).toSet shouldBe Seq(8, 16, 24).toSet
  }

  it should "return break long path down if blocked and allow attack when possible" in {
    implicit val board = emptyBoard.set(Address("A4").toOption.get, Pawn(isWhite = false)).toOption.get.board

    ForwardPath(length = 10, location = 0, attack = true) shouldBe Seq(32)
  }
}
