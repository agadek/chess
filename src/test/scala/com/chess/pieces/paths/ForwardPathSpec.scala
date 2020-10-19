package com.chess.pieces.paths

import com.chess.pieces.Board
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class ForwardPathSpec extends AnyFlatSpec {
  behavior of "ForwardPath"
  val emptyBoard: Board = Board()

  it should "return one step down" in {
    ForwardPath(1, 0)(emptyBoard.board) shouldBe Seq(8)
  }

  it should "return two steps down" in {
    ForwardPath(2, 0)(emptyBoard.board) shouldBe Seq(8, 16)
  }

  it should "return long path down" in {
    ForwardPath(10, 0)(emptyBoard.board).toSet shouldBe Seq(8, 16, 24, 32, 40, 48, 56).toSet
  }

  it should "return one step up" in {
    ForwardPath(-1, 56)(emptyBoard.board) shouldBe Seq(48)
  }

  it should "return two steps up" in {
    ForwardPath(-2, 56)(emptyBoard.board) shouldBe Seq(48, 40)
  }

  it should "return long path up" in {
    ForwardPath(-10, 56)(emptyBoard.board).toSet shouldBe Seq(0, 8, 16, 24, 32, 40, 48).toSet
  }
}
