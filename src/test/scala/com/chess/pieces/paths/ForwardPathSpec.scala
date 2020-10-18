package com.chess.pieces.paths

import com.chess.pieces.Board
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class ForwardPathSpec extends AnyFlatSpec {
  behavior of "ForwardPath"
  val emptyBoard = Board()

  it should "return one step down" in {
    ForwardPath(1)(emptyBoard.board, 0) shouldBe Seq(8)
  }

  it should "return two steps down" in {
    ForwardPath(2)(emptyBoard.board, 0) shouldBe Seq(8,16)
  }

  it should "return one step up" in {
    ForwardPath(-1)(emptyBoard.board, 56) shouldBe Seq(48)
  }

  it should "return two steps up" in {
    ForwardPath(-2)(emptyBoard.board, 56) shouldBe Seq(48,40)
  }
}
