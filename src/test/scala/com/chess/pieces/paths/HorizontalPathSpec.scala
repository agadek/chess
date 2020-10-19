package com.chess.pieces.paths

import com.chess.pieces.Board
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class HorizontalPathSpec extends AnyFlatSpec {
  behavior of "ForwardPath"
  val emptyBoard: Board = Board()

  it should "return one step horizontally" in {
    HorizontalPath(1)(emptyBoard.board, 35) shouldBe Seq(34, 36)
  }

  it should "return one step horizontally right" in {
    HorizontalPath(1)(emptyBoard.board, 0) shouldBe Seq(1)
  }

  it should "return one step horizontally left" in {
    HorizontalPath(1)(emptyBoard.board, 7) shouldBe Seq(6)
  }

  it should "return whole row move 1" in {
    HorizontalPath(8)(emptyBoard.board, 35).toSet shouldBe Seq(32, 33, 34, 36, 37, 38, 39).toSet
  }

  it should "return whole row move 2" in {
    HorizontalPath(8)(emptyBoard.board, 32).toSet shouldBe Seq(33, 34, 35, 36, 37, 38, 39).toSet
  }

  it should "return whole row move 3" in {
    HorizontalPath(8)(emptyBoard.board, 39).toSet shouldBe Seq(32, 33, 34, 35, 36, 37, 38).toSet
  }

}
