package com.chess.pieces.paths

import com.chess.pieces.Board
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class DiagonalPathSpec extends AnyFlatSpec {
  behavior of "DiagonalPath"
  val emptyBoard: Board = Board()

  it should "return one step down-right" in {
    DiagonalPath(1)(emptyBoard.board, 0) shouldBe Seq(9)
  }

  it should "return one step down-left" in {
    DiagonalPath(1)(emptyBoard.board, 7) shouldBe Seq(14)
  }

  it should "return long diagonal path down-left and down-right" in {
    DiagonalPath(8)(emptyBoard.board, 3).toSet shouldBe Seq(24, 17, 10, 12, 21, 30, 39).toSet
  }

  it should "return down-right and down-left" in {
    DiagonalPath(1)(emptyBoard.board, 1) shouldBe Seq(8, 10)
  }

  it should "return one step up-right" in {
    DiagonalPath(-1)(emptyBoard.board, 56) shouldBe Seq(49)
  }

  it should "return one step up-left" in {
    DiagonalPath(-1)(emptyBoard.board, 55) shouldBe Seq(46)
  }

  it should "return steps up-right and up-left" in {
    DiagonalPath(-1)(emptyBoard.board, 57).toSet shouldBe Seq(50, 48).toSet
  }

  it should "return long diagonal path up-left and up-right" in {
    DiagonalPath(-8)(emptyBoard.board, 43).toSet shouldBe Seq(34, 25, 16, 36, 29, 22, 15).toSet
  }
}
