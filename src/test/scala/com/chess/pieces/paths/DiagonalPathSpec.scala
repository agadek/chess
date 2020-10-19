package com.chess.pieces.paths

import com.chess.pieces.Board
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class DiagonalPathSpec extends AnyFlatSpec {
  behavior of "DiagonalPath"
  val emptyBoard: Board = Board()

  it should "return one step down-right" in {
    DiagonalPath(1, 0, false, false)(emptyBoard.board) shouldBe Seq(9)
  }

  it should "return one step down-left" in {
    DiagonalPath(1, 7, false, false)(emptyBoard.board) shouldBe Seq(14)
  }

  it should "return long diagonal path down-left and down-right" in {
    DiagonalPath(8, 3, false, false)(emptyBoard.board).toSet shouldBe Seq(24, 17, 10, 12, 21, 30, 39).toSet
  }

  it should "return down-right and down-left" in {
    DiagonalPath(1, 1, false, false)(emptyBoard.board) shouldBe Seq(8, 10)
  }

  it should "return one step up-right" in {
    DiagonalPath(-1, 56, false, false)(emptyBoard.board) shouldBe Seq(49)
  }

  it should "return one step up-left" in {
    DiagonalPath(-1, 55, false, false)(emptyBoard.board) shouldBe Seq(46)
  }

  it should "return steps up-right and up-left" in {
    DiagonalPath(-1, 57, false, false)(emptyBoard.board).toSet shouldBe Seq(50, 48).toSet
  }

  it should "return long diagonal path up-left and up-right" in {
    DiagonalPath(-8, 43, false, false)(emptyBoard.board).toSet shouldBe Seq(34, 25, 16, 36, 29, 22, 15).toSet
  }
}
