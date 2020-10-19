package com.chess.pieces.paths

import com.chess.pieces.Board
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class KnightPathSpec extends AnyFlatSpec {
  behavior of "ForwardPath"
  val emptyBoard: Board = Board()

  it should "return all steps for Knight in the middle of board" in {
    KnightPath(1, 35)(emptyBoard.board).toSet shouldBe Seq(25, 18, 20, 29, 41, 50, 52, 45).toSet
  }

  it should "return all steps for Knight in bottom right corner" in {
    KnightPath(1, 63)(emptyBoard.board).toSet shouldBe Seq(53, 46).toSet
  }

  it should "return all steps for Knight in the upper middle" in {
    KnightPath(1, 3)(emptyBoard.board).toSet shouldBe Seq(9, 18, 20, 13).toSet
  }

  it should "return all steps for Knight in the bottom middle" in {
    KnightPath(1, 59)(emptyBoard.board).toSet shouldBe Seq(49, 42, 44, 53).toSet
  }

  it should "return all steps for Knight in the left middle" in {
    KnightPath(1, 32)(emptyBoard.board).toSet shouldBe Seq(17, 26, 42, 49).toSet
  }

  it should "return all steps for Knight in the right middle" in {
    KnightPath(1, 39)(emptyBoard.board).toSet shouldBe Seq(22, 29, 45, 54).toSet
  }
}
