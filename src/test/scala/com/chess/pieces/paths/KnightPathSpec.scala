package com.chess.pieces.paths

import com.chess.pieces.Board
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class KnightPathSpec extends AnyFlatSpec {
  behavior of "ForwardPath"
  val emptyBoard: Board = Board()

  it should "return all steps for Knight in the middle of board" in {
    KnightPath(1)(emptyBoard.board, 35).toSet shouldBe Seq(25, 18, 20, 29, 41, 50, 52, 45).toSet
  }

  it should "return all steps for Knight in bottom right corner" in {
    KnightPath(1)(emptyBoard.board, 63).toSet shouldBe Seq(53, 46).toSet
  }

  it should "return all steps for Knight in the upper middle" in {
    KnightPath(1)(emptyBoard.board, 3).toSet shouldBe Seq(9, 18, 20, 13).toSet
  }

  it should "return all steps for Knight in the bottom middle" in {
    KnightPath(1)(emptyBoard.board, 59).toSet shouldBe Seq(49,42,44,53).toSet
  }

  it should "return all steps for Knight in the left middle" in {
    KnightPath(1)(emptyBoard.board, 32).toSet shouldBe Seq(17, 26, 42, 49).toSet
  }

  it should "return all steps for Knight in the right middle" in {
    KnightPath(1)(emptyBoard.board, 39).toSet shouldBe Seq(22, 29, 45, 54).toSet
  }
}