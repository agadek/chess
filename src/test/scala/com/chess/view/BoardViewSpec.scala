package com.chess.view

import com.chess.pieces.Board
import com.chess.view.ViewUtil.BackgroundRemover
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class BoardViewSpec extends AnyFlatSpec {
  behavior of "BoardView"

  it should "display proper clean Board" in {
    val emptyBoard = Board()

    BoardView(emptyBoard).removeBackground shouldBe
      """-ABCDEFGH-
        |8        8
        |7        7
        |6        6
        |5        5
        |4        4
        |3        3
        |2        2
        |1        1
        |-ABCDEFGH-""".stripMargin
  }

  it should "display proper new default setup Board (white on bottom)" in {
    val board = Board.build()

    BoardView(board).removeBackground shouldBe
      """-ABCDEFGH-
        |8rbnqknbr8
        |7pppppppp7
        |6        6
        |5        5
        |4        4
        |3        3
        |2PPPPPPPP2
        |1RBNQKNBR1
        |-ABCDEFGH-""".stripMargin
  }
}
