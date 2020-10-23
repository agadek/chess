package com.chess.view

import com.chess.pieces.Pawn
import com.chess.view.ViewUtil.BackgroundRemover
import com.chess.game.{Address, Board}
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
        |8rnbqkbnr8
        |7pppppppp7
        |6        6
        |5        5
        |4        4
        |3        3
        |2PPPPPPPP2
        |1RNBQKBNR1
        |-ABCDEFGH-""".stripMargin
  }

  it should "display proper Board with some figures" in {

    val address = Seq("C6", "E6", "D5", "C4", "E4").map(Address(_).toOption.get)

    val board = address.foldLeft(Board())((board, address) => board.set(address, Pawn(isWhite = true)).toOption.get)

    BoardView(board).removeBackground shouldBe
      """-ABCDEFGH-
        |8        8
        |7        7
        |6  P P   6
        |5   P    5
        |4  P P   4
        |3        3
        |2        2
        |1        1
        |-ABCDEFGH-""".stripMargin
  }
}
