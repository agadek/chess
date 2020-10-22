package com.chess

import com.chess.pieces.{King, Queen, Rook}
import com.chess.view.BoardView
import com.chess.view.ViewUtil.BackgroundRemover
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.contain
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class BoardSpec extends AnyFlatSpec with EitherValues {
  behavior of "Address"
  val whiteKing = King(isWhite = true)
  val blackKing = King(isWhite = false)
  val whiteKingAddress = Address("H1").toOption.get
  val blackKingAddress = Address("H8").toOption.get
  val emptyBoard: Board = Board().set(whiteKingAddress, whiteKing).toOption.get
                                    .set(blackKingAddress, blackKing).toOption.get

  it should "not move piece from empty field" in {
    val address = Address("A1").toOption.get
    implicit val player = White

    emptyBoard.move(address, address).left.value shouldBe EmptyAddress(address)

  }

  it should "not move piece from A to A" in {
    implicit val player = White
    val address = Address("A1").toOption.get
    val queen = Queen(isWhite = true)
    val board = emptyBoard.set(address, queen).toOption.get

    board.move(address, address).left.value shouldBe InvalidMove(address, address)

  }
  it should "not be able to move enemy piece from A to B" in {
    implicit val player = White
    val addressA = Address("A1").toOption.get
    val addressB = Address("A8").toOption.get
    val queen = Queen(isWhite = false)
    val board = emptyBoard.set(addressA, queen).toOption.get
    BoardView(board).removeBackground shouldBe
      """-ABCDEFGH-
        |8       k8
        |7        7
        |6        6
        |5        5
        |4        4
        |3        3
        |2        2
        |1q      K1
        |-ABCDEFGH-""".stripMargin

    board.move(addressA, addressB).left.value shouldBe NotYoursPiece(addressA, White, queen)
  }

  it should "be able to move piece from A to B" in {
    implicit val player = White
    val addressA = Address("A1").toOption.get
    val addressB = Address("A8").toOption.get
    val queen = Queen(isWhite = true)
    val board = emptyBoard.set(addressA, queen).toOption.get
    BoardView(board).removeBackground shouldBe
      """-ABCDEFGH-
        |8       k8
        |7        7
        |6        6
        |5        5
        |4        4
        |3        3
        |2        2
        |1Q      K1
        |-ABCDEFGH-""".stripMargin

    BoardView(board.move(addressA, addressB).toOption.get).removeBackground shouldBe
      """-ABCDEFGH-
        |8Q      k8
        |7        7
        |6        6
        |5        5
        |4        4
        |3        3
        |2        2
        |1       K1
        |-ABCDEFGH-""".stripMargin

  }

  it should "not be able to move piece from A to B if puts the King in danger" in {
    implicit val player = Black
    val whiteQueen = Queen(isWhite = true)
    val addressQueen = Address("A8").toOption.get
    val blackRook = Rook(isWhite = false)
    val addressRookA = Address("C8").toOption.get
    val addressRookB = Address("C7").toOption.get
    val board = emptyBoard.set(addressQueen, whiteQueen).toOption.get
                          .set(addressRookA, blackRook).toOption.get

    BoardView(board).removeBackground shouldBe
      """-ABCDEFGH-
        |8Q r    k8
        |7        7
        |6        6
        |5        5
        |4        4
        |3        3
        |2        2
        |1       K1
        |-ABCDEFGH-""".stripMargin

    board.move(addressRookA, addressRookB).left.value shouldBe PossibleCheck(Black, Vector((whiteQueen, addressQueen)))
  }

  it should "not be able to make illegal move over some piece" in {
    implicit val player = White
    val whiteQueen = Queen(isWhite = true)
    val addressQueen = Address("A8").toOption.get
    val blackRook = Rook(isWhite = false)
    val addressRookA = Address("C8").toOption.get
    val board = emptyBoard.set(addressQueen, whiteQueen).toOption.get
      .set(addressRookA, blackRook).toOption.get

    BoardView(board).removeBackground shouldBe
      """-ABCDEFGH-
        |8Q r    k8
        |7        7
        |6        6
        |5        5
        |4        4
        |3        3
        |2        2
        |1       K1
        |-ABCDEFGH-""".stripMargin

    board.move(addressQueen, blackKingAddress).left.value shouldBe InvalidMove(addressQueen, blackKingAddress)
  }

  it should "be able to kill enemy!" in {
    implicit val player = White
    val whiteQueen = Queen(isWhite = true)
    val addressQueen = Address("A8").toOption.get
    val blackRook = Rook(isWhite = false)
    val addressRookA = Address("C8").toOption.get
    val board = emptyBoard.set(addressQueen, whiteQueen).toOption.get
                            .set(addressRookA, blackRook).toOption.get

    BoardView(board).removeBackground shouldBe
      """-ABCDEFGH-
        |8Q r    k8
        |7        7
        |6        6
        |5        5
        |4        4
        |3        3
        |2        2
        |1       K1
        |-ABCDEFGH-""".stripMargin

    val newBoard = board.move(addressQueen, addressRookA).toOption.get

    BoardView(newBoard).removeBackground shouldBe
      """-ABCDEFGH-
        |8  Q    k8
        |7        7
        |6        6
        |5        5
        |4        4
        |3        3
        |2        2
        |1       K1
        |-ABCDEFGH-""".stripMargin

    newBoard.checkKingStatus(White)            shouldBe Right(())
    newBoard.checkKingStatus(Black).left.value shouldBe PossibleCheck(Black, Vector((whiteQueen, addressRookA)))
    newBoard.kiaBlack                          should contain(blackRook)

  }
}
