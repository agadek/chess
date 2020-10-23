package com.chess

import com.chess.game.{Address, BoardErrorWrapper, InvalidMove}
import com.chess.view.ViewUtil.BackgroundRemover
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class GameControllerSpec extends AnyFlatSpec with EitherValues {
  behavior of "GameController"

  it should "work with sample-moves.txt" in {
    val game = GameController(fileMode = true, inputF=  () => "")


      val newGame = game.next(Some("src/test/resources/sample-moves.txt"))

    newGame.toOption.get.printBoardState().removeBackground.trim shouldBe
      """Black KIA:
        |
        |-ABCDEFGH-
        |8rn qkbnr8
        |7ppp  ppp7
        |6   pb   6
        |5    p   5
        |4    P   4
        |3  N    P3
        |2PPPP PPR2
        |1R BQKBN 1
        |-ABCDEFGH-
        |
        |White KIA:
        |
        |--------------------------""".stripMargin
  }

  it should "fail with sample-moves-invalid.txt" in {
    val game = GameController(fileMode = true, inputF=  () => "")


    val newGame = game.next(Some("src/test/resources/sample-moves-invalid.txt"))

    newGame.left.value shouldBe BoardErrorWrapper(InvalidMove(Address("B1").toOption.get, Address("B3").toOption.get))

  }

  it should "work with checkmate.txt and detect end of game" in {
    val game = new GameController(fileMode = true, inputF=  () => "")


    val newGame = game.next(Some("src/test/resources/checkmate.txt"))

    newGame.toOption.get.printBoardState().removeBackground.trim shouldBe
      """Black KIA:p
        |
        |-ABCDEFGH-
        |8r bqkbnr8
        |7ppp  Qpp7
        |6  np    6
        |5    p   5
        |4  B P   4
        |3        3
        |2PPPP PPP2
        |1RNB K NR1
        |-ABCDEFGH-
        |
        |White KIA:
        |
        |--------------------------
        |You are in check! From:(Queen,F7)""".stripMargin

    newGame.toOption.get.isDone shouldBe true
  }

}
