package com.chess.game

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class AddressSpec extends AnyFlatSpec with EitherValues {
  behavior of "Address"
  val emptyBoard: Board = Board()

  it should "A1 is 56" in {
    val address = Address("A1")

    address shouldBe Right(Address(0, 7))
    address.map(_.toString) shouldBe Right("A1")
    address.map(_.filedIndex) shouldBe Right(56)
  }

  it should "a8 is 0" in {
    val address = Address("a8")

    address shouldBe Right(Address(0, 0))
    address.map(_.toString) shouldBe Right("A8")
    address.map(_.filedIndex) shouldBe Right(0)
  }

  it should "d7 is 11" in {
    val address = Address("d7")

    address shouldBe Right(Address(3, 1))
    address.map(_.toString) shouldBe Right("D7")
    address.map(_.filedIndex) shouldBe Right(11)
  }

  it should "h1 is 63" in {
    val address = Address("h1")

    address shouldBe Right(Address(7, 7))
    address.map(_.toString) shouldBe Right("H1")
    address.map(_.filedIndex) shouldBe Right(63)
  }

  it should "56 is A1" in {
    val address = Address(56)

    address.map(_.filedIndex) shouldBe Right(56)
    address shouldBe Right(Address(0, 7))
    address.map(_.toString) shouldBe Right("A1")
  }

  it should "63 is h1" in {
    val address = Address(63)

    address.map(_.filedIndex) shouldBe Right(63)
    address shouldBe Right(Address(7, 7))
    address.map(_.toString) shouldBe Right("H1")
  }
}
