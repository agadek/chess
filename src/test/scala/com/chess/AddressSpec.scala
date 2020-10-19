package com.chess

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class AddressSpec extends AnyFlatSpec with EitherValues {
  behavior of "Address"
  val emptyBoard: Board = Board()

  it should "A1 is 56" in {
    val address = Address("A1")

    address shouldBe Right(Address('A', 1))
    address.map(_.filedIndex) shouldBe Right(56)
  }

  it should "a8 is 0" in {
    val address = Address("a8")

    address shouldBe Right(Address('A', 8))
    address.map(_.filedIndex) shouldBe Right(0)
  }

  it should "h1 is 63" in {
    val address = Address("h1")

    address shouldBe Right(Address('H', 1))
    address.map(_.filedIndex) shouldBe Right(63)
  }
}
