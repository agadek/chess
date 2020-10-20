package com.chess

import cats.syntax.either._

case class Address(col: Int, row: Int) {
  override def toString = Address.alfaCol(col).toString + row

  def filedIndex = (8 - row) * 8 + col
}

object Address {
  private val r = "([a-hA-H])([1-8])".r

  def alfaCol(col: Int): Char = (65 + col).toChar

  def numericCol(col: Char): Int = col.toInt - 65

  def apply(address: String): Either[InvalidAddress, Address] = {

    address match {
      case r(col, row) => Either.catchNonFatal(Address(numericCol(col.toUpperCase.head), row.toInt)).leftMap(_ => InvalidAddress(address))
      case _           => Left(InvalidAddress(address))
    }
  }

  def apply(address: (Int, Int)): Either[InvalidAddress, Address] = {
    def cond(index: Int) = index >= 0 && index <= 7

    Either.cond(cond(address._1) && cond(address._2), Address(address._1, address._2), InvalidAddress(s"${address._1},${address._2}"))
  }
}