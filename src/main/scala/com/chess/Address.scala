package com.chess

case class Address(col: Char, row: Int) {
  override def toString = col.toString + row

  private lazy val nummericCol = col match {
    case 'A' => 0
    case 'B' => 1
    case 'C' => 2
    case 'D' => 3
    case 'E' => 4
    case 'F' => 5
    case 'G' => 6
    case 'H' => 7
  }

  def filedIndex = (8 - row) * 8 + nummericCol
}

object Address {
  private val r = "([a-hA-H])([1-8])".r

  def apply(address: String): Either[InvalidAddress, Address] = address match {
    case r(col, row) => Right(Address(col.toUpperCase.head, row.toInt))
    case _ => Left(InvalidAddress(address))

  }
}