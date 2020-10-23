package com.chess

import cats.instances.either._
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.traverse._
import com.chess.game.{Address, BoardErrorWrapper, GameError, InputFileError}
import com.whitehatgaming.UserInputFile

object FileManager {
  def apply(file: String): Either[GameError, List[(Address, Address)]] = {
    for {
      file <- Either.catchNonFatal(new UserInputFile(file)).leftMap(InputFileError(file, _))
      moves <- IteratorWrapper(file)
        .toList
        .traverse { row =>
          for {
            from <- Address(row.from)
            to <- Address(row.to)
          } yield from -> to
        }.leftMap(BoardErrorWrapper(_))
    } yield moves
  }

  private case class Row(from: (Int, Int), to: (Int, Int))

  private case class IteratorWrapper(userInputFile: UserInputFile) extends Iterator[Row] {

    class IteratorWrapperException extends Exception

    private var buffer: Option[Row] = getOne.toOption

    private def getOne: Either[Throwable, Row] =
      for {
        next <- Either.catchNonFatal(userInputFile.nextMove())
        row <- Either.catchNonFatal(Row(from = next(0) -> next(1), to = next(2) -> next(3)))
      } yield row

    override def hasNext: Boolean = buffer.isDefined

    override def next(): Row = {
      val tmp = buffer.fold(throw new IteratorWrapperException)(identity)
      buffer = getOne.toOption
      tmp
    }
  }

}
