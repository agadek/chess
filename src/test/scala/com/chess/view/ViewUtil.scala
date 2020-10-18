package com.chess.view

object ViewUtil {

  implicit class BackgroundRemover(string: String) {
    def removeBackground: String = ViewUtil.removeBackground(string)

    def removeNewLines: String = ViewUtil.removeNewLines(string)

  }


  def removeBackground(string: String): String = string.replaceAll("\u001B\\[[;\\d]*m", "")

  def removeNewLines(string: String): String = string.replaceAll("\n", "")
}
