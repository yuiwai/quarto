package com.yuiwai.game.quarto

import org.scalajs.dom

import scala.concurrent.duration.Duration

object Timer:
  def delay(f: () => Unit, duration: Duration): Unit = dom.window.setTimeout(f, duration.toMillis.toDouble)
