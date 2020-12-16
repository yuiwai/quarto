package com.yuiwai.game.quarto

import org.scalajs.dom
import scala.concurrent.{Future, Promise}
import scala.language.postfixOps

object Main:
  def main(args: Array[String]): Unit =
    import scala.concurrent.ExecutionContext.Implicits.global
    given Decider[Future] =
      CompositeDecider(
        PlayerDecider(QuartoApp.emit),
        EventHandledFutureWrappedDecider(SimpleDecider.decider, QuartoApp.emit))
    QuartoApp[Future](dom.document.body).init()
