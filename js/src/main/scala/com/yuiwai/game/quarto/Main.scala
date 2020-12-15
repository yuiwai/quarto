package com.yuiwai.game.quarto

import Quarto._
import org.scalajs.dom
import org.scalajs.dom.raw.{HTMLButtonElement, HTMLCanvasElement, CanvasRenderingContext2D}

import scala.concurrent.{Future, Promise}
import scala.language.postfixOps

type State = CanvasRenderingState

object Main: 
  def main(args: Array[String]): Unit =
    import scala.concurrent.ExecutionContext.Implicits.global
    given Decider[Future] =
      CompositeDecider(
        PlayerDecider(playerGiveHandler(_, _), playerPutHander(_, _, _)),
        FutureWrappedDecider(SimpleDecider.decider))
    QuartoApp[Future](dom.document.body).init()
  def playerGiveHandler(quarto: Quarto, promise: Promise[Option[Piece]]): Unit =
    dom.document.onkeypress = _ => promise.success(quarto.second.hand.headOption)
  def playerPutHander(quarto: Quarto, piece: Piece, promise: Promise[Option[Pos]]): Unit =
    dom.document.onkeypress = _ => promise.success(quarto.board.spaces.headOption)

final case class Offset(x: Double, y: Double)
object Offset:
  val zero: Offset = apply(0, 0)

final case class ViewState(
  pieces: Map[Pos, Piece],
  black: Set[Piece],
  white: Set[Piece],
  reaches: Seq[Line],
  quartoLine: Option[Line],
  winner: Option[Color]
):
  def isFinished: Boolean = winner.isDefined
  def finish(winner: Color, pieces: Map[Pos, Piece], quartoLine: Option[Line]): ViewState = 
    copy(winner = Some(winner), pieces = pieces, quartoLine = quartoLine)

object ViewState:
  enum Phase:
    case GivePiece()
    case PutPiece()
  def progress(quarto: Quarto): ViewState =
    ViewState(quarto.pieces, quarto.black, quarto.white, quarto.reaches, None, None)
