package com.yuiwai.game.quarto

import Quarto._
import org.scalajs.dom
import org.scalajs.dom.raw.{HTMLCanvasElement, CanvasRenderingContext2D => Ctx}

import scala.concurrent.{Future, Promise}

object Main {
  import scala.concurrent.ExecutionContext.Implicits.global
  given Decider[Future] = 
    CompositeDecider(
      PlayerDecider(playerGiveHandler(_, _), playerPutHander(_, _, _)), 
      FutureWrappedDecider(SimpleDecider.decider))
  lazy val canvas = dom.document.createElement("canvas")
    .asInstanceOf[HTMLCanvasElement]
  lazy val ctx = canvas.getContext("2d").asInstanceOf[Ctx]
  lazy val info = dom.document.createElement("div")
  private var quarto = Quarto.init()
  private var viewState = ViewState.Progress(quarto.pieces, quarto.black, quarto.white)
  def main(args: Array[String]): Unit = {
    canvas.setAttribute("width", "500")
    canvas.setAttribute("height", "500")
    canvas.onclick = _ => update
    info.innerHTML = "&nbsp;"
    dom.document.body.appendChild(info)
    dom.document.body.appendChild(canvas)
    draw(ViewState.Progress(quarto.pieces, quarto.black, quarto.white))(using ctx)
  }
  
  def playerGiveHandler(quarto: Quarto[Future], promise: Promise[Option[Piece]]): Unit =
    dom.document.onkeypress = _ => promise.success(quarto.second.hand.headOption)  
  def playerPutHander(quarto: Quarto[Future], piece: Piece, promise: Promise[Option[Pos]]): Unit =
    dom.document.onkeypress = _ => promise.success(quarto.board.spaces.headOption)
  
  def update: Unit = {
    viewState match
      case _: ViewState.Progress =>
        quarto.next.map {
          case QuartoResult.Processing(q, _) =>
            viewState = ViewState.Progress(q.pieces, q.black, q.white)
            quarto = q
            draw(viewState)(using ctx)
          case QuartoResult.Finished(winner, q, _) =>
            viewState = ViewState.Finished(q.pieces)
            draw(viewState)(using ctx)
            quarto = q
            dom.console.log(s"$winner won!")
          case _ =>
        }
      case _: ViewState.Finished =>
  }
  
  // TODO QuartoResult => ViewState

  def draw(viewState: ViewState)(using ctx: Ctx): Unit = {
    ctx.clearRect(0, 0, 500, 500)
    for {
      x <- 0 until 4
      y <- 0 until 4
      pos = Pos.fromIndex(x + y * 4)
      piece = viewState.pieces(pos)
    } {
      ctx.strokeStyle = "black"
      ctx.lineWidth = 2
      
      BoardRenderer.drawTile(pos, piece)
    }
  }
}

object BoardRenderer:
  type WithCtx = Ctx ?=> Unit
  val tileSize = 40
  val unitSize = 50
  
  def drawPiece(pos: Pos, piece: Piece): WithCtx = (using ctx) => ()
  def drawPiece(x: Double, y: Double, piece: Piece): WithCtx = (using ctx) => ()
  
  def drawTile(pos: Pos, piece: Piece): WithCtx = (using ctx) =>
    // background
    ctx.fillStyle = "lightgrey"
    drawSquare(pos, tileSize)
    ctx.fill()
    // shape
    if piece.isSquare then 
      withFillColor(piece.color)
      drawSquare(pos, 30.0)
      ctx.fill()
    else if piece.isCircle then  
      withFillColor(piece.color)
      drawCircle(pos, 30.0)
      ctx.fill()
    // hole
    if piece.hasHole then
      ctx.fillStyle = "grey"
      drawCircle(pos, 16)
      ctx.fill()
    // tall
    if piece.isTall then
      if piece.isSquare then
        drawSquare(pos, 36)
        ctx.stroke()
      else if piece.isCircle then
        drawCircle(pos, 36)
        ctx.stroke()

  def drawSquare(pos: Pos, size: Double): WithCtx =
    drawSquare(pos.x * unitSize + (tileSize - size) / 2, pos.y * unitSize + (tileSize - size) / 2, size)
  def drawSquare(x: Double, y: Double, size: Double): WithCtx = (using ctx) =>
    drawPath(ctx.rect(x, y, size, size))
  def drawCircle(pos: Pos, size: Double): WithCtx = (using ctx) =>
    drawPath(ctx.arc(pos.x * unitSize + 20, pos.y * unitSize + 20, size / 2, 0, Math.PI * 2))  
  def drawPath(f: WithCtx): WithCtx = (using ctx) =>
    ctx.beginPath(); f; ctx.closePath()
  def withFillColor(color: Color): WithCtx = (using ctx) => 
    ctx.fillStyle = color.toStyle
  extension(color: Color):
    def toStyle: String = color match
      case Color.Black => "black"
      case Color.White => "white"

enum ViewState:
  val pieces: Map[Pos, Piece]
  case Progress(pieces: Map[Pos, Piece], black: Set[Piece], white: Set[Piece])
  case Finished(pieces: Map[Pos, Piece])
object ViewState

final class PlayerDecider(
  giveHandler: (quarto: Quarto[Future], promise: Promise[Option[Piece]]) => Unit,
  putHandler: (quarto: Quarto[Future], piece: Piece, promise: Promise[Option[Pos]]) => Unit)
  extends Decider[Future]:
  def give(quarto: Quarto[Future]): Future[Option[Piece]] = {
    val promise: Promise[Option[Piece]] = Promise()
    giveHandler(quarto, promise)
    promise.future
  }
  override def put(quarto: Quarto[Future], piece: Piece): Future[Option[Pos]] = {
    val promise: Promise[Option[Pos]] = Promise()
    putHandler(quarto, piece, promise)
    promise.future
  }