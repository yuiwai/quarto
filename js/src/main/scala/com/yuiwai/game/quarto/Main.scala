package com.yuiwai.game.quarto

import Quarto._
import org.scalajs.dom
import org.scalajs.dom.raw.{HTMLCanvasElement, CanvasRenderingContext2D => Ctx}

object Main {
  import RandomDecider.{given}
  lazy val canvas = dom.document.createElement("canvas")
    .asInstanceOf[HTMLCanvasElement]
  lazy val ctx = canvas.getContext("2d").asInstanceOf[Ctx]
  private var quarto = Quarto.init()
  private var viewState = ViewState.Progress(quarto.pieces)
  def main(args: Array[String]): Unit = {
    canvas.setAttribute("width", "500")
    canvas.setAttribute("height", "500")
    canvas.onclick = _ => update
    dom.document.body.appendChild(canvas)
    draw(ViewState.Progress(quarto.pieces))(using ctx)
  }
  
  def update: Unit = {
    viewState match
      case _: ViewState.Progress =>
        quarto.next match
          case QuartoResult.Processing(q, _) =>
            viewState = ViewState.Progress(q.pieces)
            quarto = q
            draw(viewState)(using ctx)
          case QuartoResult.Finished(winner, q, _) =>
            viewState = ViewState.Finished(q.pieces)
            draw(viewState)(using ctx)
            quarto = q
            dom.console.log(s"$winner won!")
          case _ =>
      case _: ViewState.Finished =>
  }
  
  // TODO QuartoResult => ViewState

  def draw(viewState: ViewState)(using ctx: Ctx): Unit = {
    for {
      x <- 0 until 4
      y <- 0 until 4
      pos = Pos.fromIndex(x + y * 4)
      piece = viewState.pieces(pos)
    } {
      ctx.strokeStyle = "black"
      ctx.lineWidth = 2
      
      BoardRenderer.drawPiece(pos, piece)
    }
  }
}

object BoardRenderer:
  type WithCtx = Ctx ?=> Unit
  val tileSize = 40
  def drawPiece(pos: Pos, piece: Piece): WithCtx = (using ctx) =>
    // shape
    if piece.isSquare then 
      withFillColor(piece.color)
      drawSquare(pos, 30.0)
      ctx.fill()
    else if piece.isCircle then  
      withFillColor(piece.color)
      drawCircle(pos, 30.0)
      ctx.fill()
    else 
      ctx.fillStyle = "lightgrey"
      drawSquare(pos, tileSize)
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

  def drawSquare(pos: Pos, size: Double): WithCtx = (using ctx) =>
    drawPath(ctx.rect(pos.x * 50 + (tileSize - size) / 2, pos.y * 50 + (tileSize - size) / 2, size, size))
  def drawCircle(pos: Pos, size: Double): WithCtx = (using ctx) =>
    drawPath(ctx.arc(pos.x * 50 + 20, pos.y * 50 + 20, size / 2, 0, Math.PI * 2))  
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
  case Progress(pieces: Map[Pos, Piece])
  case Finished(pieces: Map[Pos, Piece])
