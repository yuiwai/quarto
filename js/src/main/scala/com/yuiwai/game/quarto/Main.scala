package com.yuiwai.game.quarto

import Quarto._
import org.scalajs.dom
import org.scalajs.dom.raw.{HTMLButtonElement, HTMLCanvasElement, CanvasRenderingContext2D}

import scala.concurrent.{Future, Promise}
import scala.language.postfixOps

type State = CanvasRenderingState

object Main {
  import scala.concurrent.ExecutionContext.Implicits.global
  given Decider[Future] = 
    CompositeDecider(
      PlayerDecider(playerGiveHandler(_, _), playerPutHander(_, _, _)), 
      FutureWrappedDecider(SimpleDecider.decider))
  lazy val canvas = dom.document.createElement("canvas")
    .asInstanceOf[HTMLCanvasElement]
  lazy val ctx = 
    CanvasRenderingState(canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D])
  lazy val infoArea = dom.document.createElement("div")
  lazy val resetBtn = dom.document.createElement("button").asInstanceOf[HTMLButtonElement]
  private var quarto = Quarto.init()
  private var viewState: ViewState = ViewState.progress(quarto)
  val operation = QuartoOperation[Future]
  
  def main(args: Array[String]): Unit = init()
  def init(): Unit = {
    canvas.setAttribute("width", "300")
    canvas.setAttribute("height", "400")
    canvas.onclick = _ => update()
    infoArea.innerHTML = "&nbsp;"
    resetBtn.innerText = "リセット"
    resetBtn.setAttribute("style", "display: none")
    resetBtn.onclick = _ => reset()
    dom.document.body.appendChild(infoArea)
    dom.document.body.appendChild(canvas)
    dom.document.body.appendChild(resetBtn)
    draw(viewState)(using ctx)
  }
  def reset(): Unit = {
    resetBtn.setAttribute("style", "display: none")
    infoArea.innerHTML = "&nbsp;"
    quarto = Quarto.init()
    viewState = ViewState.progress(quarto)
    draw(viewState)(using ctx)
  }
  
  def playerGiveHandler(quarto: Quarto, promise: Promise[Option[Piece]]): Unit =
    dom.document.onkeypress = _ => promise.success(quarto.second.hand.headOption)
  def playerPutHander(quarto: Quarto, piece: Piece, promise: Promise[Option[Pos]]): Unit =
    dom.document.onkeypress = _ => promise.success(quarto.board.spaces.headOption)
    
  def modify(q: Quarto, vs: ViewState): Unit =
    quarto = q
    viewState = vs

  def update(): Unit = {
    if !viewState.isFinished then
      operation.next(quarto).map {
        case QuartoResult.Processing(q, _) =>
          modify(q, ViewState.progress(q))
          draw(viewState)(using ctx)
        case QuartoResult.Finished(winner, q, pos) =>
          modify(q, viewState.finish(winner, q.pieces, q.linesFromPos(pos).find(_.isQuarto)))
          draw(viewState)(using ctx)
          infoArea.innerText = s"${winner}の勝利"
          resetBtn.setAttribute("style", "display: inline")
        case _ =>
          resetBtn.setAttribute("style", "display: inline")
      }
    else ()
  }
  
  def draw(viewState: ViewState)(using s: State): Unit =
    drawBoard(viewState)
    drawHands(viewState)
  
  def drawBoard(viewState: ViewState)(using s: State): Unit = {
    import RenderingOperation._
    fillRect(0, 0, 300, 400, "olive").run
    (for {
      x <- 0 until 4
      y <- 0 until 4
      pos = Pos.fromIndex(x + y * 4)
      piece = viewState.pieces(pos)
    } yield 
      strokeStyle("black") >> lineWidth(2) >>
      BoardRenderer.drawTile(pos, piece, viewState.quartoLine, viewState.reaches)
      ).reduce(_ >> _)
  }.run(using s.copy(offset = Offset(50, 100)))
  
  def drawHands(viewState: ViewState)(using s: State): Unit = {
    import RenderingOperation._
    BoardRenderer.drawHand(viewState.black).run(using s.copy(offset = Offset(50, 300)))
    BoardRenderer.drawHand(viewState.white).run(using s.copy(offset = Offset(50, 0)))
  }
  
  def handleEvent(event: Event): Unit = ???
}

object BoardRenderer:
  import RenderingOperation._
  type Ope = RenderingOperation.Operation
  val pieceSize = 30
  val pieceOutlineSize = 36
  val pieceHoleSize = 16
  val tileSize = 40
  val unitSize = 50
  
  def drawPiece(pos: Pos, piece: Piece): Ope =
    drawShape(pos, piece) >> drawHole(pos, piece) >> drawHeight(pos, piece)
  def drawPiece(x: Double, y: Double, piece: Piece): Ope = ???
  
  def drawBackground(pos: Pos, piece: Piece, quartoLine: Option[Line], reaches: Seq[Line]): Ope =
    fillStyle(
      if quartoLine.exists(_.contains(piece)) then "red"
      else if !piece.isEmpty && reaches.exists(_.contains(piece)) then "purple"
      else "lightgrey"
    ) >> drawSquare(pos, tileSize) >> fill()
    
  def drawShape(pos: Pos, piece: Piece): Ope =
    if piece.isSquare then
      withFillColor(piece.color) >> drawSquare(pos, 30.0) >> fill()
    else if piece.isCircle then
      withFillColor(piece.color) >> drawCircle(pos, 30.0) >> fill()
    else nop()
    
  def drawHole(pos: Pos, piece: Piece): Ope =
    if piece.hasHole then
      fillStyle("grey") >> drawCircle(pos, 16) >> fill()
    else nop()
    
  def drawHeight(pos: Pos, piece: Piece): Ope = 
    (piece.isTall, piece.isSquare, piece.isCircle) match
      case (true, true, _) => drawSquare(pos, 36) >> stroke()
      case (true, false, true) => drawCircle(pos, 36) >> stroke()
      case _ => nop()

  def drawTile(pos: Pos, piece: Piece, quartoLine: Option[Line], reaches: Seq[Line]): Ope =
    drawBackground(pos, piece, quartoLine, reaches) >> drawPiece(pos, piece)
    
  def drawHand(hand: Set[Piece]): Ope =
    hand.zipWithIndex.map { 
      case (piece, index) => drawPiece(Pos.fromIndex(index), piece)
    }.reduce(_ >> _)

  def drawSquare(pos: Pos, size: Double): Ope =
    drawSquare(pos.x * unitSize + (tileSize - size) / 2, pos.y * unitSize + (tileSize - size) / 2, size)
  def drawSquare(x: Double, y: Double, size: Double): Ope = drawPath(rect(x, y, size, size))
  def drawCircle(pos: Pos, size: Double): Ope =
    drawPath(arc(pos.x * unitSize + 20, pos.y * unitSize + 20, size / 2, 0, Math.PI * 2))  
  def drawPath(o: Ope): Ope = beginPath() >> o >> closePath()
  def withFillColor(color: Color): Ope = fillStyle(color.toStyle)
  extension(color: Color):
    def toStyle: String = color match
      case Color.Black => "black"
      case Color.White => "white"

final case class CanvasRenderingState(ctx: CanvasRenderingContext2D, offset: Offset = Offset.zero)

object RenderingOperation:
  opaque type Operation = State ?=> Unit
  opaque type Point = (Double, Double)
  def stack(o: Operation): Operation = (using s) => s.ctx.save() >> o >> s.ctx.restore()
  def clearRect(x: Double, y: Double, w: Double, h: Double): Operation = 
    (using s) => s.ctx.clearRect(x, y, w, h)
  def strokeStyle(v: String): Operation = (using s) => s.ctx.strokeStyle = v
  def fillStyle(v: String): Operation = (using s) => s.ctx.fillStyle = v
  def lineWidth(v: Double): Operation = (using s) => s.ctx.lineWidth = v
  def beginPath(): Operation = (using s) => s.ctx.beginPath()
  def closePath(): Operation = (using s) => s.ctx.closePath()
  def stroke(): Operation = (using s) => s.ctx.stroke()
  def fill(): Operation = (using s) => s.ctx.fill()
  def rect(x: Double, y: Double, w: Double, h: Double): Operation =
    stack((using s) => s.ctx.translate(s.offset.x, s.offset.y) >> s.ctx.rect(x, y, w, h))
  def arc(x: Double, y: Double, r: Double, start: Double, end: Double): Operation = 
    stack((using s) => s.ctx.translate(s.offset.x, s.offset.y) >> s.ctx.arc(x, y, r, start, end))
  def nop(): Operation = (using s) => ()
  def fillRect(x: Double, y: Double, w: Double, h: Double, style: String): Operation =
    fillStyle(style) >> rect(x, y, w, h) >> fill()
  extension(o: Operation):
    def >>(o2: Operation): Operation = { (using s) => o; o2 }
    def run: State ?=> Unit = o

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

enum Event:
  case Initialized()

final class PlayerDecider(
  giveHandler: (quarto: Quarto, promise: Promise[Option[Piece]]) => Unit,
  putHandler: (quarto: Quarto, piece: Piece, promise: Promise[Option[Pos]]) => Unit)
  extends Decider[Future]:
  def give(quarto: Quarto): Future[Option[Piece]] = {
    val promise: Promise[Option[Piece]] = Promise()
    giveHandler(quarto, promise)
    promise.future
  }
  override def put(quarto: Quarto, piece: Piece): Future[Option[Pos]] = {
    val promise: Promise[Option[Pos]] = Promise()
    putHandler(quarto, piece, promise)
    promise.future
  }
