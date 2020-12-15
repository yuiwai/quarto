package com.yuiwai.game.quarto

import com.yuiwai.game.quarto.Quarto.{Piece, Pos}
import com.yuiwai.game.quarto.RenderingOperation.Region
import org.scalajs.dom
import org.scalajs.dom.raw.{CanvasRenderingContext2D, HTMLButtonElement, HTMLCanvasElement, HTMLElement, MouseEvent}

import scala.concurrent.Promise

class QuartoApp[F[_]: Decider : FlatMap](parent: HTMLElement) {
  private var quarto = Quarto.init()
  private var viewState: ViewState = ViewState.progress(quarto)
  private var regions: Region = Region.Empty
  lazy val canvas = dom.document.createElement("canvas")
    .asInstanceOf[HTMLCanvasElement]
  lazy val ctx =
    CanvasRenderingState(canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D])
  lazy val infoArea = dom.document.createElement("div")
  lazy val resetBtn = dom.document.createElement("button").asInstanceOf[HTMLButtonElement]
  val operation = QuartoOperation[F]
  
  def init(): Unit = {
    canvas.setAttribute("width", "300")
    canvas.setAttribute("height", "400")
    canvas.onclick = _ => update()
    infoArea.innerHTML = "&nbsp;"
    resetBtn.innerText = "リセット"
    resetBtn.setAttribute("style", "display: none")
    resetBtn.onclick = _ => reset()
    parent.appendChild(infoArea)
    parent.appendChild(canvas)
    parent.appendChild(resetBtn)
    draw(viewState)(using ctx)
    
    // Regionのテスト
    canvas.onmousemove = (e: MouseEvent) =>
      regions.fire(e.clientX - canvas.offsetLeft, e.clientY - canvas.offsetTop)
    
    emmit(Event.Initialized())
  }
  
  def reset(): Unit = {
    resetBtn.setAttribute("style", "display: none")
    infoArea.innerHTML = "&nbsp;"
    quarto = Quarto.init()
    viewState = ViewState.progress(quarto)
    draw(viewState)(using ctx)
  }

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
    regions = drawBoard(viewState) + drawHands(viewState)

  def drawBoard(viewState: ViewState)(using s: State): Region = {
    import RenderingOperation._
    fillRect(0, 0, 300, 400, "olive").run
    (for {
      x <- 0 until 4
      y <- 0 until 4
      pos = Pos.fromIndex(x + y * 4)
      piece = viewState.pieces(pos)
    } yield
      strokeStyle("black") >> lineWidth(2) >>
        BoardRenderer.drawTile(pos, piece, viewState.quartoLine, viewState.reaches).handle(() => println(pos))
      ).reduce(_ >> _)
  }.run(using s.copy(offset = Offset(50, 100)))

  def drawHands(viewState: ViewState)(using s: State): Region = {
    import RenderingOperation._
    BoardRenderer.drawHand(viewState.black).run(using s.copy(offset = Offset(50, 300))) +
    BoardRenderer.drawHand(viewState.white).run(using s.copy(offset = Offset(50, 0)))
  }

  def emmit(event: Event): Unit = ???
  def handleEvent(event: Event): Unit = ???
}

enum Event:
  case Initialized()
