package com.yuiwai.game.quarto

import com.yuiwai.game.quarto.Quarto.{Line, Piece, Pos}
import com.yuiwai.game.quarto.RenderingOperation.Region
import org.scalajs.dom
import org.scalajs.dom.raw.{CanvasRenderingContext2D, HTMLButtonElement, HTMLCanvasElement, HTMLElement, MouseEvent}

import scala.concurrent.Promise
import scala.concurrent.duration._

type State = CanvasRenderingState

class QuartoApp[F[_]: Decider : FlatMap](parent: HTMLElement) {
  import QuartoApp.emit
  private val AIDuration = 500.millis
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
    QuartoApp.subscribe(this)
    canvas.setAttribute("width", "300")
    canvas.setAttribute("height", "400")
    infoArea.innerHTML = "&nbsp;"
    resetBtn.innerText = "リセット"
    resetBtn.setAttribute("style", "display: none")
    resetBtn.onclick = _ => reset()
    parent.appendChild(infoArea)
    parent.appendChild(canvas)
    parent.appendChild(resetBtn)
    draw(viewState)(using ctx)
    
    canvas.onclick = (e: MouseEvent) =>
      regions.fire(e.clientX - canvas.offsetLeft, e.clientY - canvas.offsetTop)
    
    emit(Event.Initialized())
  }
  
  def reset(): Unit = {
    resetBtn.setAttribute("style", "display: none")
    infoArea.innerHTML = "&nbsp;"
    quarto = Quarto.init()
    viewState = ViewState.progress(quarto)
    draw(viewState)(using ctx)
    
    emit(Event.Initialized())
  }

  def set(q: Quarto, vs: ViewState): Unit =
    quarto = q
    modifyViewState(_ => vs)
  
  def modifyViewState(f: ViewState => ViewState): Unit =
    viewState = f(viewState)
    draw(viewState)(using ctx)

  def update(): Unit = {
    if !viewState.isFinished then
      operation.next(quarto).map {
        case QuartoResult.Processing(q, _) =>
          set(q, ViewState.progress(q))
          update()
        case QuartoResult.Finished(winner, q, pos) =>
          set(q, viewState.finish(winner, q.pieces, q.linesFromPos(pos).find(_.isQuarto)))
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
        BoardRenderer.drawTile(pos, piece, viewState.quartoLine, viewState.reaches)
          .handle(() => emit(Event.PosSelected(pos)))
      ).reduce(_ >> _)
  }.run(using s.copy(offset = Offset(50, 100)))

  def drawHands(viewState: ViewState)(using s: State): Region = {
    import RenderingOperation._
    BoardRenderer
      .drawHand(viewState.black, viewState.selectedPiece, Some((piece, index) => emit(Event.PieceSelected(piece, index))))
      .run(using s.copy(offset = Offset(50, 300))) +
    BoardRenderer
      .drawHand(viewState.white, viewState.selectedPiece, Some((piece, index) => emit(Event.PieceSelected(piece, index))))
      .run(using s.copy(offset = Offset(50, 0)))
  }
  
  def waitInput(input: ViewState.Input): Unit =
    modifyViewState(_.copy(waiting = Some(input)))
    
  def evalInput(pf: PartialFunction[ViewState.Input, Unit]): Unit =
    viewState.waiting.collect(pf)

  def handleEvent(event: Event): Unit =  event match
    case Event.Initialized() => update()
    case Event.PlayerGiveHandlerCalled(run) =>
      waitInput(ViewState.Input.GivePiece { piece =>
        if quarto.second.hand.contains(piece) then run(Some(piece))
      })
    case Event.PlayerPutHandlerCalled(piece, run) =>
      waitInput(ViewState.Input.PutPiece (piece, { pos =>
        if quarto.board.spaces.contains(pos) then run(Some(pos))
      }))
    case Event.AIGiveHandlerCalled(run) => 
      Timer.delay(() => run(), AIDuration)
    case Event.AIPutHandlerCalled(run) => 
      Timer.delay(() => { run() } , AIDuration)
    case Event.PieceSelected(piece, _) => evalInput {
      case ViewState.Input.GivePiece(run) => run(piece)
    }
    case Event.PosSelected(pos) => evalInput {
      case ViewState.Input.PutPiece(_, run) => run(pos)
    }
}

object QuartoApp:
  private var subscriber: Option[QuartoApp[_]] = None
  def subscribe(app: QuartoApp[_]): Unit = subscriber = Some(app)
  def emit(event: Event): Unit = subscriber.foreach(_.handleEvent(event))

final case class ViewState(
  pieces: Map[Pos, Piece],
  black: Set[Piece],
  white: Set[Piece],
  reaches: Seq[Line],
  quartoLine: Option[Line],
  winner: Option[Color],
  waiting: Option[ViewState.Input]
):
  def isFinished: Boolean = winner.isDefined
  def finish(winner: Color, pieces: Map[Pos, Piece], quartoLine: Option[Line]): ViewState =
    copy(winner = Some(winner), pieces = pieces, quartoLine = quartoLine)
  def selectedPiece: Option[Piece] = waiting.collect {
    case ViewState.Input.PutPiece(piece, _) => piece
  }

object ViewState:
  enum Input:
    case GivePiece(run: Piece => Unit)
    case PutPiece(piece: Piece, run: Pos => Unit)
  def progress(quarto: Quarto): ViewState =
    ViewState(quarto.pieces, quarto.black, quarto.white, quarto.reaches, None, None, None)

enum Event:
  case Initialized()
  case PlayerGiveHandlerCalled(run: Option[Piece] => Unit)
  case PlayerPutHandlerCalled(piece: Piece, run: Option[Pos] => Unit)
  case AIGiveHandlerCalled(run: () => Unit)
  case AIPutHandlerCalled(run: () => Unit)
  case PieceSelected(piece: Piece, index: Int)
  case PosSelected(pos: Pos)
