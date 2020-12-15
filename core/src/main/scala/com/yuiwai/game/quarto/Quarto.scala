package com.yuiwai.game.quarto

import Quarto._

import scala.concurrent.{ExecutionContext, Future}

final case class Quarto(
  board: Board,
  first: Player,
  second: Player):
  def turn: Option[Color] = second.color
  def black: Set[Piece] = (first.hand ++ second.hand).filter(_.isBlack).toSet
  def white: Set[Piece] = (first.hand ++ second.hand).filter(_.isWhite).toSet
  def linesFromPos(pos: Pos): Seq[Line] = Seq(board.hLines(pos.y), board.vLines(pos.x))
  def nextAll(piece: Piece): Set[QuartoResult] =
    board.spaces.toSet.map(put(_, piece))  
  def put(pos: Pos, piece: Piece): QuartoResult =
    second.release(piece).map { p =>
      board.put(pos, piece) match
        case PutResult.Success(b) =>
          if b.lines.exists(_.isQuarto) then
            QuartoResult.Finished(piece.color, copy(b, p, first), pos)
          else QuartoResult.Processing(copy(b, p, first), pos)
        case PutResult.AlreadyExists(_) =>
          QuartoResult.Error()
    }.getOrElse(QuartoResult.Error())
  def pieces: Map[Pos, Piece] = (0 until 16).map(Pos.fromIndex(_)).map(p => p -> board(p)).toMap
  def reaches: Seq[Line] = board.reaches

enum QuartoResult:
  case Error() // TODO エラーを分類
  case Processing(quarto: Quarto, pos: Pos)
  case Draw(quarto: Quarto)
  case Finished(winner: Color, quarto: Quarto, pos: Pos)
  def isFinished: Boolean = this match
    case Finished(_, _, _) => true
    case _ => false
  def lastPutPos: Option[Pos] = this match
    case Processing(_, pos) => Some(pos)
    case Finished(_, _, pos) => Some(pos)
    case _ => None

class QuartoOperation[F[_]](using decider: Decider[F], F: FlatMap[F]):
  def next(quarto: Quarto): F[QuartoResult] = 
    for {
      pieceOpt <- decider.give(quarto)
      posOpt <- pieceOpt.map(decider.put(quarto, _)).getOrElse(F.unit(None))
    } yield (pieceOpt, posOpt) match
      case (Some(piece), Some(pos)) => quarto.put(pos, piece)
      case _ => QuartoResult.Draw(quarto)

object Quarto:
  type Coord = 0 | 1 | 2 | 3
  opaque type Piece = Int
  opaque type Board = Seq[Piece]
  opaque type Player = Set[Piece]
  opaque type Pos = (Coord, Coord)
  opaque type Line = Seq[Piece]
  private val EMPTY = 0
  private val HAS_HOLE = 1
  private val IS_FLAT = 2
  private val IS_SHORT = 4
  private val IS_TALL = 8
  private val IS_SQUARE = 16
  private val IS_CIRCLE = 32
  private val IS_BLACK = 64
  private val IS_WHITE = 128
  private val LINE_LENGTH = 4
  private val PIECE_PATTERNS = 16
  val coords: Seq[Coord] = Seq(0, 1, 2, 3)

  def init(): Quarto =
    apply(
      Board.empty,
      Player.init(Color.Black),
      Player.init(Color.White))

  object Board:
    def empty: Board = Seq.fill(LINE_LENGTH * LINE_LENGTH)(EMPTY)

  object Player:
    def init(color: Color): Player = Piece.all(color)

  object Piece:
    def empty: Piece = EMPTY
    def apply(face: Face, height: Height, shape: Shape, color: Color): Piece =
      EMPTY.withFace(face).withHeight(height).withShape(shape).withColor(color)
    def all(color: Color): Set[Piece] = 
      for {
        face <- Set(HAS_HOLE, IS_FLAT)
        height <- Set(IS_TALL, IS_SHORT)
        shape <- Set(IS_SQUARE, IS_CIRCLE)
      } yield (face | height | shape).withColor(color) 

  object Pos:
    def apply(x: Coord, y: Coord): Pos = (x, y)
    def fromIndex(index: Int): Pos =
      apply((index % LINE_LENGTH).asInstanceOf[Coord], (index / LINE_LENGTH).asInstanceOf[Coord])

  object Line:
    def empty: Line = Seq.fill(LINE_LENGTH)(EMPTY)
    def apply(piece1: Piece, piece2: Piece, piece3: Piece, piece4: Piece): Line =
      Seq(piece1, piece2, piece3, piece4)

  extension(piece: Piece):
    def color: Color = if isBlack then Color.Black else Color.White
    def hasHole: Boolean = (piece & HAS_HOLE) != 0
    def isTall: Boolean = (piece & IS_TALL) != 0
    def isShort: Boolean = (piece & IS_SHORT) != 0
    def isSquare: Boolean = (piece & IS_SQUARE) != 0
    def isCircle: Boolean = (piece & IS_CIRCLE) != 0
    def isBlack: Boolean = (piece & IS_BLACK) != 0
    def isWhite: Boolean = (piece & IS_WHITE) != 0
    def exists: Boolean = piece != EMPTY
    def isEmpty: Boolean = piece == EMPTY
    def withFace(face: Face): Piece = face match
      case Face.Hole => piece | HAS_HOLE & ~IS_FLAT
      case Face.Flat => piece | IS_FLAT & ~HAS_HOLE
    def withHeight(height: Height): Piece = height match
      case Height.Tall => piece | IS_TALL & ~IS_SHORT
      case Height.Short => piece | IS_SHORT & ~IS_TALL
    def withShape(shape: Shape): Piece = shape match
      case Shape.Square => piece | IS_SQUARE & ~IS_CIRCLE
      case Shape.Circle => piece | IS_CIRCLE & ~IS_SQUARE
    def withColor(color: Color): Piece = color match
      case Color.Black => piece | IS_BLACK & ~IS_WHITE
      case Color.White => piece | IS_WHITE & ~IS_BLACK

  extension(pos: Pos):
    def x: Coord = pos._1
    def y: Coord = pos._2
    def toIndex: Int = x + y * LINE_LENGTH

  extension(line: Line):
    def isQuarto: Boolean = line.reduce(_ & _) != 0
    def isFilled: Boolean = line.forall(_ != EMPTY)
    def isReach: Boolean = line.count(_ != EMPTY) == 3 && line.filter(_ != EMPTY).reduce(_ & _) != 0
    def isDouble: Boolean = line.count(_ != EMPTY) == 2 && line.filter(_ != EMPTY).reduce(_ & _) != 0
    def contains(piece: Piece): Boolean = line.contains(piece)

  extension(board: Board):
    def spaces: Seq[Pos] = board.zipWithIndex.filter(_._1.isEmpty).map(t => Pos.fromIndex(t._2))
    def lines: Seq[Line] = hLines ++ vLines
    def hLines: Seq[Line] = coords.map(hLine(_))
    def vLines: Seq[Line] = coords.map(vLine(_))
    def reaches: Seq[Line] = lines.filter(_.isReach)
    private def hLine(index: Coord): Line =
      Line(board(Pos(0, index)), board(Pos(1, index)), board(Pos(2, index)), board(Pos(3, index)))
    private def vLine(index: Coord): Line =
      Line(board(Pos(index, 0)), board(Pos(index, 1)), board(Pos(index, 2)), board(Pos(index, 3)))
    def put(pos: Pos, piece: Piece): PutResult =
      if board(pos).isEmpty then
        PutResult.Success(board.updated(pos.toIndex, piece))
      else PutResult.AlreadyExists(pos)
    def apply(pos: Pos): Piece = board(pos.toIndex)

  extension(player: Player):
    def hand: Seq[Piece] = player.toSeq
    def color: Option[Color] = player.headOption.map(_.color)
    def release(piece: Piece): Option[Player] =
      if player(piece) then Some(player - piece) else None

enum Face:
  case Hole, Flat

enum Height:
  case Tall, Short

enum Shape:
  case Square, Circle

enum Color:
  case White, Black

enum PutResult:
  case Success(board: Board)
  case AlreadyExists(pos: Pos)

trait FlatMap[F[_]]:
  def unit[A](a: A): F[A]
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

object FlatMap:
  given FlatMap[[T] =>> T]:
    def unit[A](a: A): A = a
    def map[A, B](fa: A)(f: A => B): B = f(fa)
    def flatMap[A, B](fa: A)(f: A => B): B = f(fa)
  given (using ExecutionContext) as FlatMap[Future]:
    def unit[A](a: A): Future[A] = Future.successful(a)
    def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
    def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa.flatMap(f)

extension[F[_]: FlatMap, A, B](fa: F[A]):
  def map(f: A => B): F[B] = summon[FlatMap[F]].map(fa)(f)
  def flatMap(f: A => F[B]): F[B] = summon[FlatMap[F]].flatMap(fa)(f)
