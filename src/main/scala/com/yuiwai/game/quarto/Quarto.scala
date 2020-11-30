package com.yuiwai.game.quarto

import com.yuiwai.game.quarto.Quarto.{Board, Coord, Piece, Player, Pos}

import scala.util.Random

final case class Quarto[F[_]: FlatMap](
  board: Board,
  first: Player,
  second: Player)(using decider: Decider[F]):
  def next: F[QuartoResult[F]] =
    for {
      piece <- decider.give(this)
      pos <- decider.put(this, piece)
    } yield put(pos, piece)
  private def put(pos: Pos, piece: Piece): QuartoResult[F] =
    second.release(piece).map { p =>
      board.put(pos, piece) match
        case PutResult.Success(b) =>
          if b.lines.exists(_.isQuarto) then
            QuartoResult.Finished(piece.color, copy(b, p, first))
          else QuartoResult.Processing(copy(b, p, first))
        case PutResult.AlreadyExists(_) =>
          QuartoResult.Error()
    }.getOrElse(QuartoResult.Error())

enum QuartoResult[F[_]]:
  case Error() // TODO エラーを分類
  case Processing(quarto: Quarto[F])
  case Finished(winner: Color, quarto: Quarto[F])

trait Decider[F[_]]:
  def give(quarto: Quarto[F]): F[Piece]
  def put(quarto: Quarto[F], piece: Piece): F[Pos]

object Quarto:
  type Coord = 0 | 1 | 2 | 3
  opaque type Piece = Int
  opaque type Board = Seq[Piece]
  opaque type Player = Set[Piece]
  opaque type Pos = (Coord, Coord)
  opaque type Line = Seq[Piece]
  private val EMPTY = 0
  private val HAS_HOLE = 1
  private val IS_TALL = 2
  private val IS_SQUARE = 4
  private val IS_BLACK = 8
  private val IS_EXISTS = 16
  private val LINE_LENGTH = 4
  private val PIECE_PATTERNS = 16
  val coords: Seq[Coord] = Seq(0, 1, 2, 3)

  def init[F[_]: Decider : FlatMap](): Quarto[F] =
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
      IS_EXISTS.withFace(face).withHeight(height).withShape(shape).withColor(color)
    def all(color: Color): Set[Piece] =
      Set.tabulate(PIECE_PATTERNS / 2)(i => i.withColor(color) | IS_EXISTS)

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
    def isShort: Boolean = !isTall
    def isSquare: Boolean = (piece & IS_SQUARE) != 0
    def isCircle: Boolean = !isSquare
    def isBlack: Boolean = (piece & IS_BLACK) != 0
    def isWhite: Boolean = !isBlack
    def exists: Boolean = (piece & IS_EXISTS) != 0
    def isEmpty: Boolean = !exists
    def withFace(face: Face): Piece = face match
      case Face.Hole => piece | HAS_HOLE
      case Face.Flat => piece & ~HAS_HOLE
    def withHeight(height: Height): Piece = height match
      case Height.Tall => piece | IS_TALL
      case Height.Short => piece & ~IS_TALL
    def withShape(shape: Shape): Piece = shape match
      case Shape.Square => piece | IS_SQUARE
      case Shape.Circle => piece & ~IS_SQUARE
    def withColor(color: Color): Piece = color match
      case Color.Black => piece | IS_BLACK
      case Color.White => piece & ~IS_BLACK

  extension(pos: Pos):
    def x: Coord = pos._1
    def y: Coord = pos._2
    def toIndex: Int = x + y * LINE_LENGTH

  extension(line: Line):
    def isQuarto: Boolean = line.reduce(_ & _) != 0

  extension(board: Board):
    def spaces: Seq[Pos] = board.zipWithIndex.filter(_._1.isEmpty).map(t => Pos.fromIndex(t._2))
    def lines: Seq[Line] = hLines ++ vLines
    def hLines: Seq[Line] = coords.map(hLine(_))
    def vLines: Seq[Line] = coords.map(vLine(_))
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

object RandomDecider:
  type F = [T] =>> T
  given Decider[F]:
    def give(quarto: Quarto[F]): Piece =
      quarto.second.hand(new Random().between(0, quarto.second.hand.size))
    def put(quarto: Quarto[F], piece: Piece): Pos =
      quarto.board.spaces(new Random().between(0, quarto.board.spaces.size))

trait FlatMap[F[_]]:
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

object FlatMap:
  given FlatMap[[T] =>> T]:
    def map[A, B](fa: A)(f: A => B): B = f(fa)
    def flatMap[A, B](fa: A)(f: A => B): B = f(fa)

extension[F[_]: FlatMap, A, B](fa: F[A]):
  def map(f: A => B): F[B] = summon[FlatMap[F]].map(fa)(f)
  def flatMap(f: A => F[B]): F[B] = summon[FlatMap[F]].flatMap(fa)(f)
