package com.yuiwai.game.quarto

import scala.util.Random
import Quarto._

import scala.concurrent.Future

trait Decider[F[_]]:
  def give(quarto: Quarto[F]): F[Option[Piece]]
  def put(quarto: Quarto[F], piece: Piece): F[Option[Pos]]

object RandomDecider:
  type F = [T] =>> T
  given decider as Decider[F]:
    def give(quarto: Quarto[F]): Option[Piece] = {
      quarto.second.hand.lift(new Random().between(0, quarto.second.hand.size))
    }
    def put(quarto: Quarto[F], piece: Piece): Option[Pos] =
      quarto.board.spaces.lift(new Random().between(0, quarto.board.spaces.size))

object SimpleDecider:
  type F = [T] =>> T
  type Score = -1 | 0 | 1 | 2 | 3 | 4 | 5
  given decider as Decider[F]:
    // TODO 評価関数を採用する
    def give(quarto: Quarto[F]): F[Option[Piece]] =
      // TODO 相手が3を作ってしまうコマを渡す
      quarto.second.hand.find(p => !quarto.nextAll(p).exists(_.isFinished))
        .orElse(quarto.second.hand.headOption)
    def put(quarto: Quarto[F], piece: Piece): F[Option[Pos]] =
      (quarto.nextAll(piece)
        .foldLeft(RandomDecider.decider.put(quarto, piece), 0) { (acc, r) =>
          r.lastPutPos.map { pos =>
            val score = evalPut(quarto, r, pos)
            if score > acc._2 then Some(pos) -> score else acc
          }.getOrElse(acc)
        }) match
          case (r, _) => r
    def evalPut(before: Quarto[F], result: QuartoResult[F], pos: Pos): Score =
      result match
        case QuartoResult.Finished(_, _, _) => 5
        case QuartoResult.Processing(after, pos) =>
          if before.linesFromPos(pos).exists(_.isReach) then 3
          else if after.linesFromPos(pos).exists(_.isDouble) then 2
          else if after.linesFromPos(pos).exists(_.isReach) then -1
          else 0
        case _ => 0

object RecursiveDecider:
  type Score = Double
  type F = [T] =>> T
  given decider as Decider[F]:
    def give(quarto: Quarto[F]): Option[Piece] = quarto.second.hand match
      case hand if hand.nonEmpty =>
        Some(hand.maxBy(giveImpl(quarto, _, 3)))
      case _ => None
    def put(quarto: Quarto[F], piece: Piece): Option[Pos] = quarto.nextAll(piece) match
      case nextAll if nextAll.nonEmpty =>
        nextAll.maxBy {
          case QuartoResult.Finished(_, _, _) => 1.0
          case QuartoResult.Processing(_, pos) => putImpl(quarto, piece, pos, 3)
          case _ => 0.5
        }.lastPutPos
      case _ => None
    private def giveImpl(quarto: Quarto[F], piece: Piece, depth: Int): Score = 1.0 // TODO impl
    private def putImpl(quarto: Quarto[F], piece: Piece, pos: Pos, depth: Int): Score = 1.0 // TODO impl

final case class CompositeDecider[F[_]]
(black: Decider[F], white: Decider[F])(using F: FlatMap[F]) extends Decider[F]:
  def give(quarto: Quarto[F]): F[Option[Piece]] = (quarto.turn.map {
    case Color.Black => white.give(quarto)
    case Color.White => black.give(quarto)
  }).getOrElse(F.unit(None))
  def put(quarto: Quarto[F], piece: Piece): F[Option[Pos]] = quarto.turn.map {
    case Color.Black => black.put(quarto, piece)
    case Color.White => white.put(quarto, piece)
  }.getOrElse(F.unit(None))

final class FutureWrappedDecider(decider: Decider[[T] =>> T]) extends Decider[Future]:
  def give(quarto: Quarto[Future]): Future[Option[Piece]] = 
    Future.successful(decider.give(Quarto(quarto.board, quarto.first, quarto.second)(using decider)))
  def put(quarto: Quarto[Future], piece: Piece): Future[Option[Pos]] =
    Future.successful(decider.put(Quarto(quarto.board, quarto.first, quarto.second)(using decider), piece))