package com.yuiwai.game.quarto

import com.yuiwai.game.quarto.Quarto.{Coord, Piece, Pos}

import scala.util.Random

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
  given decider as Decider[F]:
    def give(quarto: Quarto[F]): F[Option[Piece]] =
      // TODO 相手が3を作ってしまうコマを渡す
      quarto.second.hand.find(p => !quarto.nextAll(p).exists(_.isFinished))
        .orElse(quarto.second.hand.headOption)
    // TODO 評価関数を分離する
    def put(quarto: Quarto[F], piece: Piece): F[Option[Pos]] =
      (quarto.nextAll(piece)
        .foldLeft(RandomDecider.decider.put(quarto, piece), 0) { (acc, r) => 
          r match             
            case QuartoResult.Finished(_, _, pos) => Some(pos) -> 5
            case QuartoResult.Processing(q, pos) => 
              val score = 
                if quarto.linesFromPos(pos).exists(_.isReach) then 3
                else if quarto.linesFromPos(pos).exists(_.isDouble) then 2
                else if q.linesFromPos(pos).exists(_.isReach) then -1 
                else 1
              if score >= acc._2 then Some(pos) -> score else acc
            case _ => acc
        }) match
          case (r, _) => r

final case class CompositeDecider[F[_]]
(black: Decider[F], white: Decider[F])(using F: FlatMap[F]) extends Decider[F]:
  def give(quarto: Quarto[F]): F[Option[Piece]] = (quarto.turn.map {
    case Color.Black => black.give(quarto)
    case Color.White => white.give(quarto)
  }).getOrElse(F.unit(None))
  def put(quarto: Quarto[F], piece: Piece): F[Option[Pos]] = quarto.turn.map {
    case Color.Black => black.put(quarto, piece)
    case Color.White => white.put(quarto, piece)
  }.getOrElse(F.unit(None))
