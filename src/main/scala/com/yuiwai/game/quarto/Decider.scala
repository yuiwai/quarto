package com.yuiwai.game.quarto

import com.yuiwai.game.quarto.Quarto.{Coord, Piece, Pos}

import scala.util.Random

trait Decider[F[_]]:
  def give(quarto: Quarto[F]): F[Piece]
  def put(quarto: Quarto[F], piece: Piece): F[Pos]

object RandomDecider:
  type F = [T] =>> T
  given decider as Decider[F]:
    def give(quarto: Quarto[F]): Piece =
      quarto.second.hand(new Random().between(0, quarto.second.hand.size))
    def put(quarto: Quarto[F], piece: Piece): Pos =
      quarto.board.spaces(new Random().between(0, quarto.board.spaces.size))

object SimpleDecider:
  type F = [T] =>> T
  given decider as Decider[F]:
    def give(quarto: Quarto[F]): F[Piece] =
      // TODO 相手が3を作ってしまうコマを渡す
      quarto.second.hand.find(p => !quarto.nextAll(p).exists(_.isFinished)).getOrElse(quarto.second.hand.head)
    // TODO 相手が次に上がってしまうなら止める
    // TODO 2つ並べる
    // TODO 3つを並べない
    def put(quarto: Quarto[F], piece: Piece): F[Pos] =
      (quarto.nextAll(piece)
        .foldLeft(RandomDecider.decider.put(quarto, piece), 0) { (acc, r) => 
          r match
            case QuartoResult.Finished(_, _, pos) => pos -> 5
            case _ => acc
        })._1

final case class CompositeDecider[F[_]](black: Decider[F], white: Decider[F]) extends Decider[F]:
  def give(quarto: Quarto[F]): F[Piece] = quarto.turn.get match // FIXME Option.getしているので直したい
    case Color.Black => black.give(quarto)
    case Color.White => white.give(quarto)
  def put(quarto: Quarto[F], piece: Piece): F[Pos] = quarto.turn.get match // FIXME Option.getしているので直したい
    case Color.Black => black.put(quarto, piece)
    case Color.White => white.put(quarto, piece)
