package com.yuiwai.game.quarto

import com.yuiwai.game.quarto.Quarto.{Piece, Pos}

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

final case class CompositeDecider[F[_]](black: Decider[F], white: Decider[F]) extends Decider[F]:
  def give(quarto: Quarto[F]): F[Piece] = quarto.turn.get match // FIXME Option.getしているので直したい
    case Color.Black => black.give(quarto)
    case Color.White => white.give(quarto)
  def put(quarto: Quarto[F], piece: Piece): F[Pos] = quarto.turn.get match // FIXME Option.getしているので直したい
    case Color.Black => black.put(quarto, piece)
    case Color.White => white.put(quarto, piece)
