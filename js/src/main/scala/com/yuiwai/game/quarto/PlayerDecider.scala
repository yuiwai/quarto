package com.yuiwai.game.quarto

import com.yuiwai.game.quarto.Quarto.{Piece, Pos}

import scala.concurrent.{Future, Promise}

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
