package com.yuiwai.game.quarto

import com.yuiwai.game.quarto.Quarto.{Piece, Pos}

import scala.concurrent.{Future, Promise}

final class PlayerDecider(emit: Event => Unit) extends Decider[Future]:
  def give(quarto: Quarto): Future[Option[Piece]] =
    val promise: Promise[Option[Piece]] = Promise()
    emit(Event.PlayerGiveHandlerCalled(promise.success(_)))
    promise.future
  override def put(quarto: Quarto, piece: Piece): Future[Option[Pos]] =
    val promise: Promise[Option[Pos]] = Promise()
    emit(Event.PlayerPutHandlerCalled(promise.success(_)))
    promise.future

final class EventHandledFutureWrappedDecider(decider: Decider[[T] =>> T], emit: Event => Unit) 
  extends Decider[Future]:
  def give(quarto: Quarto): Future[Option[Piece]] =
    val promise: Promise[Option[Piece]] = Promise()
    emit(Event.AIGiveHandlerCalled(() => promise.success(decider.give(quarto))))
    promise.future
  def put(quarto: Quarto, piece: Piece): Future[Option[Pos]] =
    val promise: Promise[Option[Pos]] = Promise()
    emit(Event.AIPutHandlerCalled(() => promise.success(decider.put(quarto, piece))))
    promise.future
