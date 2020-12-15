package com.yuiwai.game.quarto

import org.scalajs.dom.raw.CanvasRenderingContext2D

final case class CanvasRenderingState(ctx: CanvasRenderingContext2D, offset: Offset = Offset.zero)

object RenderingOperation:
  opaque type Operation = State ?=> Unit
  opaque type Point = (Double, Double)
  def stack(o: Operation): Operation = (using s) => s.ctx.save() >> o >> s.ctx.restore()
  def clearRect(x: Double, y: Double, w: Double, h: Double): Operation =
    (using s) => s.ctx.clearRect(x, y, w, h)
  def strokeStyle(v: String): Operation = (using s) => s.ctx.strokeStyle = v
  def fillStyle(v: String): Operation = (using s) => s.ctx.fillStyle = v
  def lineWidth(v: Double): Operation = (using s) => s.ctx.lineWidth = v
  def beginPath(): Operation = (using s) => s.ctx.beginPath()
  def closePath(): Operation = (using s) => s.ctx.closePath()
  def stroke(): Operation = (using s) => s.ctx.stroke()
  def fill(): Operation = (using s) => s.ctx.fill()
  def rect(x: Double, y: Double, w: Double, h: Double): Operation =
    stack((using s) => s.ctx.translate(s.offset.x, s.offset.y) >> s.ctx.rect(x, y, w, h))
  def arc(x: Double, y: Double, r: Double, start: Double, end: Double): Operation =
    stack((using s) => s.ctx.translate(s.offset.x, s.offset.y) >> s.ctx.arc(x, y, r, start, end))
  def nop(): Operation = (using s) => ()
  def fillRect(x: Double, y: Double, w: Double, h: Double, style: String): Operation =
    fillStyle(style) >> rect(x, y, w, h) >> fill()
  extension(o: Operation):
    def >>(o2: Operation): Operation = { (using s) => o; o2 }
    def run: State ?=> Unit = o
