package com.yuiwai.game.quarto

import org.scalajs.dom.raw.CanvasRenderingContext2D

final case class CanvasRenderingState(ctx: CanvasRenderingContext2D, offset: Offset = Offset.zero)

object RenderingOperation:
  enum Region:
    case Empty
    case Rect(p: Point, width: Double, height: Double, handler: Option[() => Unit])
    case Regions(l: Region, r: Region)
    case CompositeRegion(l: Region, r: Region)
    def +(other: Region): Region = (this, other) match
      case (Empty, r) => r
      case (l, Empty) => l
      case (l, r) => Regions(l, r)
    def *(other: Region): Region = (this, other) match
      case (Empty, r) => r
      case (l, Empty) => l
      case (l, r) => CompositeRegion(l, r)
    def setHandler(f: () => Unit): Region = this match
      case r: Rect => r.copy(handler = Some(f))
      case CompositeRegion(l, r) => CompositeRegion(l.setHandler(f), r.setHandler(f))
      case _ => this
    def fire(x: Double, y: Double): Boolean = this match
      case Regions(l, r) => l.fire(x, y) || r.fire(x, y)
      case CompositeRegion(l ,r) => l.fire(x, y) || r.fire(x, y)
      case Rect(p, w, h, Some(handler)) => 
        val (px, py) = (p._1, p._2)
        if px <= x && x <= px + w && py <= y && y <= py + h then
          handler()
          true
        else false
      case _ => false
  opaque type Operation = State ?=> Region
  opaque type Point = (Double, Double)
  def stack(o: Operation): Operation = (using s) => { s.ctx.save(); o; s.ctx.restore(); Region.Empty }
  def clearRect(x: Double, y: Double, w: Double, h: Double): Operation =
    (using s) => { s.ctx.clearRect(x, y, w, h); Region.Empty }
  def strokeStyle(v: String): Operation = (using s) => { s.ctx.strokeStyle = v; Region.Empty }
  def fillStyle(v: String): Operation = (using s) => { s.ctx.fillStyle = v; Region.Empty }
  def lineWidth(v: Double): Operation = (using s) => { s.ctx.lineWidth = v; Region.Empty }
  def beginPath(): Operation = (using s) => { s.ctx.beginPath(); Region.Empty }
  def closePath(): Operation = (using s) => { s.ctx.closePath(); Region.Empty }
  def stroke(): Operation = (using s) => { s.ctx.stroke(); Region.Empty }
  def fill(): Operation = (using s) => { s.ctx.fill(); Region.Empty }
  def rect(x: Double, y: Double, w: Double, h: Double): Operation = (using s) =>
    stack((using s) => { s.ctx.translate(s.offset.x, s.offset.y); s.ctx.rect(x, y, w, h); Region.Empty })
    Region.Rect((x + s.offset.x, y + s.offset.y), w, h, None)
  def arc(x: Double, y: Double, r: Double, start: Double, end: Double): Operation = (using s) =>
    stack((using s) => { s.ctx.translate(s.offset.x, s.offset.y); s.ctx.arc(x, y, r, start, end); Region.Empty }) 
    Region.Rect((x - r + s.offset.x, y - r + s.offset.y), r * 2, r * 2, None)
  def nop(): Operation = (using s) => Region.Empty
  def fillRect(x: Double, y: Double, w: Double, h: Double, style: String): Operation =
    fillStyle(style) >> rect(x, y, w, h) >> fill()
  extension(o: Operation):
    def *>(o2: Operation): Operation = { (using s) => o * o2 }
    def >>(o2: Operation): Operation = { (using s) => o + o2 }
    def run: State ?=> Region = o
    def handle(f: () => Unit): Operation = (using s) =>
      o(using s).setHandler(f)
