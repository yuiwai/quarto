package com.yuiwai.game.quarto

import com.yuiwai.game.quarto.Quarto.{Line, Piece, Pos}

object BoardRenderer:
  import RenderingOperation._
  type Ope = RenderingOperation.Operation
  val pieceSize = 30
  val pieceOutlineSize = 36
  val pieceHoleSize = 16
  val tileSize = 40
  val unitSize = 50

  def drawPiece(pos: Pos, piece: Piece): Ope =
    drawShape(pos, piece) >> drawHole(pos, piece) >> drawHeight(pos, piece)
  def drawPiece(x: Double, y: Double, piece: Piece): Ope = ???

  def drawBackground(pos: Pos, piece: Piece, quartoLine: Option[Line], reaches: Seq[Line]): Ope =
    fillStyle(
      if quartoLine.exists(_.contains(piece)) then "red"
      else if !piece.isEmpty && reaches.exists(_.contains(piece)) then "purple"
      else "lightgrey"
    ) >> drawSquare(pos, tileSize) >> fill()

  def drawShape(pos: Pos, piece: Piece): Ope =
    if piece.isSquare then
      withFillColor(piece.color) >> drawSquare(pos, 30.0) >> fill()
    else if piece.isCircle then
      withFillColor(piece.color) >> drawCircle(pos, 30.0) >> fill()
    else nop()

  def drawHole(pos: Pos, piece: Piece): Ope =
    if piece.hasHole then
      fillStyle("grey") >> drawCircle(pos, 16) >> fill()
    else nop()

  def drawHeight(pos: Pos, piece: Piece): Ope =
    (piece.isTall, piece.isSquare, piece.isCircle) match
      case (true, true, _) => drawSquare(pos, 36) >> stroke()
      case (true, false, true) => drawCircle(pos, 36) >> stroke()
      case _ => nop()

  def drawTile(pos: Pos, piece: Piece, quartoLine: Option[Line], reaches: Seq[Line]): Ope =
    drawBackground(pos, piece, quartoLine, reaches) >> drawPiece(pos, piece)

  def drawHand(hand: Set[Piece]): Ope =
    hand.zipWithIndex.map {
      case (piece, index) => drawPiece(Pos.fromIndex(index), piece)
    }.reduce(_ >> _)

  def drawSquare(pos: Pos, size: Double): Ope =
    drawSquare(pos.x * unitSize + (tileSize - size) / 2, pos.y * unitSize + (tileSize - size) / 2, size)
  def drawSquare(x: Double, y: Double, size: Double): Ope = drawPath(rect(x, y, size, size))
  def drawCircle(pos: Pos, size: Double): Ope =
    drawPath(arc(pos.x * unitSize + 20, pos.y * unitSize + 20, size / 2, 0, Math.PI * 2))
  def drawPath(o: Ope): Ope = beginPath() >> o >> closePath()
  def withFillColor(color: Color): Ope = fillStyle(color.toStyle)
  extension(color: Color):
    def toStyle: String = color match
      case Color.Black => "black"
      case Color.White => "white"
