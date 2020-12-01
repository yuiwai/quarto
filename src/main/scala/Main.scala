import com.yuiwai.game.quarto.{Color, CompositeDecider, Decider, Face, Height, PutResult, Quarto, QuartoResult, RandomDecider, Shape}
import com.yuiwai.game.quarto.Quarto.{Board, Line, Piece, Pos}

object Main {
  val blackPieces = Piece.all(Color.Black).toSeq
  val whitePieces = Piece.all(Color.White).toSeq
  val allPieces = blackPieces ++ whitePieces
  val pe = Piece.empty
  val p1 = Piece(Face.Hole, Height.Tall, Shape.Square, Color.Black)
  val p2 = Piece(Face.Flat, Height.Short, Shape.Circle, Color.White)

  def main(args: Array[String]): Unit = {
    posSpec
    pieceSpec
    lineSpec
    boardSpec
    quartoSpec
    evaluate(RandomDecider.decider, RandomDecider.decider)
  }

  def posSpec = {
    assert(Pos.fromIndex(0) == Pos(0, 0))
    assert(Pos.fromIndex(1) == Pos(1, 0))
    assert(Pos.fromIndex(4) == Pos(0, 1))
    assert(Pos.fromIndex(5) == Pos(1, 1))
  }

  def pieceSpec = {
    assert(blackPieces.forall(_.isBlack))
    assert(blackPieces.forall(_.exists))
    checkContainAllPieceOnce(blackPieces)

    assert(whitePieces.forall(!_.isBlack))
    assert(whitePieces.forall(_.exists))
    checkContainAllPieceOnce(whitePieces)

    assert(p1.exists && p2.exists)
    assert(p1.hasHole && !p2.hasHole)
    assert(p1.isTall && p2.isShort)
    assert(p1.isSquare && p2.isCircle)
  }

  def lineSpec = {
    assert(!Line.empty.isQuarto)
    assert(Line(blackPieces(0), blackPieces(1), blackPieces(2), blackPieces(3)).isQuarto)
  }

  def boardSpec = {
    val emptyBoard = Board.empty
    assert(emptyBoard.lines.size == 8)
    emptyBoard.put(Pos(1, 1), p1) match
      case PutResult.Success(b1) =>
        assert(b1(Pos(1, 1)) == p1)
        assert(b1.lines.count(_ == Line(pe, p1, pe, pe)) == 2)
        assert(b1.put(Pos(1, 1), p2) == PutResult.AlreadyExists(Pos(1, 1)))
      case _ => ???
  }

  def quartoSpec = {
    import com.yuiwai.game.quarto.RandomDecider.given
    val quarto = Quarto.init()
    assert(quarto.turn == Some(Color.White))
    quarto.next match
      case QuartoResult.Processing(q1) =>
        // 持ち駒が減っている(手番も変わっている)
        assert(q1.first.hand.size == 7)
        // Boardにコマが1つ置かれている
        assert(q1.board.spaces.size == 15)
      case _ => ???
  }

  def checkContainAllPieceOnce(pieces: Iterable[Piece]) =
    assert(pieces.count(p => p.hasHole && p.isTall && p.isSquare) == 1) // 穴, 高, 四角
    assert(pieces.count(p => p.hasHole && p.isTall && p.isCircle) == 1) // 穴, 高, 丸
    assert(pieces.count(p => p.hasHole && p.isShort && p.isSquare) == 1) // 穴, 低, 四角
    assert(pieces.count(p => p.hasHole && p.isShort && p.isCircle) == 1) // 穴, 低, 丸
    assert(pieces.count(p => !p.hasHole && p.isTall && p.isSquare) == 1) // 穴なし, 高, 四角
    assert(pieces.count(p => !p.hasHole && p.isTall && p.isCircle) == 1) // 穴なし, 高, 丸
    assert(pieces.count(p => !p.hasHole && p.isShort && p.isSquare) == 1) // 穴なし, 低, 四角
    assert(pieces.count(p => !p.hasHole && p.isShort && p.isCircle) == 1) // 穴なし, 低, 丸

  // 決着まで実行
  def run(q: Quarto[[T] =>> T]): QuartoResult[[T] =>> T] = {
      q.next match
        case QuartoResult.Processing(q1) => run(q1)
        case r as QuartoResult.Finished(_, _) => r
        case _ => ???
    }
  
  def evaluate(black: Decider[[T] =>> T], white: Decider[[T] =>> T], numOfEval: Int = 1000): Unit = {
    given Decider[[T] =>> T] = CompositeDecider(black, white)
    val quarto = Quarto.init()
    val result = (1 to numOfEval).map(_ => run(quarto)).foldLeft(Map.empty[Color, Int]) {
      case (acc, QuartoResult.Finished(c, _)) => acc.updatedWith(c)(_.map(_ + 1).orElse(Some(0)))
      case (acc, _) => acc
    }
    println(result)
  }
}
