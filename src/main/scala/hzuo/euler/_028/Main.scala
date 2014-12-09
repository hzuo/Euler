package hzuo.euler._028

object Main extends App {

  def corners(offset: Int, ring: Int): IndexedSeq[Int] = {
    val sideLen = (ring + 1) * 2
    Stream.from(offset).grouped(sideLen).map(_.head).drop(1).take(4).toIndexedSeq
  }

  val (answer, _) = {
    (0 until 500).foldLeft((1, 1)) {
      case ((sum, offset), ring) =>
        val cs = corners(offset, ring)
        (sum + cs.sum, cs.last)
    }
  }
  println(answer)

}