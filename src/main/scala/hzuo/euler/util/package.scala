package hzuo.euler

package object util {

  def time[A](block: => A) = {
    val start = System.currentTimeMillis
    block
    println(System.currentTimeMillis - start)
  }

}