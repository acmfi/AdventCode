import scala.annotation.tailrec
import scala.collection.immutable.Stream.Empty
import scala.io.StdIn

object Day01 {
  def main(args: Array[String]): Unit = {
    def input(): Stream[String] =
      StdIn.readLine() match {
        case null => Empty //EOF
        case x => x #:: input()
      }
    def cycle(in: Stream[String]): Stream[String] = in #::: cycle(in)

    @tailrec
    def firstDup[A](lst: Stream[A], acc: Set[A] = Set[A]()): A = lst match {
      case hd #:: tl => if (acc.contains(hd)) hd else firstDup(tl, acc + hd)
      case _ => throw new Exception("No duplicate found")
    }

    println(firstDup(cycle(input()).scanLeft(0)(_ + _.toInt)))
  }
}
