import java.io.{FileInputStream, FileOutputStream}

import scala.io.StdIn

/**
 * Created by YoungJoo on 2015-07-16.
 */
object Pegman extends App {
  val path = "D:\\dev\\intellySpace\\scala\\Pegman\\src\\"
  Console.setIn(new FileInputStream(path + "example.in"))

  //  Console.setIn(new FileInputStream("C-large-practice.in"))
  //  Console.setOut(new FileOutputStream("C-large-practice.out"))

  /**
   * solve
   * @param box
   * @return
   */
  def solve(box: IndexedSeq[Array[Char]]): String = {
    var result = 0

    val coodWithDirections = {
      for {
        line <- box
        text <- line
      } yield {
        new CoodWithDirection(1, 1, text)//TODO : index 제대로 받아야 함.
      }
    }

    class CoodWithDirection(x: Int, y: Int, direction: Char) {
      val otherDirections = {
        Array('<', '>', 'v', '^').filter(_ != this.direction)
      }

      def hasPointOnDirection: Boolean = {
        if (direction == '.') true
        if (direction == '>' && onRight) true
        if (direction == '<' && onLeft) true
        if (direction == '^' && onUp) true
        if (direction == 'v' && onDown) true
        false
      }

      def hasPointOnOtherDirection: Boolean = {
        otherDirections.filter(_ match {
          case '>' => onRight
          case '<' => onLeft
          case '^' => onUp
          case 'v' => onDown
        }).length > 0
      }

      private def onLeft: Boolean = {
        coodWithDirections.filter(element => ((element.y == this.y) && (element.x < this.x))).length > 0//TODO:컴파일 에러 잡자
      }

      private def onRight: Boolean = {
        coodWithDirections.filter(element => ((element.y == this.y) && (element.x > this.x))).length > 0
      }

      private def onUp: Boolean = {
        coodWithDirections.filter(element => ((element.x == this.x) && (element.y < this.y))).length > 0
      }

      private def onDown: Boolean = {
        coodWithDirections.filter(element => ((element.x == this.x) && (element.y > this.y))).length > 0
      }
    }

    try {
      coodWithDirections.foreach(coodWithDirection => {
        if (coodWithDirection.hasPointOnDirection) {
          //doNoting
        } else if (coodWithDirection.hasPointOnOtherDirection) {
          result += 1
        } else {
          throw new RuntimeException("IMPOSSIBLE")
        }
      })
    } catch {
      case ex: RuntimeException => "IMPOSSIBLE"
    }
    result.toString
  }

  val cases = StdIn.readLine().toInt
  (1 to cases) foreach { n =>
    println(s"Case #$n: ${
      solve(for {index <- 0 until StdIn.readLine().split(" ")(0).toInt} yield {
        StdIn.readLine().toArray[Char]
      })
    }")
  }

}
