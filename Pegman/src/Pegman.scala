import java.io.{FileOutputStream, FileInputStream}

import scala.io.StdIn

/**
 * Created by YoungJoo on 2015-07-16.
 */
object Pegman extends App {
  val path = "D:\\dev\\intellySpace\\scala\\Pegman\\src\\"
  Console.setIn(new FileInputStream(path + "example.in"))


  Console.setIn(new FileInputStream(path +"A-small-practice.in"))
  Console.setOut(new FileOutputStream(path +"A-small-practice.out"))

  Console.setIn(new FileInputStream(path +"A-large-practice.in"))
  Console.setOut(new FileOutputStream(path +"A-large-practice.out"))

  class CoodWithDirection(val x: Int, val y: Int, val direction: Char) {
    private val otherDirections = {
      Array('<', '>', 'v', '^').filter(_ != this.direction)
    }
    var coodWithDirections: IndexedSeq[CoodWithDirection] = null

    def hasPointOnDirection: Boolean = {
      if (direction == '.') return true
      if (direction == '>' && onRight) return true
      if (direction == '<' && onLeft) return true
      if (direction == '^' && onUp) return true
      if (direction == 'v' && onDown) return true
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
      coodWithDirections.filter(element => ((element.y == this.y) && (element.x < this.x)) && element.direction != '.').length > 0
    }

    private def onRight: Boolean = {
      coodWithDirections.filter(element => ((element.y == this.y) && (element.x > this.x)) && element.direction != '.').length > 0
    }

    private def onUp: Boolean = {
      //      println(coodWithDirections.filter(element => ((element.x == this.x) && (element.y < this.y)) && element.direction != '.').length)
      //
      //      println(coodWithDirections.filter(element => element.direction != ".").length)
      //
      //      coodWithDirections.foreach(e => if(e.direction != '.')println("not equal"))
      coodWithDirections.filter(element => ((element.x == this.x) && (element.y < this.y)) && element.direction != '.').length > 0
    }

    private def onDown: Boolean = {
      coodWithDirections.filter(element => ((element.x == this.x) && (element.y > this.y)) && element.direction != '.').length > 0
    }
  }

  /**
   * solve
   * @param box
   * @return
   */
  def solve(box: IndexedSeq[Array[Char]]): String = {
    var result = 0
    var index1 = 0
    var index2 = 0
    val coodWithDirections = {
      for {
        index1 <- 0 until box.length
        line = box(index1)
        index2 <- 0 until line.length
      } yield {
        new CoodWithDirection(index2 + 1, index1 + 1, line(index2))
      }
    }

    try {
      coodWithDirections.foreach(coodWithDirection => {
        coodWithDirection.coodWithDirections = coodWithDirections
        //        println(coodWithDirection.hasPointOnDirection)
        //        println(coodWithDirection.hasPointOnOtherDirection)
        if (coodWithDirection.hasPointOnDirection) {
          //doNoting
        } else if (coodWithDirection.hasPointOnOtherDirection) {
          result += 1
        } else {
          throw new RuntimeException("IMPOSSIBLE")
        }
      })
    } catch {
      case ex: RuntimeException => return "IMPOSSIBLE"
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
