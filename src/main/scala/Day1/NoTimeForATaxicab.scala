package Day1

import scala.collection.immutable.HashSet

/**
  * Created by rrodriguez on 01/12/2016.
  */
object NoTimeForATaxicab {

  sealed trait Direction
  case object North extends Direction
  case object East extends Direction
  case object South extends Direction
  case object West extends Direction

  sealed trait Turn
  case object Right extends Turn
  case object Left extends Turn

  def ChangeDirection(actual: Direction, turn: Turn): Direction = {
    (actual, turn) match {
      case (North, Right) => East
      case (North, Left) => West
      case (East, Right) => South
      case (East, Left) => North
      case (South, Right) => West
      case (South, Left) => East
      case (West, Right) => North
      case (West, Left) => South
    }
  }

  case class Point(val x: Int, val y: Int)

  def MovePosition(actual: Point, direction: Direction, steps: Int): Point = {
    direction match {
      case North => Point(actual.x, actual.y + steps)
      case East => Point(actual.x + steps, actual.y)
      case South => Point(actual.x, actual.y - steps)
      case West => Point(actual.x - steps, actual.y)
    }
  }

  def ParseInstruction(instruction: String): (Turn, Int) = {
    val turnCharacter = instruction.head
    val stepsCharacter = instruction.tail

    val turn = turnCharacter match {
      case 'R' => Right
      case 'L' => Left
      case _ => throw new IllegalArgumentException("No valid turn")
    }

    (turn, stepsCharacter.toInt)
  }

  def ProcessInstructions(initial: Point, direction: Direction,
                          instructions: List[String]): Point = {
    instructions match {
      case Nil => initial
      case x::xs => {
        val instruction = ParseInstruction(x)
        val newDirection = ChangeDirection(direction, instruction._1)
        val newPosition = MovePosition(initial, newDirection, instruction._2)
        ProcessInstructions(newPosition, newDirection, xs)
      }
    }
  }

  var visited: HashSet[Point] = new collection.immutable.HashSet[Point]

  def MovePositionStepByStep(actual: Point, direction: Direction, steps: Int): (Point, Option[Point]) = {
    if (steps == 0) (actual, None)
    else
      {
        if (visited.contains(actual)) (actual, Some(actual))
        else  {
          visited = visited + actual

          direction match {
            case North => MovePositionStepByStep(Point(actual.x, actual.y + 1), direction, steps - 1 )
            case East => MovePositionStepByStep(Point(actual.x + 1, actual.y), direction, steps - 1)
            case South => MovePositionStepByStep(Point(actual.x, actual.y - 1), direction, steps - 1)
            case West => MovePositionStepByStep(Point(actual.x - 1, actual.y), direction, steps - 1)
          }
        }
      }
  }

  def FindFirstRepeatedLocation(initial: Point, direction: Direction,
                                instructions: List[String])
  : Point = {
    instructions match {
      case Nil => initial
      case x::xs => {
        val instruction = ParseInstruction(x)
        val newDirection = ChangeDirection(direction, instruction._1)
        val newPosition = MovePositionStepByStep(initial, newDirection, instruction._2)
        if (newPosition._2.isDefined) newPosition._2.get
        else FindFirstRepeatedLocation(newPosition._1, newDirection, xs)
      }
    }
  }


  def TaxicabDistance(a: Point, b: Point): Int = {
    Math.abs(a.x - b.x) + Math.abs(a.y - b.y)
  }
}