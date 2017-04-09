package factory

import scala.collection.mutable.ListBuffer
import scala.reflect.io.File
import vendor._

class Parser extends ProgramParser {

  // TODO: should we make these private?
  val nameNumPattern = "([a-zA-Z]+) ([0-9]+)".r // space is represented by an actual space in the regex
  val namePattern = "([a-zA-Z]+)".r

  def parse(file: String): Vector[Instruction] = doParsing(File(file).lines)

  def parseString(string: String): InstructionList = doParsing(string.split('\n'))


  /**
   * Helper method to parse a string into an Instruction
 	 * @param instructions
 	 * 						collection of strings representing the intructions to be parsed
   * @return a valid Instruction or throws an error
   */
  private def doParsing(instructions: TraversableOnce[String]): Vector[Instruction] = {

    for(toBeParsed <- instructions.toVector) yield {

      toBeParsed match {
        case namePattern(name) => new Instruction(name, Vector[Int]())
        case nameNumPattern(name, number) => new Instruction(name, Vector(number.toInt))
        case _ => throw new InvalidInstructionFormatException("Invalid instruction format")
      }
    }
  }
}