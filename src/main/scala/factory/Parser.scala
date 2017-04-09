package factory

import scala.collection.mutable.ListBuffer
import scala.reflect.io.File
import vendor._

class Parser extends ProgramParser {

  // TODO: should we make these private?
  val namePattern = "([a-zA-Z]+)".r
  val nameNumPattern = s"$namePattern ([0-9]+)".r

  def parse(file: String): InstructionList = doParsing(File(file).lines)

  def parseString(string: String): InstructionList = doParsing(string.split('\n'))


/**
  * Helper method to parse a collection of strings into an InstructionList
  *
  * @param instructions collection of strings representing the intructions to be parsed
  * @return a collection of Instruction instances or throws an error
  * @throws InvalidInstructionFormatException
  */
private def doParsing(instructions: TraversableOnce[String]): InstructionList = {

    for(toBeParsed <- instructions.toVector) yield {

      toBeParsed match {
        case namePattern(name) => new Instruction(name, Vector[Int]())
        case nameNumPattern(name, number) => new Instruction(name, Vector(number.toInt))
        case _ => throw new InvalidInstructionFormatException(s"Unable to parse '$toBeParsed' instruction.")
      }
    }
  }
}