package factory

import scala.reflect.io.File
import vendor._

class VendorParser extends ProgramParser {

  // TODO: should we make these private? or move them to another (nested) singleton object?
  val namePattern = "([^0-9]+)".r // accepting any non-numeric character
  val nameNumPattern = s"$namePattern ([0-9]+)".r

  def parse(file: String): InstructionList = doParsing(File(file).lines)

  def parseString(string: String): InstructionList = doParsing(string.split('\n'))

/**
  * Helper method to parse a collection of strings into an InstructionList
  *
  * @param instructions collection of strings representing the intructions to be parsed
  * @return an InstructionList, that is a Vector of Instruction instances
  * @throws InvalidInstructionFormatException if any string has an invalid format
  */
private def doParsing(instructions: TraversableOnce[String]): InstructionList = {

    // turning it into a vector as a 'for-comprehension' returns the
    // same collection type that is being iterated
    for(toBeParsed <- instructions.toVector) yield {

      toBeParsed match {
        case namePattern(name) => new Instruction(name, Vector.empty)
        case nameNumPattern(name, number) => new Instruction(name, Vector(number.toInt))
        case _ => throw new InvalidInstructionFormatException(s"Unable to parse '$toBeParsed' instruction.")
      }
    }
  }
}