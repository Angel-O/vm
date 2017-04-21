package factory

import scala.reflect.io.File
import vendor._

class VendorParser extends ProgramParser {

  // TODO: should we make these private? or move them to another (nested) singleton object?
  val noSpaces = "([^ ]+)".r // anything but spaces or numeric characters
  val atLeastOneAlpha = ".*([^0-9]+).*".r // at least one non numeric character
  val noSpaceNum = s"$noSpaces ([0-9]+)".r // combining with the first pattern
  val atLeastOneAlphaNum = s"$atLeastOneAlpha ([0-9]+)".r // combining with the second pattern

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
    // same collection type that is being iterated.
    // Yielding a pair so we can match two conditions at once without nested cases of if else stmts
    for(toBeParsed <- instructions.toVector) yield (toBeParsed, toBeParsed) match {

        case (noSpaces(_), atLeastOneAlpha(_)) => new Instruction(toBeParsed, Vector.empty)
        case (noSpaceNum(name, number), atLeastOneAlphaNum(_, _)) => new Instruction(name, Vector(number.toInt))
        case _ => throw new InvalidInstructionFormatException(s"Unable to parse '$toBeParsed' instruction.")
    }
  }
}