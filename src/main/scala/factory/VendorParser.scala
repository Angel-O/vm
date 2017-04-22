package factory

import scala.reflect.io.File
import vendor._

class VendorParser extends ProgramParser {

  // TODO: should we make these private? or move them to another (nested) singleton object?
  val noSpaces = "([^ ]+)".r // matching anything but spaces or numeric characters
  val atLeastOneNonNum = ".*([^0-9]+).*".r // matching strings with at least one non-numeric character
  val numeric = "([0-9]+)".r // matching only numeric strings

  // combined patterns
  val noSpaces_num = s"$noSpaces $numeric".r
  val atLeastOneNonNum_num = s"$atLeastOneNonNum $numeric".r

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
    // same collection type that is being iterated. Yielding a pair
    // with "twin" elements so we can match two conditions at once
    // without nested cases of if else stmts
    for(toBeParsed <- instructions.toVector) yield (toBeParsed, toBeParsed) match {

      case (noSpaces(_), atLeastOneNonNum(_)) => new Instruction(toBeParsed, Vector.empty)
      case (noSpaces_num(name, number), atLeastOneNonNum_num(_, _)) => new Instruction(name, Vector(number.toInt))
      case _ => throw new InvalidInstructionFormatException(s"Unable to parse '$toBeParsed' instruction.")
    }
  }
}