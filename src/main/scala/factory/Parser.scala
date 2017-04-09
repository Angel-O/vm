package factory

import scala.collection.mutable.ListBuffer
import scala.reflect.io.File
import vendor._

class Parser extends ProgramParser {

  // TODO: should we make these private?
  val nameNumPattern = "([a-zA-Z]+) ([0-9]+)".r // space is represented by an actual space in the regex
  val namePattern = "([a-zA-Z]+)".r

  def parse(file: String): Vector[Instruction] = {

    // create a file and parse each line
    // TODO: is this efficient? will it generate a new file at each iteration?
    for(line <- File(file).lines().toVector) yield doParsing(line)
  }

  def parseString(string: String): InstructionList = {

    // turning the array to a vector since "yield"
    // returns the same type of collection being iterated
    for(line <- string.split('\n').toVector) yield doParsing(line)
  }


  /**
   * Helper method to parse a string into an Instruction
 	 * @param string string representation of the Intruction to be parsed
   * @return a valid Instruction or throws an error
   */
  private def doParsing(string: String):Instruction = {

    string match{
        case nameNumPattern(name, number) => new Instruction(name, Vector(number.toInt))
        // TODO: establish if this will be empty or contain a zero
        case namePattern(name) => new Instruction(name, Vector[Int]())
        case _ => throw new InvalidInstructionFormatException("Invalid instruction format")
    }
  }
}