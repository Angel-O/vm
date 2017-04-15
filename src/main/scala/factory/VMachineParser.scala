package factory

import vm.VirtualMachineParser
import bc._
import vendor.Instruction

class VMachineParser extends VirtualMachineParser with ByteCodeValues{

  val vendorParser = VirtualMachineFactory.vendorParser
  val byteCodeParser = VirtualMachineFactory.byteCodeParser

  def parse(file: String): Vector[ByteCode] = doParsing(vendorParser.parse(file))

  def parseString(str: String): Vector[ByteCode] = doParsing(vendorParser.parseString(str))

/**
  * Helper method to parse a collection of Instructions into an Vector of ByteCode.
  * It will deconstruct each instruction and create a list containing all members
  * of every instruction and the parse the the list of instruction to get a list of
  * Byte, finally it will delegate the task of creating a vector of ByteCode to
  * the `ByteCodeParser`.
  *
  * @param instructions Vector of `Instruction` instances
  * @return a Vector of `ByteCode`
  * @throws InvalidIBytecodeException if any instruction name does not match a
  * 	valid instruction name
  */
private def doParsing(instructions: Vector[Instruction]) = {

    // map-map-flatten: map each instruction to a pair of (name: String, args: Vector[Int]) and then map the sequence of pairs
    // to a list of vectors of string and integers, finally flatten the list of vectors to a list strings and integers
    val flattenedInstructions = instructions.map(i => (i.name, i.args)).flatMap(

        pair => pair match {
          case (x, head +: tail) => Vector(x, head) // deconstruct the second element of the pair into head and tail
          case (x, _) => Vector(x) // the second element of the pair holds an empty vector
          }
    )

    // parse the flattened list to get a Byte vector: if a String is found get the correspondent byte from the bytecode map
    // or throw an exception if the value cannot be found; if an Int is found then it was an argument that we can safely turn into a byte
    val parsed = for (element <- flattenedInstructions) yield element match {

      case s: String => bytecode.getOrElse(s, throw new InvalidBytecodeException(s"The '$s' bytecode does not match any instruction name"))
      case n: Int => n toByte
    }

    // parse the Byte vector
    byteCodeParser.parse(parsed)
  }

}