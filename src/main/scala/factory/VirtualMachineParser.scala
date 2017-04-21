package factory

import vm.{VirtualMachineParser => VMP}
import bc.{InvalidBytecodeException => IBE, ByteCodeParser => BCP, _}
import vendor.{Instruction, ProgramParser}

class VirtualMachineParser(vendorParser: ProgramParser, byteCodeParser: BCP) extends VMP with ByteCodeValues{

  def parse(file: String): Vector[ByteCode] = doParsing(vendorParser.parse(file))

  def parseString(str: String): Vector[ByteCode] = doParsing(vendorParser.parseString(str))

/**
  * Helper method to parse a collection of `Instruction` into an Vector of `ByteCode`.
  * It will map each instruction to a list containing all members of
  * each instruction and then parse the resulting list of "deconstructed" instructions
  * to get a Byte list. Finally it will delegate the task of creating a ByteCode
  * vector to the `ByteCodeParser`.
  *
  * @param instructions Vector of `Instruction` instances
  * @return a Vector of `ByteCode`
  * @throws InvalidIBytecodeException if any instruction name does not match a
  * 	valid instruction name
  */
private def doParsing(instructions: Vector[Instruction]) = {

    // map each instruction to their string representation, then split the string using a space as separator
    // to get an array representing name as first element and args as second (if it's there). Finally flatten
    // the resulting collection of string arrays to get a vector of strings
    val flattenedInstructions = instructions.flatMap(_.toString.split(' '))

    // try mapping each string to the corresponding bytecode (if any). If that fails check if it's a numeric string
    // and if so, convert it to a byte, otherwise throw an exception.
    val instructionsToByte = for (member <- flattenedInstructions) yield bytecode.get(member) match {
      case option: Some[Byte] => option.get
      case None => if (member forall(_ isDigit)) member toByte else throw new IBE(s"Unknown instruction: '$member'.")
    }

    // parse the Byte vector
    byteCodeParser.parse(instructionsToByte)
  }
}