package factory

import bc.{ByteCodeFactory => Factory, InvalidBytecodeException => IBE, _}
import scala.collection.immutable.SortedSet

class ByteCodeFactory extends Factory with ByteCodeValues {

  // maintain a Set (ideally this would be a sorted set, let's just call the "distinct" method)
  // of unary bytecodes, that is bytcodes that take no arguments
  val unaryByteCodes = Vector(Iadd, Isub, Imul, Idiv, Irem, Ineg, Iinc, Idec, Idup, Iswap, Iprint) distinct

  // map each one of them to their code and then invert the mapping: each bytecode
  // has a unique byte by definition so inverting the mapping is safe
  val byteToBytecode = ( for((byteCode, byte) <- unaryByteCodes.zip(unaryByteCodes.map(_.code))) yield (byte, byteCode) ) toMap

  def make(byte: Byte, args: Int*): ByteCode = (byte, args size) match {

    // valid binary bytecode or invalid binary bytecode (too few or too many args)
    case (1, size) => if (size == 1) Iconst(args(0)) else throw new IBE(s"'${names(0)}' bytecode command requires exactly one argument.")

    // valid unary bytecode, (if it's unknown an IBE will be thrown)
    case (byte, 0) => byteToBytecode.getOrElse(byte, throw new IBE(s"'$byte' byte is not associated to a valid bytecode command."))

    // invalid (e.g: negative or out of range) unary bytecode (or valid but with too many args)
    case _ => throw new IBE("unable to make bytecode command: invalid parameters.")
  }
}