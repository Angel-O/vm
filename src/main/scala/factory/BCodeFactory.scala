package factory

import bc.{ByteCodeFactory => Factory, InvalidBytecodeException => IBE, _}

class BCodeFactory extends Factory with ByteCodeValues {

  // maintain a Set of unary bytecodes, that is bytcodes that take no arguments
  val unaryByteCodes = Vector(Iadd, Isub, Imul, Idiv, Irem, Ineg, Iinc, Idec, Idup, Iswap, Iprint) distinct

  // map each one of them to their code for greater flexibility: each bytecode
  // has a unique byte by definition so inverting the mapping is safe
  val byteToBytecode = ( for((byteCode, byte) <- unaryByteCodes.zip(unaryByteCodes.map( u => u.code))) yield (byte, byteCode) ) toMap

  def make(byte: Byte, args: Int*): ByteCode = (byte, args size) match {

    // valid binary bytecode
    case (1, 1) => Iconst(args(0))

    // binary bytecode with too few or too many args
    case (1, _) => throw new IBE(s"'${names(0)}' instruction requires at least and at most one argument.")

    // valid unary bytecode, (if it's unknown an IBE will be thrown)
    case (b, 0) => byteToBytecode.getOrElse(b, throw new IBE(s"'$b' byte code is not valid."))

    // invalid (e.g: negative or out of range) unary bytecode (or valid but with too many args)
    case _ => throw new IBE("invalid instruction.")
  }
}