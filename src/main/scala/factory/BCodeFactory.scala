package factory

import bc.{ByteCodeFactory => Factory, _}

class BCodeFactory extends Factory with ByteCodeValues {

  // val validBytes = names collect(bytecode)

  val inverse = for ((name, byte) <- bytecode) yield (byte, name)

  val unaryByteCodes = Set(Iadd, Isub, Imul, Idiv, Irem, Ineg, Iinc, Idec, Idup, Iswap, Iprint)

  def make(byte: Byte, args: Int*): ByteCode = {

    byte match {
      case 1 => args match {
          case Nil => throw new InvalidBytecodeException(
                          s"'${names(0)}' instruction requires at least one argument.")
          case _ => if(args.size > 2)
                      throw new InvalidBytecodeException(
                          s"'${names(0)}' instruction can have at most one argument.")
                    else
                      new Iconst(args(0))
        }
      case 2 => Iadd
      case 3 => Isub
      case 4 => Imul
      case 5 => Idiv
      case 6 => Irem
      case 7 => Ineg
      case 8 => Iinc
      case 9 => Idec
      case 10 => Idup
      case 11 => Iswap
      case 12 => Iprint
      case invalid => throw new InvalidBytecodeException(s"'$invalid' byte code is not valid.")
    }
  }
}