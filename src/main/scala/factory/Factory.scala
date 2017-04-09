package factory

import bc.ByteCodeFactory
import bc.ByteCode
import bc.ByteCodeValues
import bc.InvalidBytecodeException

class Factory extends ByteCodeFactory with ByteCodeValues {

  def make(byte: Byte, args: Int*): ByteCode = {

    byte match {
      case 1 => args match {
          case Nil => throw new InvalidBytecodeException(
                          s"'${names(1)}' instruction requires at least one argument.")
          case _ => if(args.size > 2)
                      throw new InvalidBytecodeException(
                          s"'${names(1)}' instruction can have at most one argument.")
                    else
                      new Bconst(args(0))
        }
      case 2 => Badd
      case 3 => Bsub
      case 4 => Bmul
      case 5 => Bdiv
      case 6 => Brem
      case 7 => Bneg
      case 8 => Binc
      case 9 => Bdec
      case 10 => Bswap
      case 11 => Bdup
      case 12 => Bprint
      case invalid => throw new InvalidBytecodeException(s"'$invalid' byte code is not invalid.")
    }
  }
}