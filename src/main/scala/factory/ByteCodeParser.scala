package factory

import bc.{ByteCodeParser => BCP, ByteCodeFactory => BCF, _}

class ByteCodeParser(factory: BCF) extends BCP {

  // byte corresponding to the Iconst instruction: NOTE: needs to start with a capital as
  // it is used below as a constant to "pattern-match" against, not as a 'match' variable.
  val Iconst = bytecode(names(0))

  // returning the second element of the pair
  def parse(bytes: Vector[Byte]): Vector[ByteCode] = scanVectors(bytes)._2

/**
  * This recursive method will do the heavy work and parse a Vector of Bytes into a Vector
  * of ByteCodes
  *
  * @param bytes Vector of Bytes to be parsed
  * @param bytecodes optional Vector of ByteCodes that will be recursively replaced with a Vector
  * 	containing the Bytecodes wanted: defaults to an empty vector
  * @return a pair of Vectors, where the second element of the pair is a Vector of `ByteCode` instances
  */
def scanVectors(bytes: Vector[Byte], bytecodes: Vector[ByteCode] = Vector.empty): (Vector[Byte], Vector[ByteCode]) = bytes match {

    // if bytes contains at least two elements and the head is equal to Iconst we will
    // "consume" the head and the first element of the tail (ht) and keep scanning the tail
    // of the tail (tt)
    case Iconst +: ht +: tt => scanVectors(tt, bytecodes :+ factory.make(Iconst, ht))

    // if bytes has an empty tail (aka single element) OR it has at least two elements but the head
    // is not equal to Iconst (this is the complemetary clause to the first match case) we will
    // "consume" the head of the bytes Vector and keep scanning its tail
    case _+:_ | _+:_+:_ => scanVectors(bytes.tail, bytecodes :+ factory.make(bytes.head))

    // if bytes is empty we are done: return the pair of vectors
    case _ => (bytes, bytecodes)
  }
}