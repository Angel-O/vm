package factory

import bc._

class BCodeParser extends ByteCodeParser {

  val factory = new BCodeFactory

  def parse(bytes: Vector[Byte]): Vector[ByteCode] = {

      // byte corresponding to the Iconst instruction: needs to be capital(see below)
      val Iconst = bytecode(names(0))

      // use the guard wisely: if the index is zero the second condition, that could throw an
      // IndexOutOfBounds exception, won't be evaluated). The second condition in the guard
      // checks wether or not the current element has already been used in a previous iteration
      // as an argument to build an Iconst instruction
      for(i <- bytes.indices.toVector if( i == 0 || bytes(i - 1) != Iconst) ) yield {

        bytes(i) match{ // match against the current element

          // if the current element is an Iconst create the correspondent command using
          // the next element as a an argument. NOTE: the item we are matching against
          // needs to start with a capital letter as it does not represent a 'match' variable
          // , but a constant we are matching against.
          case Iconst => factory.make(bytes(i), bytes(i + 1))

          // otherwise create a bytecode command with the current element
          case x => factory make x
        }
      }
  }
}