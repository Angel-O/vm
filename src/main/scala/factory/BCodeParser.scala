package factory

import bc._
import scala.collection.mutable.ListBuffer

class BCodeParser extends ByteCodeParser {

  val factory = new BCodeFactory

  // byte corresponding to the Iconst instruction: needs to be capital(see below)
  val Iconst = bytecode(names(0))

  def parse(bytes: Vector[Byte]): Vector[ByteCode] = {

      val ignored = ListBuffer[Int]()

      // The ignored list contains elements that have already been used in a previous iteration
      // as an argument to build an Iconst instruction and therfore do not need to be scanned
      for(i <- bytes.indices.toVector if( !ignored.contains(i) )) yield {

        bytes(i) match{ // match against the current element

          // if the current element is an Iconst create the correspondent command using
          // the next element as a an argument. NOTE: the item we are matching against
          // needs to start with a capital letter as it does not represent a 'match' variable
          // , but a constant we are matching against. Add the next element to the ignored
          // list as we are "consuming" it in this iteration.
          case Iconst => ignored += i + 1;  factory.make(bytes(i), bytes(i + 1))

          // otherwise create a bytecode command with the current element
          case x => factory make x
        }
      }
  }
}