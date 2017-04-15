package factory

import vm.VirtualMachine
import bc.ByteCode
import scala.collection.mutable.ListBuffer
import vm.MachineUnderflowException

class VMachine extends VirtualMachine {

  // using a Vector as a stack
  val stack = ListBuffer[Int]()

  def execute(bc: Vector[ByteCode]): VirtualMachine = bc match {

    // performing a recursive call
    case head +: tail => executeOne(bc)._2 execute(tail)

    // if 'bc' is empty (aka there are no commands to execute) return
    // the original Vector and the Virtual Machine as it is. Note this
    // will only happen if we pass an empty Vector to this method: if
    // we pass a non-empty stack the executeOne method will have already
    // covered such scenario
    case _ => this
  }

  def executeOne(bc: Vector[ByteCode]): (Vector[ByteCode], VirtualMachine) = bc match {

    // execute the first command and return a pair with the remaining ones
    // and the result of the cmmand execution: Note here "execute" is the
    // command method...
    case head +: tail => (tail, head.execute(this))

    // if 'bc' is empty (aka there are no commands to execute) return
    // the original Vector and the Virtual Machine as it is
    case _ => (bc, this)
  }

  def pop(): (Int, VirtualMachine) = {

    // remove the element on top of the stack (position 0) if the stack is not empty, otherwise throw an exception
    if (!stack.isEmpty) (stack.remove(0), this) else throw new MachineUnderflowException("Cannot pop from empty stack.")
  }

  def push(value: Int): VirtualMachine = {

    // prepend the value to the stack (this will edit the original stack)
    // adn return the virtual machine
    value +=: stack
    this
  }

  def state: Vector[Int] = stack.toVector
}