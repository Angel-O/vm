package factory

import vm.{MachineUnderflowException => MUE, _}
import bc.ByteCode

case class VMachine(private val stack: Vector[Int] = Vector empty) extends VirtualMachine {

  def execute(bc: Vector[ByteCode]): VirtualMachine = bc match {

    // performing a recursive call: Note here "execute" is the virtual
    // machine method
    case _ +: tail => executeOne(bc)._2 execute(tail)

    // if 'bc' is empty (aka there are no commands to execute) return
    // the original Virtual Machine as it is.
    case _ => this
  }

  def executeOne(bc: Vector[ByteCode]): (Vector[ByteCode], VirtualMachine) = bc match {

    // execute the first command and return a pair with the remaining ones
    // and the result of the command execution: Note here "execute" is the
    // command method
    case head +: tail => (tail, head execute this)

    // if 'bc' is empty (aka there are no commands to execute) return
    // the original Vector and the Virtual Machine as it is
    case _ => (bc, this)
  }

  // if the stack is empty throw an MUE, otherwise return the head of the stack
  // and a new virtual machine having a stack equal to the tail of the current stack
  def pop(): (Int, VirtualMachine) = if (stack isEmpty) throw new MUE("Cannot pop from empty stack.") else (stack head, VMachine(stack tail))

  // return a virtual machine having a stack obtained by prepending the value to the current stack
  def push(value: Int): VirtualMachine = VMachine(value +: stack)

  def state: Vector[Int] = stack
}