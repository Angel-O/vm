package factory

import bc.ByteCode
import vm.VirtualMachine

final case class Bconst(val value: Int) extends ByteCode {

  val name = names(1)
  val code: Byte = bytecode(name)
  def execute(vm: VirtualMachine) = vm.push(value)
}

final case object Badd extends ByteCode {

  val name = names(2)
  val code: Byte = bytecode(name)
  def execute(vm: VirtualMachine) = {
    val first = vm.pop()
    val second = first._2.pop()
    second._2.push(first._1 + second._1)
    }
}

final case object Bsub extends ByteCode {

  val name = names(3)
  val code: Byte = bytecode(name)
  def execute(vm: VirtualMachine) = {
    val first = vm.pop()
    val second = first._2.pop()
    second._2.push(first._1 - second._1)
    }
}

final case object Bmul extends ByteCode {

  val name = names(4)
  val code: Byte = bytecode(name)
  def execute(vm: VirtualMachine) = {
    val first = vm.pop()
    val second = first._2.pop()
    second._2.push(first._1 * second._1)
    }
}

final case object Bdiv extends ByteCode {

  val name = names(5)
  val code: Byte = bytecode(name)
  def execute(vm: VirtualMachine) = {
    val first = vm.pop()
    val second = first._2.pop()
    second._2.push(first._1 / second._1)
    }
}

final case object Brem extends ByteCode {

  val name = names(6)
  val code: Byte = bytecode(name)
  def execute(vm: VirtualMachine) = {
    val first = vm.pop()
    val second = first._2.pop()
    second._2.push(first._1 % second._1)
    }
}

final case object Bneg extends ByteCode {

  val name = names(7)
  val code: Byte = bytecode(name)
  def execute(vm: VirtualMachine) = {
    val elem = vm.pop()
    elem._2.push(-elem._1)
    }
}

final case object Binc extends ByteCode {

  val name = names(8)
  val code: Byte = bytecode(name)
  def execute(vm: VirtualMachine) = {
    val elem = vm.pop()
    elem._2.push(elem._1 + 1)
    }
}

final case object Bdec extends ByteCode {

  val name = names(9)
  val code: Byte = bytecode(name)
  def execute(vm: VirtualMachine) = {
    val elem = vm.pop()
    elem._2.push(elem._1 - 1)
    }
}

final case object Bswap extends ByteCode {

  val name = names(10)
  val code: Byte = bytecode(name)
  def execute(vm: VirtualMachine) = {
    val first = vm.pop()
    val second = first._2.pop()
    val newVm = second._2.push(second._1)
    newVm.push(first._1)
    }
}

final case object Bdup extends ByteCode {

  val name = names(11)
  val code: Byte = bytecode(name)
  def execute(vm: VirtualMachine) = vm.push(vm.pop()._1)
}

final case object Bprint extends ByteCode {

  val name = names(12)
  val code: Byte = bytecode(name)
  def execute(vm: VirtualMachine) = {
    val elem = vm.pop()
    println(elem._1)
    elem._2
  }
}