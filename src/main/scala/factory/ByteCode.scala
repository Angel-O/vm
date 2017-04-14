package factory

import bc.ByteCode
import vm.VirtualMachine


final case class Iconst(val value: Int) extends ByteCode {

  val name = names(0)
  val code: Byte = bytecode(name)
  def execute(vm: VirtualMachine) = vm.push(value)
}

final case object Iadd extends ByteCode {

  val name = names(1)
  val code: Byte = bytecode(name)
  def execute(vm: VirtualMachine) = {
    val first = vm.pop()
    val second = first._2.pop()
    second._2.push(first._1 + second._1)
    }
}

final case object Isub extends ByteCode {

  val name = names(2)
  val code: Byte = bytecode(name)
  def execute(vm: VirtualMachine) = {
    val first = vm.pop()
    val second = first._2.pop()
    second._2.push(first._1 - second._1)
    }
}

final case object Imul extends ByteCode {

  val name = names(3)
  val code: Byte = bytecode(name)
  def execute(vm: VirtualMachine) = {
    val first = vm.pop()
    val second = first._2.pop()
    second._2.push(first._1 * second._1)
    }
}

final case object Idiv extends ByteCode {

  val name = names(4)
  val code: Byte = bytecode(name)
  def execute(vm: VirtualMachine) = {
    val first = vm.pop()
    val second = first._2.pop()
    second._2.push(first._1 / second._1)
    }
}

final case object Irem extends ByteCode {

  val name = names(5)
  val code: Byte = bytecode(name)
  def execute(vm: VirtualMachine) = {
    val first = vm.pop()
    val second = first._2.pop()
    second._2.push(first._1 % second._1)
    }
}

final case object Ineg extends ByteCode {

  val name = names(6)
  val code: Byte = bytecode(name)
  def execute(vm: VirtualMachine) = {
    val elem = vm.pop()
    elem._2.push(-elem._1)
    }
}

final case object Iinc extends ByteCode {

  val name = names(7)
  val code: Byte = bytecode(name)
  def execute(vm: VirtualMachine) = {
    val elem = vm.pop()
    elem._2.push(elem._1 + 1)
    }
}

final case object Idec extends ByteCode {

  val name = names(8)
  val code: Byte = bytecode(name)
  def execute(vm: VirtualMachine) = {
    val elem = vm.pop()
    elem._2.push(elem._1 - 1)
    }
}

final case object Idup extends ByteCode {

  val name = names(9)
  val code: Byte = bytecode(name)
  def execute(vm: VirtualMachine) = {
    val first = vm.pop()
    val second = first._2.pop()
    val newVm = second._2.push(second._1)
    newVm.push(first._1)
    }
}

final case object Iswap extends ByteCode {

  val name = names(10)
  val code: Byte = bytecode(name)
  def execute(vm: VirtualMachine) = vm.push(vm.pop()._1)
}

final case object Iprint extends ByteCode {

  val name = names(11)
  val code: Byte = bytecode(name)
  def execute(vm: VirtualMachine) = {
    val elem = vm.pop()
    println(elem._1)
    elem._2
  }
}