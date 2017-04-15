package factory

import bc.{ByteCodeFactory, ByteCodeParser}
import vendor.ProgramParser
import vm.{VirtualMachine, VirtualMachineParser}


/**
  * The `VirtualMachineFactory` follows the *factory pattern*. It provides
  * methods for each of the important parts in this assignment. You must
  * implement each method such that it returns an object of the correct type.
  */
object VirtualMachineFactory {
  // TODO
  def byteCodeFactory: ByteCodeFactory = new BCodeFactory

  // TODO
  def vendorParser: ProgramParser = new VParser

  // TODO
  def byteCodeParser: ByteCodeParser = new BCodeParser

  // TODO
  def virtualMachineParser: VirtualMachineParser = new VMachineParser

  // TODO
  def virtualMachine: VirtualMachine = ???
}
