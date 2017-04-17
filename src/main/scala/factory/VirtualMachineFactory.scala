package factory

import bc.{ByteCodeFactory => BCF, ByteCodeParser => BCP}
import vendor.ProgramParser
import vm.{VirtualMachine => VM, VirtualMachineParser => VMP}


/**
  * The `VirtualMachineFactory` follows the *factory pattern*. It provides
  * methods for each of the important parts in this assignment. You must
  * implement each method such that it returns an object of the correct type.
  */
object VirtualMachineFactory {
  // TODO
  def byteCodeFactory: BCF = new ByteCodeFactory

  // TODO
  def vendorParser: ProgramParser = new VendorParser

  // injecting the bytecode factory
  def byteCodeParser: BCP = new ByteCodeParser(byteCodeFactory)

  // injecting the vendor parser and the bytecode parser into the adapter
  def virtualMachineParser: VMP = new VirtualMachineParser(vendorParser, byteCodeParser)

  // TODO
  def virtualMachine: VM = VirtualMachine()
}
