package mylib

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import scala.util.Random


//MyTopLevel's testbench
object MyTopLevelSim {
  def main(args: Array[String]) {
    SimConfig.withWave.doSim(new MyTopLevel){dut =>
      dut.mainClockDomain.forkStimulus(period = 10)
      sleep(10000000)
      simSuccess()
    }
  }
}
