package mylib

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib.graphic.RgbConfig

import scala.util.Random


//MyTopLevel's testbench
object Chip8LevelSim {
  def main(args: Array[String]) {
    SimConfig.withWave.doSim(new Chip8TopLevel(RgbConfig(3,3,2),FixedFrequency(24 MHz))){dut =>
      dut.coreClockDomain.forkStimulus(period = 10)
      sleep(10000000)
      simSuccess()
    }
  }
}
