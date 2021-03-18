package mylib

import spinal.core.{Bits, _}
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.graphic._
import spinal.lib.graphic.vga._

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Tools {
  def readmemh(path: String): Array[BigInt] = {
    val buffer = new ArrayBuffer[BigInt]
    for (line <- Source.fromFile(path).getLines) {
      val tokens: Array[String] = line.split("(//)").map(_.trim)
      if (tokens.length > 0 && tokens(0) != "") {
        val i = Integer.parseInt(tokens(0), 16)
        buffer.append(i)
      }
    }
    buffer.toArray
  }
}

case class RamInterface(data_w: Int, adr_w : Int) extends Bundle {
  val address = Reg(UInt(adr_w bits)).asOutput()
  val data_out = Reg(Bits(data_w bits)).asOutput()
  val data_in = Bits(data_w bits).asInput()
  val we = Reg(Bool()).asOutput()
}

class Chip8VgaCtrl(rgbConfig: RgbConfig, timingsWidth: Int = 12) extends Component {
  val io = new Bundle {
    val timings       = in(VgaTimings(timingsWidth))
    val vga           = master(Vga(rgbConfig))
    val vram_address  = out UInt(11 bits)
    val vram_data_in  = in Bits(1 bits)
  }

  case class HVArea(timingsHV: VgaTimingsHV, enable: Bool) extends Area {
    val counter = Reg(UInt(timingsWidth bit)) init(0)

    val syncStart = counter === timingsHV.syncStart
    val syncEnd = counter === timingsHV.syncEnd
    val colorStart = counter === timingsHV.colorStart
    val colorEnd = counter === timingsHV.colorEnd
    val polarity = timingsHV.polarity

    when(enable) {
      counter := counter + 1
      when(syncEnd) {
        counter := 0
      }
    }

    val sync    = RegInit(False) setWhen(syncStart) clearWhen(syncEnd)
    val colorEn = RegInit(False) setWhen(colorStart) clearWhen(colorEnd)
  }

  val h = HVArea(io.timings.h, True)
  val v = HVArea(io.timings.v, h.syncEnd)
  val colorEn = h.colorEn && v.colorEn
  io.vga.hSync := h.sync ^ h.polarity
  io.vga.vSync := v.sync ^ v.polarity
  io.vga.colorEn := colorEn

  when(colorEn & io.vram_data_in === 1){
    io.vga.color.r := (default -> true)
    io.vga.color.g := (default -> true)
    io.vga.color.b := (default -> true)
  }.otherwise(io.vga.color.clear())

  io.vram_address := ((h.counter-io.timings.h.colorStart)/10 +((v.counter-io.timings.v.colorStart)/10)*64).resized
}

class Chip8Core extends Component {
  val io = new Bundle {
    val ram = RamInterface(8,12)
    val vram = RamInterface(1,11)

    val key = in UInt(4 bits)
    val key_pressed = in Bool()
    val beep_en = out Bool()
  }
  //Internal registers:
  val program_counter = Reg(UInt(12 bits)) init(0x200)
  val index_register = Reg(UInt(12 bits))
  val registers = Reg(Vec(UInt(8 bits),16))
  val stack = Reg(Vec(UInt(12 bits),16))
  val stack_pointer = Reg(UInt(4 bits)) init(0)
  val delay_timer = Reg(UInt(8 bits)) init(0)
  val beep_timer = Reg(UInt(8 bits)) init(0)

  //Other signals:
  val instruction_hi = Reg(Bits(8 bits))

  val screen_pos = Reg(UInt(11 bits))
  val screen_x = Reg(UInt(6 bits))
  val screen_y = Reg(UInt(5 bits))
  val sprite_h = Reg(UInt(4 bits))
  val pos_col = Reg(UInt(4 bits))
  val pos_row = Reg(UInt(3 bits))

  val freq_divider = Reg(UInt(24 bits)) init(0)
  val random_number = Reg(UInt(8 bits))

  val copy_size = Reg(UInt(4 bits))
  val current_reg = Reg(UInt(4 bits))

  //Ease of use functions:
  def ins_regx() = instruction_hi(3 downto 0).asUInt
  def ins_regy() = io.ram.data_in(7 downto 4).asUInt
  def ins_n() = io.ram.data_in(3 downto 0).asUInt
  def ins_nn() = io.ram.data_in(7 downto 0).asUInt
  def ins_nnn() = (instruction_hi(3 downto 0) ## io.ram.data_in(7 downto 0)).asUInt

  val fsm = new StateMachine{
    io.ram.we := False
    io.vram.we := False

    val fetch_1 : State = new State with EntryPoint{
      whenIsActive {
        io.ram.address := program_counter
        program_counter := program_counter + 1
        goto(fetch_2)
      }
    }
    val fetch_2 : State = new State {
      whenIsActive {
        io.ram.address := program_counter
        program_counter := program_counter + 1
        goto(fetch_3)
      }
    }
    val fetch_3 : State = new State {
      whenIsActive {
        instruction_hi := io.ram.data_in
        goto(decode)
      }
    }
    val decode : State = new State {
      whenIsActive {
        goto(fetch_1)
        switch(instruction_hi(7 downto 4)) {
          is(0x0) {
            when(ins_nnn === 0x0E0) {
              goto(screen_clear)
            }.elsewhen(ins_nnn === 0x0EE) {
              program_counter := stack(stack_pointer - 1)
              stack_pointer := stack_pointer - 1
            }
          }
          is(0x1) {
            program_counter := ins_nnn
          }
          is(0x2) {
            stack(stack_pointer) := program_counter
            stack_pointer := stack_pointer + 1
            program_counter := ins_nnn
          }
          is(0x3) {
            when(registers(ins_regx) === ins_nn)(program_counter := program_counter + 2)
          }
          is(0x4) {
            when(registers(ins_regx) =/= ins_nn)(program_counter := program_counter + 2)
          }
          is(0x5) {
            when(registers(ins_regx) === registers(ins_regy))(program_counter := program_counter + 2)
          }
          is(0x6) {
            registers(ins_regx) := ins_nn
          }
          is(0x7) {
            registers(ins_regx) := registers(ins_regx) + ins_nn
          }
          is(0x8) { //Arthmetic
            switch(ins_n) {
              is(0x0) {
                registers(ins_regx) := registers(ins_regy)
              }
              is(0x1) {
                registers(ins_regx) := registers(ins_regx) | registers(ins_regy)
              }
              is(0x2) {
                registers(ins_regx) := registers(ins_regx) & registers(ins_regy)
              }
              is(0x3) {
                registers(ins_regx) := registers(ins_regx) ^ registers(ins_regy)
              }
              is(0x4) {
                registers(ins_regx) := registers(ins_regx) + registers(ins_regy)
                when(registers(ins_regx) + registers(ins_regy) > 255)(registers(15) := 1).otherwise(registers(15) := 0)
              }
              is(0x5) {
                registers(ins_regx) := registers(ins_regx) - registers(ins_regy)
                when(registers(ins_regx) > registers(ins_regy))(registers(15) := 1).otherwise(registers(15) := 0)
              }
              is(0x7) {
                registers(ins_regx) := registers(ins_regy) - registers(ins_regx)
                when(registers(ins_regy) > registers(ins_regx))(registers(15) := 1).otherwise(registers(15) := 0)
              }
              is(0x6) {
                registers(ins_regx) := registers(ins_regx) |>> 1
                when(registers(ins_regx)(0) === True)(registers(15) := 1).otherwise(registers(15) := 0)
              }
              is(0xE) {
                registers(ins_regx) := registers(ins_regx) |<< 1
                when(registers(ins_regx)(7) === True)(registers(15) := 1).otherwise(registers(15) := 0)
              }
            }
          }
          is(0x9) {
            when(registers(ins_regx) =/= registers(ins_regy))(program_counter := program_counter + 2)
          }
          is(0xA) {
            index_register := ins_nnn
          }
          is(0xB) {
            program_counter := ins_nnn + registers(0)
          }
          is(0xC) {
            registers(ins_regx) := random_number & ins_nn
          }
          is(0xD) {
            screen_x := registers(ins_regx).resized
            screen_y := registers(ins_regy).resized
            sprite_h := ins_n
            registers(15) := 0
            pos_col := 0
            goto(screen_draw_row)
          }
          is(0xE) {
            when(ins_nn === 0x9E) {
              when(registers(ins_regx) === io.key)(program_counter := program_counter + 2)
            }.elsewhen(ins_nn === 0xA1){
              when(registers(ins_regx) =/= io.key)(program_counter := program_counter + 2)
            }
          }
          is(0xF) {
            switch(ins_nn) {
              is(0x07) {
                registers(ins_regx) := delay_timer
              }
              is(0x0A) {
                when(io.key_pressed){
                  registers(ins_regx) := io.key.resized
                }.otherwise(program_counter := program_counter - 2)
              }
              is(0x15) {
                delay_timer := registers(ins_regx)
              }
              is(0x18) {
                beep_timer := registers(ins_regx)
              }
              is(0x1E) {
                index_register := index_register + registers(ins_regx)
              }
              is(0x29) {
                index_register := (0x50 + (registers(ins_regx)(3 downto 0)*5)).resized
              }
              is(0x33) {
                io.ram.address := index_register
                io.ram.data_out := (registers(ins_regx) / 100).asBits
                io.ram.we := True
                goto(bcd_2)
              }
              is(0x55) {
                copy_size := ins_regx()
                goto(reg_to_mem)
              }
              is(0x65) {
                current_reg := 0
                copy_size := ins_regx()
                goto(mem_to_reg)
              }
            }
          }
        }
      }
    }
    val reg_to_mem : State = new State {
      onEntry(current_reg := 0)
      whenIsActive {
        io.ram.address := index_register + current_reg
        io.ram.data_out := registers(current_reg).asBits
        io.ram.we := True
        current_reg := current_reg + 1
        when(current_reg === copy_size)(goto(fetch_1))
      }
    }
    val mem_to_reg : State = new State {
      whenIsActive {
        io.ram.address := index_register + current_reg
        goto(mem_to_reg_delay)
      }
    }
    val mem_to_reg_delay : State = new StateDelay(1){whenCompleted(goto(mem_to_reg_2))}
    val mem_to_reg_2 : State = new State {
      whenIsActive {
        registers(current_reg) := io.ram.data_in.asUInt
        current_reg := current_reg + 1
        goto(mem_to_reg)
        when(current_reg === copy_size)(goto(fetch_1))
      }
    }
    val bcd_2 : State = new State {
      whenIsActive {
        io.ram.address := index_register + 1
        io.ram.data_out := ((registers(ins_regx)/10)%10).asBits
        io.ram.we := True
        goto(bcd_3)
      }
    }
    val bcd_3 : State = new State {
      whenIsActive {
        io.ram.address := index_register + 2
        io.ram.data_out := (registers(ins_regx)%10).asBits
        io.ram.we := True
        goto(fetch_1)
      }
    }
    val screen_clear : State = new State {
      onEntry(screen_pos := 0)
      whenIsActive {
        io.vram.data_out := 0
        io.vram.address := screen_pos
        io.vram.we := True
        screen_pos := screen_pos + 1
        when(screen_pos === 2047) {
          goto(fetch_1)
        }
      }
    }
    val screen_draw_row : State = new State {
      onEntry(pos_row := 0)
      whenIsActive {
        io.ram.address := index_register + pos_col
        goto(screen_draw_pixel_1)
        when(sprite_h === pos_col | screen_y + pos_col === 32){goto(fetch_1)}
      }
    }
    val screen_draw_pixel_1 : State = new State {
      whenIsActive {
        io.vram.address := (((screen_y + pos_col)*64) + screen_x + pos_row).resized
        goto(screen_delay)
      }
    }
    val screen_delay : State = new StateDelay(1){whenCompleted(goto(screen_draw_pixel_2))}
    val screen_draw_pixel_2 : State = new State {
      whenIsActive {
        when(io.ram.data_in(7-pos_row) === True){
          io.vram.we := True
          io.vram.data_out := ~io.vram.data_in
          when(io.vram.data_in === 1){
            registers(15) := 1
          }
        }
        pos_row := pos_row + 1
        goto(screen_draw_pixel_1)
        when(pos_row === 7 | screen_x + pos_row === 63){ //end of row
          pos_col := pos_col + 1
          goto(screen_draw_row)
        }
      }
    }
  }

  //Timer function
  io.beep_en := beep_timer =/= 0
  freq_divider := freq_divider + 1
  when(freq_divider === ClockDomain.current.frequency.getValue.toInt/60){
    when(delay_timer =/= 0)(delay_timer := delay_timer - 1)
    when(beep_timer =/= 0)(beep_timer := beep_timer - 1)
    freq_divider := 0
  }
  //"""Random""" number generator, shoudl proably use LFSR
  random_number := random_number + 1
}

class TangChip8TopLevel extends Component {
  val rgb_config = RgbConfig(3,3,2)
  val io = new Bundle {
    val clk = in Bool()
    val resetn = in Bool()
    val vga = master(Vga(rgb_config))
    val beep = out Bool()
  }

  val resetCtrlClockDomain = ClockDomain(
    clock = io.clk,
    config = ClockDomainConfig(
      resetKind = BOOT
    )
  )
  val resetCtrl = new ClockingArea(resetCtrlClockDomain) {
    val coreResetUnbuffered = False
    val coreResetCounter = Reg(UInt(6 bits)) init(0)
    when(coreResetCounter =/= U(coreResetCounter.range -> true)){
      coreResetCounter := coreResetCounter + 1
      coreResetUnbuffered := True
    }
    when(BufferCC(~io.resetn)){
      coreResetCounter := 0
    }
    val coreReset = RegNext(coreResetUnbuffered)
  }
  val coreClockDomain = ClockDomain(
    clock     = io.clk,
    reset     = resetCtrl.coreReset,
    frequency = FixedFrequency(24 MHz)
  )

  val mainArea = new ClockingArea(coreClockDomain) {
    //Memories
    val ram_main = Mem(Bits(8 bits),4096)
    ram_main.initialContent = Tools.readmemh("rom3.txt")
    val ram_video = Mem(Bits(1 bits),2048)
    ram_video.initialContent = Array.fill(2048)(0)

    //Main Chip8Core
    val core = new Chip8Core
    core.io.ram.data_in := ram_main.readSync(core.io.ram.address)
    ram_main.write(core.io.ram.address,core.io.ram.data_out,core.io.ram.we)
    core.io.vram.data_in := ram_video.readSync(core.io.vram.address)
    ram_video.write(core.io.vram.address,core.io.vram.data_out,core.io.vram.we)

    //250Hz beep output
    val beep_timer = Reg(UInt(24 bits))
    val beep_wave = Reg(Bool())
    beep_timer := beep_timer + 1;
    when(beep_timer === ClockDomain.current.frequency.getValue.toInt/500){
      beep_wave := ~beep_wave
      beep_timer := 0
    }
    io.beep := core.io.beep_en & beep_wave

    //Key input
        core.io.key := 0
        core.io.key_pressed := False

    //vga output
    val vga_timings = VgaTimings(12)
    vga_timings.setAs( //640x320
      640,
      96,
      16,
      48,
      false,
      320,
      2,
      40,
      70,
      false
    )
    val vga_ctrl = new Chip8VgaCtrl(rgb_config)
    vga_ctrl.io.timings := vga_timings
    vga_ctrl.io.vram_data_in := ram_video.readSync(vga_ctrl.io.vram_address)
    vga_ctrl.io.vga <> io.vga

  }
}

object TangChip8Vhdl {
  def main(args: Array[String]) {
    SpinalVhdl(new TangChip8TopLevel)
  }
}