package mylib

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.graphic._
import spinal.lib.graphic.vga._
import spinal.lib.misc.BinTools

case class RamInterface(data_w: Int, adr_w : Int) extends Bundle {
  val address = Reg(UInt(adr_w bits)).asOutput()
  val data_out = Reg(Bits(data_w bits)).asOutput()
  val data_in = Bits(data_w bits).asInput()
  val we = RegInit(False).asOutput()
}

case class SdInterface() extends Bundle {
  val cs = out Bool()
  val mosi = out Bool()
  val miso = in Bool()
  val sclk = out Bool()
}

class sd_controller(clockRate: Int, slowClockDivider: Int) extends BlackBox {
  val generic = new Generic {
    val clockRate = sd_controller.this.clockRate
    val slowClockDivider = sd_controller.this.slowClockDivider
  }
  val io = new Bundle {
    val reset = in Bool()
    val clk = in Bool()

    val sd_int = SdInterface()

    val card_present = in Bool()
    val card_write_prot = in Bool()

    val rd = in Bool()
    val rd_multiple = in Bool()
    val dout = out Bits(8 bits)
    val dout_avail = out Bool()
    val dout_taken = in Bool()

    val wr = in Bool()
    val wr_multiple = in Bool()
    val din = in Bits(8 bits)
    val din_valid = in Bool()
    val din_taken = out Bool()

    val addr = in Bits(32 bits)
    val erase_count = in Bits(8 bits)

    val sd_error = out Bool()
    val sd_busy = out Bool()
    val sd_error_code = out Bits(3 bits)
  }
  noIoPrefix()
  addRTLPath("./src/rtl/sd_spi.vhd")
  mapClockDomain(clock = io.clk, reset = io.reset)
  private def renameIO(): Unit = {
    io.flatten.foreach(bt => {
      if(bt.getName().contains("sd_int_")) bt.setName(bt.getName().replace("sd_int_", ""))
    })
  }
  addPrePopTask(() => renameIO())
}

class Chip8VgaCtrl(rgbConfig: RgbConfig, timingsWidth: Int = 12) extends Component {
  val io = new Bundle {
    val timings       = in(VgaTimings(timingsWidth))
    val vga           = master(Vga(rgbConfig))

    val vram_address  = out UInt(11 bits)
    val vram_data_in  = in Bits(1 bits)
    val color_sel     = in UInt(3 bits)
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

  when(colorEn){
    when(v.counter > io.timings.v.colorStart + 15 & v.counter < io.timings.v.colorEnd - 15 ){
      io.vga.color.r := (default -> (io.color_sel(2) & io.vram_data_in.asBool))
      io.vga.color.g := (default -> (io.color_sel(1) & io.vram_data_in.asBool))
      io.vga.color.b := (default -> (io.color_sel(0) & io.vram_data_in.asBool))
    }.otherwise{
      io.vga.color.r := (default -> False, 0 -> True)
      io.vga.color.g := (default -> False, 0 -> True)
      io.vga.color.b := (default -> False, 0 -> True)
    }
  }.otherwise(io.vga.color.clear())

  io.vram_address := ((h.counter-io.timings.h.colorStart)/10 +((v.counter-io.timings.v.colorStart - 15)/10)*64).resized
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
  val ins_freq_divider = Reg(UInt(24 bits)) init(0)
  val random_number = Reg(UInt(8 bits))

  val reg_in_ins = Reg(UInt(4 bits))
  val current_reg = Reg(UInt(4 bits))

  //Ease of use functions:
  def ins_regx() = instruction_hi(3 downto 0).asUInt
  def ins_regy() = io.ram.data_in(7 downto 4).asUInt
  def ins_n() = io.ram.data_in(3 downto 0).asUInt
  def ins_nn() = io.ram.data_in(7 downto 0).asUInt
  def ins_nnn() = (instruction_hi(3 downto 0) ## io.ram.data_in(7 downto 0)).asUInt

  val main_fsm = new StateMachine{
    io.ram.we := False
    io.vram.we := False

    val fetch_1 : State = new State{
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
        goto(wait_for_next_ins)
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
            goto(screen_draw)
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
                index_register := (registers(ins_regx)(3 downto 0)*5).resized
              }
              is(0x33) {
                reg_in_ins := ins_regx()
                goto(bcd)
              }
              is(0x55) {
                reg_in_ins := ins_regx()
                goto(reg_to_mem)
              }
              is(0x65) {
                reg_in_ins := ins_regx()
                goto(mem_to_reg)
              }
            }
          }
        }
      }
    }

    val bcd : State = new State {
      onEntry(current_reg := 0)
      whenIsActive {
        io.ram.address := index_register + current_reg
        io.ram.we := True
        io.ram.data_out := current_reg.mux(
          0 -> (registers(ins_regx)/100),
          1 -> ((registers(ins_regx)/10)%10),
          default -> (registers(ins_regx)%10)
        ).asBits
        current_reg := current_reg + 1
        when(current_reg === 2)(goto(wait_for_next_ins))
      }
    }

    val screen_clear : State = new State with EntryPoint{
      onEntry(screen_pos := 0)
      whenIsActive {
        io.vram.data_out := 0
        io.vram.address := screen_pos
        io.vram.we := True
        screen_pos := screen_pos + 1
        when(screen_pos === 2047) {
          goto(wait_for_next_ins)
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
        when(current_reg === reg_in_ins)(goto(wait_for_next_ins))
      }
    }

    val mem_to_reg = new StateFsm(fsm=mem_to_reg_fsm()){
      onEntry(current_reg := 0)
      whenCompleted{
        goto(wait_for_next_ins)
      }
    }

    val screen_draw = new StateFsm(fsm=screen_draw_fsm()){
      onEntry{
        registers(15) := 0
        pos_col := 0
      }
      whenCompleted{
        goto(wait_for_next_ins)
      }
    }

    val wait_for_next_ins : State = new State { //Delay to 500 instructions per second
      whenIsActive{
        when(ins_freq_divider === ClockDomain.current.frequency.getValue.toInt/700) {
          ins_freq_divider := 0
          goto(fetch_1)
        }
      }
    }
  }

  def mem_to_reg_fsm() = new StateMachine {
    val mem_to_reg_1 : State = new State with EntryPoint {
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
        goto(mem_to_reg_1)
        when(current_reg === reg_in_ins )(exit())
      }
    }
  }

  def screen_draw_fsm() = new StateMachine {
    val screen_draw_row : State = new State with EntryPoint {
      onEntry(pos_row := 0)
      whenIsActive {
        io.ram.address := index_register + pos_col
        goto(screen_draw_pixel_1)
        when(sprite_h === pos_col | screen_y + pos_col === 32)(exit())
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

  //Timer functions
  io.beep_en := beep_timer =/= 0
  freq_divider := freq_divider + 1
  ins_freq_divider := ins_freq_divider + 1
  when(freq_divider === ClockDomain.current.frequency.getValue.toInt/60){
    when(delay_timer =/= 0)(delay_timer := delay_timer - 1)
    when(beep_timer =/= 0)(beep_timer := beep_timer - 1)
    freq_divider := 0
  }

  //"""Random""" number generator
  random_number := random_number + 1
}

class Chip8Programmer extends Component {
  val io = new Bundle {
    val sd_int = SdInterface()

    val core_reset = RegInit(True).asOutput()

    val ram = RamInterface(8,12)
    val prog_sel = in UInt(8 bits)
    val prog_load = in Bool()
  }

  val font_table = Mem(Bits(8 bits), 80)
  BinTools.initRam(font_table,"./roms/font")

  val sd_spi = new sd_controller(24000000,63)
  io.sd_int <> sd_spi.io.sd_int
  sd_spi.io.card_write_prot := True
  sd_spi.io.card_present := True
  sd_spi.io.rd_multiple := False
  sd_spi.io.wr_multiple := False
  sd_spi.io.din_valid := False
  sd_spi.io.erase_count := 0
  sd_spi.io.wr := False
  sd_spi.io.din := 0

  val sd_addr = Reg(UInt(32 bits))
  val sd_dout_taken = Reg(Bool()) init(False)
  val sd_rd = Reg(Bool()) init(False)

  val current_addr = Reg(UInt(12 bits)) init(0)
  val byte_counter = Reg(UInt(9 bits))
  val block_number = Reg(UInt(12 bits))
  val block_counter = Reg(UInt(4 bits))

  val fsm = new StateMachine{
    val idle : State = new State {
      whenIsActive {
        when(io.prog_load === True)(goto(write_font))
      }
    }
    val write_font : State = new State with EntryPoint{
      onEntry{
        current_addr := 0
        io.core_reset := True
      }
      whenIsActive {
        io.ram.data_out := font_table(current_addr.resized)
        io.ram.address := current_addr
        io.ram.we := True
        current_addr := current_addr + 1
        when(current_addr === 79) {
          block_counter := 0
          block_number := (io.prog_sel * 7).resized
          current_addr := 0x200
          goto(write_prog_1)
        }
      }
    }
    val write_prog_1 : State = new State {
      whenIsActive {
        when(sd_spi.io.sd_busy === False) {
          byte_counter := 0
          sd_addr := block_number.resized
          sd_rd := True
          goto(write_prog_2)
        }
      }
    }
    val write_prog_2 : State = new State {
      whenIsActive {
        when(sd_spi.io.dout_avail === True) {
          io.ram.data_out := sd_spi.io.dout
          io.ram.address := current_addr
          current_addr := current_addr + 1
          sd_dout_taken := True
          goto(write_prog_3)
        }
      }
    }
    val write_prog_3 : State = new State {
      whenIsActive {
        when(sd_spi.io.dout_avail === False) {
          sd_dout_taken := False
          goto(write_prog_2)

          byte_counter := byte_counter + 1

          when(byte_counter === 511){ //End of block
            goto(write_prog_1)
            sd_rd := False
            block_number := block_number + 1
            block_counter := block_counter + 1
            when(block_counter === 6){ //End of row
              io.core_reset := False
              io.ram.we := False
              goto(idle)
            }
          }
        }
      }
    }
  }
  sd_spi.io.dout_taken <> sd_dout_taken
  sd_spi.io.addr <> sd_addr.asBits
  sd_spi.io.rd <> sd_rd
}

class Ps2Controller extends Component {
  val io = new Bundle {
    val ps2 = master(PS2Keyboard())

    val key_main = Reg(UInt(4 bits)).asOutput()
    val key_main_pressed = Reg(Bool()).asOutput()
    val key_control = Reg(Bits(4 bits)).asOutput()
  }

  val ps2_ctrl = new PS2KeyboardCtrl
  io.ps2 <> ps2_ctrl.io.ps2

  val prev_read_valid = RegNext(ps2_ctrl.io.read.valid)
  val break_flag = Reg(Bool()) init False
  val multi_flag = Reg(Bool()) init False
  val pressed_ps2_code = Reg(Bits(8 bits))

  def set_key(a: Int): Unit = {
    io.key_main := a
    io.key_main_pressed := True
  }

  val fsm = new StateMachine {
    io.key_control := 0
    val idle: State = new State with EntryPoint {
      whenIsActive {
        when(ps2_ctrl.io.read.valid & ~prev_read_valid){
          when(ps2_ctrl.io.read.payload === 0xF0){
            break_flag := True
          }.elsewhen(ps2_ctrl.io.read.payload === 0xE0){
            multi_flag := True
          }.otherwise{
            goto(decode)
          }
        }
      }
    }
    val decode: State = new State {
      whenIsActive {
        when(break_flag){
          when(ps2_ctrl.io.read.payload === pressed_ps2_code){
            io.key_main := 0
            io.key_main_pressed := False
          }
        }.otherwise{
          io.key_main := 0
          io.key_main_pressed := False
          pressed_ps2_code := ps2_ctrl.io.read.payload

          switch(ps2_ctrl.io.read.payload) {
            is(0x16)(set_key(0x1))
            is(0x1E)(set_key(0x2))
            is(0x26)(set_key(0x3))
            is(0x25)(set_key(0xC))
            is(0x15)(set_key(0x4))
            is(0x1D)(set_key(0x5))
            is(0x24)(set_key(0x6))
            is(0x2D)(set_key(0xD))
            is(0x1C)(set_key(0x7))
            is(0x1B)(set_key(0x8))
            is(0x23)(set_key(0x9))
            is(0x2B)(set_key(0xE))
            is(0x1A)(set_key(0xA))
            is(0x22)(set_key(0x0))
            is(0x21)(set_key(0xB))
            is(0x2A)(set_key(0xF))

            is(0x41)(io.key_control := 1)
            is(0x49)(io.key_control := 2)
            is(0x4B)(io.key_control := 4)
            is(0x42)(io.key_control := 8)
          }
        }
        goto(idle)
        break_flag := False
        multi_flag := False
      }
    }
  }
}

class Chip8TopLevel(rgb_config : RgbConfig, frequency: FixedFrequency) extends Component {
  val io = new Bundle {
    val clk = in Bool()
    val resetn = in Bool()

    val vga = master(Vga(rgb_config))
    val sd_int = SdInterface()
    val ps2 = master(PS2Keyboard())

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
    frequency = frequency
  )

  val mainArea = new ClockingArea(coreClockDomain) {
    //Memories
    val ram_main = Mem(Bits(8 bits),4096)
    val ram_video = Mem(Bits(1 bits),2048)

    //vga output
    val vga_timings = VgaTimings(12)
    vga_timings.setAs( //640x350
      640,
      96,
      16,
      48,
      false,
      350,
      2,
      37,
      60,
      false
    )
    val vga_ctrl = new Chip8VgaCtrl(rgb_config)
    vga_ctrl.io.timings := vga_timings
    vga_ctrl.io.vram_data_in := ram_video.readSync(vga_ctrl.io.vram_address)
    vga_ctrl.io.vga <> io.vga

    //Programmer
    val programmer = new Chip8Programmer
    io.sd_int <> programmer.io.sd_int
    ram_main.write(programmer.io.ram.address,programmer.io.ram.data_out,programmer.io.ram.we)

    //Core
    val coreArea = new ResetArea(programmer.io.core_reset, true) {
      //Chip8Core
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
    }

    //Ps2
    val ps2_ctrl = new Ps2Controller
    io.ps2 <> ps2_ctrl.io.ps2
    coreArea.core.io.key := ps2_ctrl.io.key_main
    coreArea.core.io.key_pressed := ps2_ctrl.io.key_main_pressed

    val prog_sel = Reg(UInt(8 bits)) init(0)
    val color_sel = Reg(UInt(3 bits)) init(7)
    when(ps2_ctrl.io.key_control(0))(prog_sel := prog_sel - 1)
    when(ps2_ctrl.io.key_control(1))(prog_sel := prog_sel + 1)
    programmer.io.prog_load := ps2_ctrl.io.key_control(2)
    when(ps2_ctrl.io.key_control(3)) {
      color_sel := color_sel + 1
      when(color_sel === 7)(color_sel := 1)
    }
    programmer.io.prog_sel := prog_sel
    vga_ctrl.io.color_sel := color_sel
  }
}

object Chip8Vhdl {
  def main(args: Array[String]) {
    SpinalVhdl(new Chip8TopLevel(RgbConfig(3,3,2),FixedFrequency(24 MHz)))
  }
}