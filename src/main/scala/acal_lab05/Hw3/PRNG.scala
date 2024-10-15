package acal_lab05.Hw3

import chisel3._
import chisel3.util._

class PRNG(seed:Int) extends Module{
    val io = IO(new Bundle{
        val gen = Input(Bool())
        val puzzle = Output(Vec(4,UInt(4.W)))
        val ready = Output(Bool())
    })

    io.puzzle := VecInit(Seq.fill(4)(0.U(4.W)))
    io.ready := false.B

    // register to store the output, init to seed
    val shiftReg = RegInit(VecInit(seed.U(16.W).asBools))
    val posReg = RegInit(0.U(2.W))
    val checkDup = WireDefault(false.B)

    val sIdle :: sShift :: sCheck :: sSingleShift :: sOut :: Nil = Enum(5)
    val state = RegInit(sIdle)

    switch(state){
        is(sIdle){
            // switch to sShift when io.gen
            when(io.gen){
                state := sShift
            }
        }
        is(sShift){
            // shift the shiftReg by 1
            (shiftReg.zipWithIndex).map{
                case(sr, i) => sr := shiftReg((i + 1) % 16)
            }
            // xor
            shiftReg(15) := shiftReg(0) ^ shiftReg(2) ^ shiftReg(3) ^ shiftReg(5)

            // switch to sCheck to handle number > 9 and duplicate number
            state := sCheck
        }
        is(sCheck){
            // check if number > 9 or duplicate number
            // get current number
            val cur_number = Cat(
                shiftReg(4.U * posReg + 3.U),
                shiftReg(4.U * posReg + 2.U),
                shiftReg(4.U * posReg + 1.U),
                shiftReg(4.U * posReg)
            )
            // check duplicate number
            for(i <- 0 until 4){
                when(i.U < posReg){
                    val number = Cat(shiftReg.slice(4 * i, 4 * (i + 1)).reverse)
                    when(number === cur_number){
                        checkDup := true.B
                    }
                }
            }
            // switch to sSingleShift to shift single number
            when(checkDup || cur_number > 9.U){
                state := sSingleShift
            }.otherwise{
                // posReg record current number position
                // check next number
                // switch to sOut when all number check finished
                posReg := (posReg + 1.U) % 4.U
                when(posReg === 3.U){
                    state := sOut
                }
            }
        }
        is(sSingleShift){
            // get current number
            val cur_number = Cat(
                shiftReg(4.U * posReg + 3.U),
                shiftReg(4.U * posReg + 2.U),
                shiftReg(4.U * posReg + 1.U),
                shiftReg(4.U * posReg)
            )
            // if current number equal 0, set it to 1
            // otherwise shift the number using LFSR in lab5-3-1
            when(cur_number === 0.U){
                shiftReg(4.U * posReg + 1.U) := true.B
            }.otherwise{
                shiftReg(4.U * posReg) := shiftReg(4.U * posReg + 1.U)
                shiftReg(4.U * posReg + 1.U) := shiftReg(4.U * posReg + 2.U)
                shiftReg(4.U * posReg + 2.U) := shiftReg(4.U * posReg + 3.U)
                shiftReg(4.U * posReg + 3.U) := shiftReg(4.U * posReg) ^ shiftReg(4.U * posReg + 1.U)
            }

            // switch to sCheck to check again
            state := sCheck
        }
        is(sOut){
            // set io.puzzle, io.ready for output
            io.puzzle := VecInit(Seq.tabulate(4)(i => {
                Cat(shiftReg.slice(i*4, (i+1)*4).reverse)
            }))
            io.ready := true.B

            // switch back to sIdle for next gen
            state := sIdle
        }
    }
}