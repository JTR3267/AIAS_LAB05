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

    // register to store the output, init to 1
    val shiftReg = RegInit(VecInit(0x1.U(16.W).asBools))

    val sIdle :: sShift :: sDown :: sCheck :: sOut :: Nil = Enum(5)
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

            // switch to sDown to handle number > 9
            state := sDown
        }
        is(sDown){
            for (i <- 0 until 4){
                // get each 4 bits number
                val slice = shiftReg.slice(4 * i, 4 * (i + 1))
                // if number > 9, number = number - 8
                when(Cat(slice.reverse) > 9.U){
                    shiftReg((4 * i + 3)) := false.B
                }
            }

            // switch to sCheck to check duplicate number
            state := sCheck
        }
        is(sCheck){
            // check if duplicate number exist
            // not exeist -> switch to sOut to output
            // exist -> switch to sShift to shift again
            when((Cat(shiftReg.slice(0, 4).reverse) =/= Cat(shiftReg.slice(4, 8).reverse)) && 
                 (Cat(shiftReg.slice(0, 4).reverse) =/= Cat(shiftReg.slice(8, 12).reverse)) &&
                 (Cat(shiftReg.slice(0, 4).reverse) =/= Cat(shiftReg.slice(12, 16).reverse)) &&
                 (Cat(shiftReg.slice(4, 8).reverse) =/= Cat(shiftReg.slice(8, 12).reverse)) &&
                 (Cat(shiftReg.slice(4, 8).reverse) =/= Cat(shiftReg.slice(12, 16).reverse)) &&
                 (Cat(shiftReg.slice(8, 12).reverse) =/= Cat(shiftReg.slice(12, 16).reverse))){
                state := sOut
            }.otherwise{
                state := sShift
            }
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