package acal_lab05.Hw3

import chisel3._
import chisel3.util._

class NumGuess(seed:Int = 1) extends Module{
    require (seed > 0 , "Seed cannot be 0")

    val io  = IO(new Bundle{
        val gen = Input(Bool())
        val guess = Input(UInt(16.W))
        val puzzle = Output(Vec(4,UInt(4.W)))
        val ready  = Output(Bool())
        val g_valid  = Output(Bool())
        val A      = Output(UInt(3.W))
        val B      = Output(UInt(3.W))

        //don't care at Hw6-3-2 but should be considered at Bonus
        val s_valid = Input(Bool())
    })

    // golden data register
    val goldReg = RegInit(VecInit(Seq.fill(4)(0.U(4.W))))
    // input register
    val inReg   = RegInit(VecInit(Seq.fill(4)(0.U(4.W))))
    val AReg = RegInit(0.U(3.W))
    val BReg = RegInit(0.U(3.W))
    val posReg = RegInit(0.U(2.W))

    io.puzzle := VecInit(Seq.fill(4)(0.U(4.W)))
    io.ready  := false.B
    io.g_valid  := false.B
    io.A      := AReg
    io.B      := BReg
    
    val sIdle :: sGen :: sInput :: sJudge :: sOut :: Nil = Enum(5)
    val state = RegInit(sIdle)

    // PRNG from HW5-3-1
    val prng = Module(new PRNG(seed = seed))
    prng.io.gen := false.B

    switch(state){
        is(sIdle){
            // let prng gen random number when io.gen
            // switch to sGen
            when(io.gen){
                prng.io.gen := true.B
                state := sGen
            }
        }
        is(sGen){
            // wait for prng.io.ready
            // set io.puzzle to prng.io.puzzle for testbench debug
            // set io.ready to true to get testbench input
            // record golden data
            // switch to sInput
            when(prng.io.ready){
                io.puzzle := prng.io.puzzle
                io.ready := true.B
                goldReg := prng.io.puzzle
                state := sInput
            }
        }
        is(sInput){
            // get testbench input
            for(i <- 0 until 4){
                val number = Cat(io.guess(4 * i + 3, 4 * i))
                inReg(i) := number
            }
            // reset AReg, BReg to 0
            // switch to sJudge
            AReg := 0.U
            BReg := 0.U
            state := sJudge
        }
        is(sJudge){
            // check 1 number in 1 cycle
            // posReg record current number position
            // switch to sOut when all number compare finished
            when(posReg === 3.U){
                state := sOut
            }
            posReg := (posReg + 1.U) % 4.U

            // compare each number in golden data and testbench input
            // A
            when(inReg(posReg) === goldReg(posReg)){
                AReg := AReg + 1.U
            }.otherwise{
                // B
                for(i <- 0 until 4){
                    when(inReg(posReg) === goldReg(i.U)){
                        BReg := BReg + 1.U
                    }
                }
            }
        }
        is(sOut){
            // set io.g_valid to true to let testbench get output and guess
            io.g_valid := true.B
            
            // switch to sIdle when testbench guess the correct answer
            // otherwise switch to sInput for testbench next input
            when(AReg === 4.U){
                state := sIdle
            }.otherwise{
                state := sInput
            }
        }
    }
}