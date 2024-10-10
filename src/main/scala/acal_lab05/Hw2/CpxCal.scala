package acal_lab05.Hw2

import chisel3._
import chisel3.util._

class CALstack(val depth: Int) extends Module {
    val io = IO(new Bundle {
        val push        = Input(Bool())
        val reset       = Input(Bool())
        val en          = Input(Bool())
        // whether input contains num
        val numIn       = Input(Bool())
        // 0 for num, 1, 2 for op
        val dataIn      = Input(Vec(3, UInt(32.W)))
        // count of valid dataIn
        val dataInCount = Input(UInt(2.W))
        val dataOut     = Output(UInt(32.W))
    })

    val stack_mem = Mem(depth, UInt(32.W))
    val sp        = RegInit(0.U(log2Ceil(depth+1).W))
    val out       = WireDefault(0.U(32.W))

    val add       = 10.U
    val sub       = 11.U
    val mul       = 12.U

    when(sp > 0.U){
        io.dataOut := stack_mem(sp - 1.U)
    }.otherwise{
        io.dataOut := stack_mem(0.U)
    }

    when (io.en) {
        when(io.push) {
            switch(io.dataInCount){
                is(1.U){
                    when(io.numIn){
                        // push 1 num to stack
                        // update stack, out
                        stack_mem(sp) := io.dataIn(0.U)
                        sp := sp + 1.U
                        io.dataOut := io.dataIn(0.U)
                    }.otherwise{
                        // compute stack_mem(sp - 1.U), stack_mem(sp - 2.U) with op io.dataIn(1.U)
                        out := MuxLookup(io.dataIn(1.U),0.U,Seq(
                            add -> (stack_mem(sp - 1.U) + stack_mem(sp - 2.U)),
                            sub -> (stack_mem(sp - 2.U) - stack_mem(sp - 1.U)),
                            mul -> (stack_mem(sp - 1.U) * stack_mem(sp - 2.U))
                        ))
                        // update stack, out
                        stack_mem(sp - 2.U) := out
                        sp := sp - 1.U
                        io.dataOut := out
                    }
                }
                is(2.U){
                    when(io.numIn){
                        // compute stack_mem(sp - 1.U), io.dataIn(0.U) with op io.dataIn(1.U)
                        out := MuxLookup(io.dataIn(1.U),0.U,Seq(
                            add -> (stack_mem(sp - 1.U) + io.dataIn(0.U)),
                            sub -> (stack_mem(sp - 1.U) - io.dataIn(0.U)),
                            mul -> (stack_mem(sp - 1.U) * io.dataIn(0.U))
                        ))
                        // update stack, out
                        stack_mem(sp - 1.U) := out
                        io.dataOut := out
                    }.otherwise{
                        // compute stack_mem(sp - 1.U), stack_mem(sp - 2.U) with op io.dataIn(1.U)
                        // then compute previous result, stack_mem(sp - 3.U) with op io.dataIn(2.U)
                        when(io.dataIn(1.U) === add && io.dataIn(2.U) === sub){
                            out := stack_mem(sp - 3.U) - (stack_mem(sp - 1.U) + stack_mem(sp - 2.U))
                        }.elsewhen(io.dataIn(1.U) === add && io.dataIn(2.U) === mul){
                            out := (stack_mem(sp - 1.U) + stack_mem(sp - 2.U)) * stack_mem(sp - 3.U)
                        }.elsewhen(io.dataIn(1.U) === sub && io.dataIn(2.U) === add){
                            out := stack_mem(sp - 2.U) - stack_mem(sp - 1.U) + stack_mem(sp - 3.U)
                        }.elsewhen(io.dataIn(1.U) === sub && io.dataIn(2.U) === mul){
                            out := (stack_mem(sp - 2.U) - stack_mem(sp - 1.U)) * stack_mem(sp - 3.U)
                        }.elsewhen(io.dataIn(1.U) === mul && io.dataIn(2.U) === add){
                            out := stack_mem(sp - 1.U) * stack_mem(sp - 2.U) + stack_mem(sp - 3.U)
                        }.elsewhen(io.dataIn(1.U) === mul && io.dataIn(2.U) === sub){
                            out := stack_mem(sp - 3.U) - (stack_mem(sp - 1.U) * stack_mem(sp - 2.U))
                        }
                        // update stack, out
                        stack_mem(sp - 3.U) := out
                        sp := sp - 2.U
                        io.dataOut := out
                    }
                }
                is(3.U){
                    // compute stack_mem(sp - 1.U), io.dataIn(0.U) with op io.dataIn(1.U)
                    // then compute previous result, stack_mem(sp - 2.U) with op io.dataIn(2.U)
                    when(io.dataIn(1.U) === add && io.dataIn(2.U) === sub){
                        out := stack_mem(sp - 2.U) - (stack_mem(sp - 1.U) + io.dataIn(0.U))
                    }.elsewhen(io.dataIn(1.U) === add && io.dataIn(2.U) === mul){
                        out := (stack_mem(sp - 1.U) + io.dataIn(0.U)) * stack_mem(sp - 2.U)
                    }.elsewhen(io.dataIn(1.U) === sub && io.dataIn(2.U) === add){
                        out := stack_mem(sp - 1.U) - io.dataIn(0.U) + stack_mem(sp - 2.U)
                    }.elsewhen(io.dataIn(1.U) === sub && io.dataIn(2.U) === mul){
                        out := (stack_mem(sp - 1.U) - io.dataIn(0.U)) * stack_mem(sp - 2.U)
                    }.elsewhen(io.dataIn(1.U) === mul && io.dataIn(2.U) === add){
                        out := stack_mem(sp - 1.U) * io.dataIn(0.U) + stack_mem(sp - 2.U)
                    }.elsewhen(io.dataIn(1.U) === mul && io.dataIn(2.U) === sub){
                        out := stack_mem(sp - 2.U) - (stack_mem(sp - 1.U) * io.dataIn(0.U))
                    }
                    // update stack, out
                    stack_mem(sp - 2.U) := out
                    sp := sp - 1.U
                    io.dataOut := out
                }
            }
        }
    }
    when (io.reset) {
        // reset sp to 0
        sp := 0.U
    }
}

class OPstack(val depth: Int) extends Module {
    val io = IO(new Bundle {
        val push         = Input(Bool())
        val pop          = Input(Bool())
        val en           = Input(Bool())
        val dataIn       = Input(UInt(4.W))
        // store op poped from stack, at most 2
        val dataOut      = Output(Vec(2, UInt(4.W)))
        // count for valid dataOut
        val dataOutCount = Output(UInt(2.W))
    })

    val stack_mem = Mem(depth, UInt(4.W))
    val sp        = RegInit(0.U(log2Ceil(depth+1).W))
    val pos       = WireDefault(0.U(log2Ceil(depth+1).W))

    for (i <- 0 until depth){
        io.dataOut(i.U) := 0.U
    }
    io.dataOutCount := 0.U

    when (io.en) {
        when(io.push) {
            when(io.dataIn === 10.U || io.dataIn === 11.U) {
                // op +-
                for (i <- 0 until depth){
                    // find last symbol (
                    when(i.U < sp && stack_mem(i.U) === 13.U){
                        pos := i.U
                    }
                }
                when(pos =/= 0.U){
                    // if found, pop all ops after symbol (
                    for (i <- 0 until depth){
                        when(i.U < sp - pos - 1.U){
                            io.dataOut(i.U) := stack_mem(sp - 1.U - i.U)
                        }
                    }
                    io.dataOutCount := sp - pos - 1.U
                    stack_mem(pos + 1.U) := io.dataIn
                    sp := pos + 2.U
                }.elsewhen(sp > 0.U){
                    // if not found, but sp > 0, pop all ops
                    when(stack_mem(0.U) === 13.U){
                        // check stack_mem(0) may be symbol (
                        for (i <- 1 until depth){
                            when(i.U < sp){
                                io.dataOut(i.U) := stack_mem(sp - 1.U - i.U)
                            }
                        }
                        io.dataOutCount := sp - 1.U
                        stack_mem(1.U) := io.dataIn
                        sp := 2.U
                    }.otherwise{
                        for (i <- 0 until depth){
                            when(i.U < sp){
                                io.dataOut(i.U) := stack_mem(sp - 1.U - i.U)
                            }
                        }
                        io.dataOutCount := sp
                        stack_mem(0.U) := io.dataIn
                        sp := 1.U
                    }
                }.otherwise{
                    // else, insert op
                    stack_mem(sp) := io.dataIn
                    sp := sp + 1.U
                }
            }.elsewhen(io.dataIn === 12.U){
                // op *
                when(sp > 0.U && stack_mem(sp - 1.U) === 12.U){
                    // pop and push op * to stack
                    io.dataOut(0.U)   := 12.U
                    io.dataOutCount := 1.U
                }.otherwise{
                    // insert op *
                    stack_mem(sp) := io.dataIn
                    sp := sp + 1.U
                }
            }.elsewhen(io.dataIn === 13.U){
                // symbol (, insert only
                stack_mem(sp) := io.dataIn
                sp := sp + 1.U
            }.otherwise{
                // symbol )
                for (i <- 0 until depth){
                    // find last symbol (
                    when(i.U < sp && stack_mem(i.U) === 13.U){
                        pos := i.U
                    }
                }
                // pop all ops after symbol (
                for (i <- 0 until depth){
                    when(i.U < sp - pos - 1.U){
                        io.dataOut(i.U) := stack_mem(sp - 1.U - i.U)
                    }
                }
                io.dataOutCount := sp - pos - 1.U
                sp := pos
            }
        } .elsewhen(io.pop) {
            // pop all ops from stack, triggered when =
            io.dataOutCount := sp
            for (i <- 0 until depth){
                when(i.U < sp){
                    io.dataOut(i.U) := stack_mem(sp - 1.U - i.U)
                }
            }
            // reset stack
            sp := 0.U
        }
    }
}

class CpxCal extends Module{
    val io = IO(new Bundle{
        val key_in = Input(UInt(4.W))
        val value = Output(Valid(UInt(32.W)))
    })

    //please implement your code below
    //Wire Declaration===================================
    val operator = WireDefault(false.B)
    val num = WireDefault(false.B)
    val equal = WireDefault(false.B)
    // check "(" input
    val leftP = WireDefault(false.B)
    // check ")" input
    val rightP = WireDefault(false.B)
    
    operator := io.key_in >= 10.U && io.key_in <= 12.U
    num := io.key_in < 10.U
    equal := io.key_in === 15.U
    leftP := io.key_in === 13.U
    rightP := io.key_in === 14.U

    //Reg Declaration====================================
    val in_buffer = RegNext(io.key_in)
    val number = RegInit(0.U(32.W))
    // check input number is negative
    val neg_num = RegInit(false.B)
    // whether need to push number to cal_stack
    val pass_number = RegInit(false.B)

    // OP stack
    val op_stack = Module(new OPstack(depth = 8))
    op_stack.io.push   := false.B
    op_stack.io.pop    := false.B
    op_stack.io.en     := false.B
    op_stack.io.dataIn := 0.U

    // Cal stack
    val cal_stack = Module(new CALstack(depth = 8))
    cal_stack.io.push   := false.B
    cal_stack.io.reset  := false.B
    cal_stack.io.en     := false.B
    cal_stack.io.numIn  := false.B
    // default set dataIn(0) to number, dataIn(1), dataIn(2), to op_stack dataOut(0), dataOut(1)
    cal_stack.io.dataIn(0.U) := number
    cal_stack.io.dataIn(1.U) := op_stack.io.dataOut(0.U)
    cal_stack.io.dataIn(2.U) := op_stack.io.dataOut(1.U)
    cal_stack.io.dataInCount := 0.U
    
    //State and Constant Declaration=====================
    // sLeft for state (
    // sRight for state )
    val sIdle :: sNum :: sOp :: sLeft :: sRight :: sEqual :: Nil = Enum(6)

    //Next State Decoder
    val state = RegInit(sIdle)
    switch(state){
        is(sIdle){
            when(num){
                state := sNum
            }.elsewhen(leftP){
                state := sLeft
            }
        }
        is(sNum){
            // when switch state from sNum, set pass_number to true to push number to cal_stack
            when(operator){
                state := sOp
                pass_number := true.B
            }.elsewhen(equal){
                state := sEqual
                pass_number := true.B
            }.elsewhen(rightP){
                state := sRight
                pass_number := true.B
            }
        }
        is(sOp){
            when(num){
                state := sNum
            }.elsewhen(leftP){
                state := sLeft
            }
        }
        is(sLeft){
            when(!leftP){
                state := sNum
            }
        }
        is(sRight){
            when(equal){
                state := sEqual
            }.elsewhen(operator){
                state := sOp
            }
        }
        is(sEqual){
            state := sIdle
        }
    }

    when(state === sNum){
        when(in_buffer <= 9.U){
            // for number input
            pass_number := true.B
            number := (number<<3.U) + (number<<1.U) + in_buffer
        }.elsewhen(in_buffer === 11.U){
            // input number is negative
            neg_num := true.B
        }
    }
    when(state === sOp || state === sRight){
        // push op, symbol ) to op_stack
        op_stack.io.push := true.B
        op_stack.io.en := true.B
        op_stack.io.dataIn := in_buffer

        // push number, output op to cal_stack
        cal_stack.io.push := true.B
        cal_stack.io.en := true.B
        
        when(pass_number){
            // push number, op
            when(neg_num){
                cal_stack.io.dataIn(0.U) := ~number + 1.U
                neg_num := false.B
            }
            cal_stack.io.numIn := true.B
            cal_stack.io.dataInCount := op_stack.io.dataOutCount + 1.U

            number := 0.U
            pass_number := false.B
        }
        .otherwise{
            // push op only
            cal_stack.io.dataInCount := op_stack.io.dataOutCount
        }
    }
    when(state === sLeft){
        // push symbol ( to op_stack
        op_stack.io.push := true.B
        op_stack.io.en := true.B
        op_stack.io.dataIn := in_buffer
    }
    when(state === sEqual){
        // pop rest op from op_stack
        op_stack.io.pop := true.B
        op_stack.io.en := true.B

        // push number, output op to cal_stack
        cal_stack.io.push := true.B
        cal_stack.io.reset := true.B
        cal_stack.io.en := true.B

        when(pass_number){
            // push number, op
            when(neg_num){
                cal_stack.io.dataIn(0.U) := ~number + 1.U
                neg_num := false.B
            }
            cal_stack.io.numIn := true.B
            cal_stack.io.dataInCount := op_stack.io.dataOutCount + 1.U

            number := 0.U
            pass_number := false.B
        }.otherwise{
            // push op only
            cal_stack.io.dataInCount := op_stack.io.dataOutCount
        }
    }

    io.value.valid := Mux(state === sEqual,true.B,false.B)
    io.value.bits := cal_stack.io.dataOut
}