package acal_lab05.Hw2

import chisel3._
import chisel3.util._

class LongCal extends Module{
    val io = IO(new Bundle{
        val key_in = Input(UInt(4.W))
        val value = Output(Valid(UInt(32.W)))
    })

    //please implement your code below
    //Wire Declaration===================================
    val operator = WireDefault(false.B)
    val num = WireDefault(false.B)
    val equal = WireDefault(false.B)
    // check "(", ")" input
    val parentheses = WireDefault(false.B)
    val out = Wire(UInt(32.W))
    operator := io.key_in >= 10.U && io.key_in <= 11.U
    num := io.key_in < 10.U
    equal := io.key_in === 15.U
    parentheses := io.key_in >= 13.U && io.key_in <= 14.U

    //Reg Declaration====================================
    val in_buffer = RegNext(io.key_in)
    val src1 = RegInit(0.U(32.W))
    val op = RegInit(0.U(1.W))
    val src2 = RegInit(0.U(32.W))
    
    //State and Constant Declaration=====================
    val sIdle :: sSrc1 :: sOp :: sSrc2 :: sEqual :: Nil = Enum(5)
    val add = 0.U
    val sub = 1.U
    
    //Next State Decoder
    val state = RegInit(sIdle)
    switch(state){
        is(sIdle){
            state := sSrc1
        }
        is(sSrc1){
            when(operator && in_buffer =/= 13.U){
                // switch to sOP state only if input before operator input is not a "("
                state := sOp
            }.elsewhen(equal){
                state := sEqual
            }
        }
        is(sOp){
            when(num || parentheses){
                state := sSrc2
            }
        }
        is(sSrc2){
            when(equal){
                state := sEqual
            }.elsewhen(operator && in_buffer =/= 13.U){
                // switch to sOP state only if input before operator input is not a "("
                state := sOp
            }
        }
        is(sEqual){
            state := sSrc1
        }
    }
    //==================================================

    when(state === sSrc1){
        when(in_buffer <= 9.U){
            // for number input
            src1 := (src1<<3.U) + (src1<<1.U) + in_buffer
        }.elsewhen(in_buffer === 14.U){
            // for negative number
            src1 := ~src1 + 1.U
        }
    }
    when(state === sSrc2){
        when(in_buffer <= 9.U){
            // for number input
            src2 := (src2<<3.U) + (src2<<1.U) + in_buffer
        }.elsewhen(in_buffer === 14.U){
            // for negative number
            src2 := ~src2 + 1.U
        }
    }
    when(state === sOp){
        // put temp result in src1, set src2 to 0.U for next input number
        src1 := out
        src2 := 0.U
        op := in_buffer - 10.U
    }
    when(state === sEqual){
        src1 := 0.U
        src2 := 0.U
        op := 0.U
    }

    io.value.valid := Mux(state === sEqual,true.B,false.B)
    // compute output
    out := MuxLookup(op,0.U,Seq(
        add -> (src1 + src2),
        sub -> (src1 - src2),
    ))
    io.value.bits := out
}