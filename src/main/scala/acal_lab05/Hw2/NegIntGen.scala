package acal_lab05.Hw2

import chisel3._
import chisel3.util._

class NegIntGen extends Module{
    val io = IO(new Bundle{
        val key_in = Input(UInt(4.W))
        val value = Output(Valid(UInt(32.W)))
    })

    //please implement your code below
    val equal = WireDefault(false.B)
    equal := io.key_in === 15.U

    val sIdle :: sAccept :: sEqual :: Nil = Enum(3)
    val state = RegInit(sIdle)

    //Next State Decoder
    switch(state){
        is(sIdle){
            state := sAccept
        }
        is(sAccept){
            when(equal){
                state := sEqual
            }
        }
        is(sEqual){
            state := sAccept
        }
    }

    val in_buffer = RegNext(io.key_in)

    val number = RegInit(0.U(32.W))
    when(state === sAccept){
        when(in_buffer <= 9.U){
            // for number input
            number := (number<<3.U) + (number<<1.U) + in_buffer
        }.elsewhen(in_buffer === 14.U){
            // negative number start with "(-", end with ")"
            // set number to its 2's complement when get input ")"
            number := ~number + 1.U
        }
    }.elsewhen(state === sEqual){
        number := 0.U
    }.otherwise{
        number := number
    }

    io.value.valid := Mux(state === sEqual,true.B,false.B)
    io.value.bits := number
}