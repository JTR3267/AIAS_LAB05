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

    // record whether input is a negative number
    val negNum = RegInit(false.B)
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
            // reset negNum register to false in sEqual state
            negNum := false.B
            state := sAccept
        }
    }

    val in_buffer = RegNext(io.key_in)

    val number = RegInit(0.U(32.W))
    when(state === sAccept){
        when(in_buffer <= 9.U){
            // for number input
            number := (number<<3.U) + (number<<1.U) + in_buffer
        }.elsewhen(in_buffer < 15.U){
            // for input "(", ")", "-", set negNum register to true
            negNum := true.B
        }
    }.elsewhen(state === sEqual){
        number := 0.U
    }.otherwise{
        number := number
    }

    io.value.valid := Mux(state === sEqual,true.B,false.B)
    when(negNum){
        // if input is a negative number, assign output with number's 2's complement
        io.value.bits := ~number + 1.U
    }.otherwise{
        io.value.bits := number
    }
}