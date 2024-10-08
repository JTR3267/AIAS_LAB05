package acal_lab05.Hw1

import chisel3._
import chisel3.util._

class TrafficLight_p(Ytime:Int, Gtime:Int, Ptime:Int) extends Module{
  val io = IO(new Bundle{
    val P_button = Input(Bool())
    val H_traffic = Output(UInt(2.W))
    val V_traffic = Output(UInt(2.W))
    val P_traffic = Output(UInt(2.W))
    val timer     = Output(UInt(5.W))
  })

  //please implement your code below...

  //parameter declaration
  val Off = 0.U
  val Red = 1.U
  val Yellow = 2.U
  val Green = 3.U

  val sIdle :: sHGVR :: sHYVR :: sHRVG :: sHRVY :: sPG :: Nil = Enum(6)
  val state = RegInit(sIdle)
  // record previous state when P_button trigger
  val prevState = RegInit(sIdle)
  // record whether sPG state triggered by P_button
  val buttonTrigger = RegInit(false.B)

  //Counter============================
  val cntMode = WireDefault(0.U(2.W))
  val cntReg = RegInit(0.U(4.W))
  val cntDone = Wire(Bool())
  cntDone := cntReg === 0.U

  when(cntDone){
    when(cntMode === 0.U){
      // 綠燈
      cntReg := (Gtime-1).U
    }.elsewhen(cntMode === 1.U){
      // 黃燈
      cntReg := (Ytime-1).U
    }.otherwise{
      // 行人
      cntReg := (Ptime-1).U
    }
  }.otherwise{
    cntReg := cntReg - 1.U
  }
  //Counter end========================

  //Next State Decoder
  switch(state){
    is(sIdle){
      state := sHGVR
    }
    is(sHGVR){
      when(io.P_button){
        // record current state, buttonTrigger, change state to sPG
        buttonTrigger := true.B
        prevState := sHGVR
        state := sPG
      }.elsewhen(cntDone){
        state := sHYVR
      }
    }
    is(sHYVR){
      when(io.P_button){
        // record current state, buttonTrigger, change state to sPG
        buttonTrigger := true.B
        prevState := sHYVR
        state := sPG
      }.elsewhen(cntDone){
        state := sHRVG
      }
    }
    is(sHRVG){
      when(io.P_button){
        // record current state, buttonTrigger, change state to sPG
        buttonTrigger := true.B
        prevState := sHRVG
        state := sPG
      }.elsewhen(cntDone){
        state := sHRVY
      }
    }
    is(sHRVY){
      when(io.P_button){
        // record current state, buttonTrigger, change state to sPG
        buttonTrigger := true.B
        prevState := sHRVY
        state := sPG
      }.elsewhen(cntDone){
        state := sPG
      }
    }
    is(sPG){
      when(cntDone){
        // if triggered by P_button
        when(buttonTrigger){
          // switch back to previous state
          state := prevState
          // set buttonTrigger to false
          buttonTrigger := false.B
        }.otherwise{
          state := sHGVR
        }
      }
    }
  }

  //Output Decoder
  //Default statement
  cntMode := 0.U
  io.H_traffic := Off
  io.V_traffic := Off
  io.P_traffic := Off

  switch(state){
    is(sHGVR){
      when(io.P_button){
        // end current time counter, switch cntMode to 2
        cntDone := true.B
        cntMode := 2.U
      }.otherwise{
        cntMode := 1.U
      }
      io.H_traffic := Green
      io.V_traffic := Red
      io.P_traffic := Red
    }
    is(sHYVR){
      when(io.P_button){
        // end current time counter, switch cntMode to 2
        cntDone := true.B
        cntMode := 2.U
      }.otherwise{
        cntMode := 0.U
      }
      io.H_traffic := Yellow
      io.V_traffic := Red
      io.P_traffic := Red
    }
    is(sHRVG){
      when(io.P_button){
        // end current time counter, switch cntMode to 2
        cntDone := true.B
        cntMode := 2.U
      }.otherwise{
        cntMode := 1.U
      }
      io.H_traffic := Red
      io.V_traffic := Green
      io.P_traffic := Red
    }
    is(sHRVY){
      when(io.P_button){
        // end current time counter
        cntDone := true.B
      }
      cntMode := 2.U
      io.H_traffic := Red
      io.V_traffic := Yellow
      io.P_traffic := Red
    }
    is(sPG){
      when(buttonTrigger){
        // hold previous state's cntMode
        switch(prevState){
          is(sHGVR){
            cntMode := 0.U
          }
          is(sHYVR){
            cntMode := 1.U
          }
          is(sHRVG){
            cntMode := 0.U
          }
          is(sHRVY){
            cntMode := 1.U
          }
        }
      }.otherwise{
        cntMode := 0.U
      }
      io.H_traffic := Red
      io.V_traffic := Red
      io.P_traffic := Green
    }
  }
  
  io.timer := cntReg
}