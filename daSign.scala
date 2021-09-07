package dsp

// signed version of  Distributed Arithmetic

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._



case class daSign(xWidth: Int, N: Int, C: Array[BigInt]) extends Component {
    val cWidth = log2Up(C.map(_.abs).max) + 1
    val pWidth = xWidth + cWidth + log2Up(N)
    val io     = new Bundle {
        val x_in = Vec(in SInt (xWidth bits), N)
        val y    = out SInt (pWidth bits) setAsReg() init (0)
    }

    noIoPrefix()
    val x       = Vec(Reg(SInt(xWidth bits)) init (0), N)
    val tableIn = Bits(N bits)
    val tableOut = SInt(cWidth + log2Up(N)  bits)
    x.zipWithIndex.foreach { case (sig, index) => tableIn(index) := sig.lsb }
    val table = new Area {
        switch(tableIn.asUInt){
            for(n <- BigInt(0) until BigInt(2).pow(N)){
                var sum = BigInt(0)
                is(n){
                    for(i <- 0 until N){
                        if(n.testBit(i))
                            sum += C(i)
                    }
                    tableOut := sum
                }
            }
        }
    }
    val FSM = new StateMachine {
        val s0      = new State with EntryPoint
        val s1      = new State
        val counter = RegInit(U(0, log2Up(xWidth) + 1 bits))
        val p       = Reg(SInt(pWidth bits)) init (0)
        s0.whenIsActive {
            counter := 0
            p := 0
            x.zip(io.x_in).foreach { case (r, i) => r := i }
            goto(s1)
        }
        s1.whenIsActive {
            when(counter === xWidth) {
                io.y := p
                goto(s0)
            } otherwise {
                when(counter === xWidth - 1) {
                    p := (p >> 1).resize(pWidth) - (tableOut << (xWidth - 1)).resize(pWidth)
                }otherwise{
                    p := (p >> 1).resize(pWidth) + (tableOut << (xWidth - 1)).resize(pWidth)
                }
                x.foreach(sig => sig := sig |>> 1)
                counter := counter + 1
                goto(s1)
            }
        }
    }
}

object daSignRTL extends App{
    val cArr = Array[BigInt](-2, 3, 1)
    SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH)
    ).generateVerilog(new daSign(4, N = 3, C = cArr))
}

object daSignSim extends App{
    val cArr = Array[BigInt](-2, 3, 1)

    SimConfig.withWave.withConfig(SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency = FixedFrequency(100 MHz)
    )).compile(new daSign(xWidth = 4, N=3, C = cArr)).doSim { dut =>
        import dut._
        clockDomain.forkStimulus(10)
        for(i <- 0 until 50){
            io.x_in.randomize()
            clockDomain.waitSampling()
        }
    }
}