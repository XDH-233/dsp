package dsp

//  Distributed Arithmetic implementation


import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._

case class DA(N: Int, B: Int, C: Int, c: Array[BigInt]) extends Component {
    val io = new Bundle {
        val x_in  = Vec(in UInt (B bits), N)
        val valid = in Bool()
        val y     = out UInt (B + C + log2Up(N) - 1 bits) setAsReg() init (0)
        val ready = out Bool() setAsReg() init(False)
    }
    noIoPrefix()
    val x          = Vec(Reg(UInt(B bits)), N) simPublic()
    val bitCount = Reg(UInt(log2Up(N) bits)) init(0)


    val bitsVer = Bits(N bits)
    (x.map(_(bitCount)), Range(0, N)).zipped.foreach((bit, index) => bitsVer(index) := bit)

    val lut = new Area {
        val lutOut = UInt(C + log2Up(N) - 1 bits)
        switch(bitsVer.asUInt) {
            for (n <- BigInt(0) until BigInt(2).pow(N)) {
                is(n) {
                    var sum = BigInt(0)
                    for (i <- 0 until N) {
                        if (n.testBit(i)) {
                            sum = sum + c(i)
                        }
                    }
                    lutOut := sum
                }
            }
        }
    }

    io.ready := False
    when(io.valid) {
        bitCount.clearAll()
        io.y.clearAll()
        x := io.x_in
    }elsewhen(bitCount < N && !io.ready){
        bitCount := bitCount + 1
        io.y := io.y + (lut.lutOut << bitCount).resize(B + C + log2Up(N) - 1)
        when(bitCount === N - 1){
            io.ready.set()
        }
    }otherwise{
        bitCount := 0
        io.y := io.y
        io.ready.set()
    }
}



object DASim extends App{
    val c = Array[BigInt](1, 2, 3,4)
    implicit class DASimMethod(Dut: DA){
        def load = {
            Dut.io.valid #= true
            Dut.io.x_in.randomize()
            Dut.clockDomain.waitSampling()
            Dut.io.valid #= false
            val xSeq = Dut.io.x_in.map(_.toBigInt).toArray
            val exp = (xSeq, c).zipped.map((x, c) => x * c).reduce(_+_)
            Dut.clockDomain.waitSampling(Dut.B + 1)
            println("---------------------")
            println("x: " + xSeq.mkString(" "))
            println("c: " + c.mkString(" "))
            println("golden: " + exp)
            println("res: " + Dut.io.y.toBigInt)
            if(Dut.io.ready.toBoolean){
                println("Done!")
            }
            assert(exp == Dut.io.y.toBigInt)
            Dut.clockDomain.waitSampling()
        }
    }
    SimConfig.withWave.withConfig(SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency = FixedFrequency(100 MHz)
    )).compile(new DA(N = 4, B = 4, C = 4, c = c)).doSim { dut =>
        import dut._
        clockDomain.forkStimulus(10)
        io.valid #= false
        clockDomain.waitSampling(8)
        for(i <- 0 until 10){
            dut.load
        }
    }
}


case class lut(N: Int, C: Int, c: Array[BigInt]) extends Component {
    val io = new Bundle {
        val bitVertical = in Bits (N bits)
        val lutOut      = out UInt (C + log2Up(N) - 1 bits)
    }
    noIoPrefix()
    switch(io.bitVertical.asUInt) {
        for (n <- BigInt(0) until BigInt(2).pow(N)) {
            is(n) {
                var sum = BigInt(0)
                for (i <- 0 until N) {
                    if (n.testBit(i)) {
                        sum += c(i)
                    }
                }
                io.lutOut := sum
            }
        }
    }
}

object lutSim extends App {
    val cArr = Array[BigInt](1, 2, 3, 4)
    SimConfig.withWave.compile(new lut(N = 4, C = 8, c = cArr)).doSim { dut =>
        import dut._
        for (i <- 0 until 100) {
            io.bitVertical.randomize()
            sleep(1)
            val bitVer = io.bitVertical.toBigInt
            var sum    = BigInt(0)
            for (i <- 0 until N) {
                if (bitVer.testBit(i)) {
                    sum += cArr(i)
                }
            }
            println("-------------------------")
            println(bitVer.toString(2))
            println(sum)
            assert(sum == io.lutOut.toBigInt)
        }
    }
}
