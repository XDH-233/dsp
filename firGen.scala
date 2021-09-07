package dsp

/*
* programmed fir with  transposed structure
* pipelined
 */

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._

import util.control

case class firGen(width: Int, L: Int) extends Component {
    val io = new Bundle {
        val x_in   = in SInt (width bits)
        val c_in   = in SInt (width bits)
        val y_out  = out SInt (2 * width + log2Up(L) - 1 bits)
        val load_x = in Bool()
    }
    noIoPrefix()
    val c    = Vec(Reg(SInt(width bits)) init (0), L)
    val a    = Vec(Reg(SInt(2 * width + log2Up(L) - 1 bits)) init (0), L)
    val p    = Vec(SInt(2 * width bits), L)
    val load = {
        when(!io.load_x) {
            c(L - 1) := io.c_in
            for (i <- 0 until L - 1) {
                c(i) := c(i + 1)
            }
        }
    }

    val sum     = {
        a(L - 1) := p(L - 1).resized
        for (i <- 0 until L - 1) {
            a(i) := a(i + 1) + p(i)
        }
    }
    val product = {
        when(io.load_x) {
            p.zip(c).foreach{case(x, c) => c := c * io.x_in}//(i => i._1 := i._2 * io.x_in)
        } otherwise {
            p.foreach(_.clearAll())
        }
    }
    io.y_out := a(0)
}

object firGenSim extends App {
    implicit class simMeth(dut: firGen) {

        import dut._

        def loadCof(cof: Array[Int]) = {
            io.load_x #= false
            for (c <- cof) {
                io.c_in #= c
                clockDomain.waitSampling()
            }
        }

        def loadX(x: Array[Int]) = {
            io.load_x #= true
            for (x_in <- x) {
                io.x_in #= x_in
                clockDomain.waitSampling()
            }
            io.x_in #= 0
        }

        def fir(cof: Array[Int], x: Array[Int]): Array[Int] = {
            val res = Array.fill(cof.length + x.length - 1)(0)
            for (n <- 0 until res.length) {
                for (k <- 0 until cof.length) {
                    if (n - k >= 0 && n - k < x.length)
                        res(n) += cof(k) * x(n - k)
                }
            }
            return res
        }
    }

    SimConfig.withWave.withConfig(SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency = FixedFrequency(100 MHz)
    )).compile(new firGen(8, 4)).doSim { dut =>
        import dut._

        val cof = Array(1, 2, 3, 4) // f[0], f[1] ... f[L - 1]
        val x   = Array(1, 2, 3, 4, 5, 6) // x[0], x[1] ...
        val yG   = dut.fir(cof, x)
        clockDomain.forkStimulus(10000)
        dut.loadCof(cof)
        dut.loadX(x)
        clockDomain.waitSampling(100)
    }
}