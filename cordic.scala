package dsp
/*
* pipeline construction cordic
* can calculation COS/SIN and ARCTAN in circle rotation mode
* use signed fixed number
 */


import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import scala.math._


object Functions extends Enumeration {
    type Functions = Value
    val COS_SIN, ARCTAN = Value
}

object cordicMath {
    def main(args: Array[String]): Unit = {
        println(Theta(8).mkString("\n"))
    }

    def Theta(n: Int) = Range(0, n).map(i => atan(1.0 / pow(2.0, i.toDouble)))

    def K(n: Int) = Theta(n).map(1 / cos(_)).reduce(_ * _)
}


case class cordic(iteration: Int = 8, Peak: Int = 10,  funct: Functions.Value = Functions.COS_SIN) extends Component {

    import Functions._
    import cordicMath._
    val Resolution = iteration + 1
    val z_in   = if (funct == COS_SIN) in SFix(peak = 2 exp, resolution = -Resolution exp) else null
    val y_in   = if (funct == ARCTAN) in SFix(peak = Peak exp, resolution = -Resolution exp) simPublic() else null
    val x, y   = if (funct == COS_SIN) Vec(Reg(SFix(peak = 1 exp, resolution = -Resolution exp)) init (0), iteration) simPublic() else Vec(Reg(SFix(peak = Peak exp, resolution = -Resolution exp)) init (0), iteration) simPublic()
    val z      = Vec(Reg(SFix(peak = 2 exp, resolution = -Resolution exp)) init (0), iteration) simPublic()
    val r, eps = if (funct == COS_SIN) out SFix(peak = 1 exp, resolution = -Resolution exp) else out SFix(peak = Peak exp, resolution = -Resolution exp)
    val phi    = out SFix(peak = 2 exp, resolution = -Resolution exp)

    val theta = Vec(SFix(peak = 2 exp, resolution = -Resolution exp), iteration) simPublic()
    theta.zip(Theta(iteration)).foreach(t => t._1 := t._2)

    val di = Vec(Bool(), iteration - 1) simPublic()
    funct match {
        case COS_SIN => {
            x(0) := 1 / K(iteration)
            y(0) := 0.0
            z(0) := z_in
        }
        case ARCTAN => {
            x(0) := 1.0
            z(0) := 0.0
            y(0) := y_in
        }
    }
    for (i <- 0 until iteration - 1) {
        funct match {
            case COS_SIN => di(i) := z(i) >= 0
            case ARCTAN => di(i) := !(y(i) >= 0)
        }
        when(di(i)) {
            x(i + 1) := x(i) - (y(i) >>| i)
            y(i + 1) := y(i) + (x(i) >>| i)
            z(i + 1) := z(i) - theta(i)
        } otherwise {
            x(i + 1) := x(i) + (y(i) >>| i)
            y(i + 1) := y(i) - (x(i) >>| i)
            z(i + 1) := z(i) + theta(i)
        }
    }
    r   := x.last
    eps := y.last
    phi := z.last
}




object cordicRTL extends App {
    SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH)
    ).generateVerilog(new cordic())
}

object cordicSim extends App {

    import SFixSimSupport._
    import Functions._

    SimConfig.withWave.withConfig(SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency = FixedFrequency(100 MHz)
    )).compile(new cordic( iteration = 7, funct = ARCTAN)).doSim { dut =>
        import dut._
        print("theta: ")
        theta.map(_.toDouble).foreach(printf("%6f ", _))
        println("")
        print("tan(theta): ")
        theta.map(_.toDouble).map(tan(_)).foreach(printf("%6f ", _))
        println("")
        clockDomain.forkStimulus(10)
        y_in #= 1
        clockDomain.waitSampling()
        for (i <- 0 until 20) {
            if (i == 0) {
                y_in #= 2
            } else {
                y_in #= scala.util.Random.nextDouble() * 10
            }
            clockDomain.waitSampling()
            println(s"--------------------------------------${i}---------------------------------------")
            println("z_in: " + y_in.toDouble)
            print("x:  ")
            x.map(_.toDouble).foreach(printf("%12.6f ", _))
            println("")
            print("y:  ")
            y.map(_.toDouble).foreach(printf("%12.6f ", _))
            println("")
            print("z:  ")
            z.map(_.toDouble).foreach(printf("%12.6f ", _))
            println("")
            print("di: ")
            di.map(_.toBigInt).foreach(printf("%12d ", _))
            println("")
            print("ta: ")
            theta.map(_.toDouble).foreach(printf("%12.6f ", _))
            println("")
            println("cos: " + r.toDouble)
            println("sin: " + eps.toDouble)
        }
    }
}