package dsp

// simulation support for SFix

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._


object SFixSimSupport {
    implicit class simMethod(s: SFix) {
        def toDouble: Double = s.raw.toBigInt.toDouble / (BigInt(2).pow(-s.minExp).toDouble)

        def #=(value: Double) = {
            require(value <= s.maxValue && value >= s.minValue)
            s.raw #= (value * BigInt(2).pow(-s.minExp).toDouble).toLong
        }
    }
}