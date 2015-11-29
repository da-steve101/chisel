 /*
 Copyright (c) 2011, 2012, 2013, 2014 The University of Sydney.
 All Rights Reserved.  Redistribution and use in source and 
 binary forms, with or without modification, are permitted
 provided that the following conditions are met:

    * Redistributions of source code must retain the above
      copyright notice, this list of conditions and the following
      two paragraphs of disclaimer.
    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      two paragraphs of disclaimer in the documentation and/or other materials
      provided with the distribution.
    * Neither the name of the Regents nor the names of its contributors
      may be used to endorse or promote products derived from this
      software without specific prior written permission.

 IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
 SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
 ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
 REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

 REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 A PARTICULAR PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF
 ANY, PROVIDED HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION
 TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
 MODIFICATIONS.
*/

package Chisel

import Node._
import ChiselError._

import scala.collection.mutable.ArrayBuffer

object LSDFHelper {
    /* Helper Functions and Objects */    
  def getStages(totalWidth : Int, digit : Int) = scala.math.round(totalWidth.toDouble/digit.toDouble).toInt

  def newExample(count : UInt, stages : Int) : Bool = Mux(count === UInt(stages - 1), Bool(true), Bool(false))

  def wrapAround(n : UInt, max : UInt) = Mux(n > max, UInt(0), n)

  def counter(max : UInt, amt : UInt, init : Int = 0) : UInt = {
    val x = Reg(init = UInt(init, max.getWidth()))
    x := wrapAround(x + amt, max)
    x
  }

}

object LSDFAdd {
  def apply(a : UInt, b : UInt, stages : Int, delay : Int) : UInt = {
    val countInit = if(delay ==0) 0 else stages - delay
    val count = LSDFHelper.counter(UInt(stages - 1), UInt(1), countInit)
    val carry = Reg(init=UInt(0))
    val (res, cout) = RippleCarryAdder(a, b, carry)
    carry := Mux(LSDFHelper.newExample(count, stages), UInt(0), cout)
    res
  }
}

object LSDFSub {
  def apply(a : UInt, b : UInt, stages : Int, delay : Int) : UInt = {
    val countInit = if(delay ==0) 0 else stages - delay
    val count = LSDFHelper.counter(UInt(stages - 1), UInt(1), countInit)
    val carry = Reg(init=UInt(1))
    val (res, cout) = RippleCarryAdder(a, ~b, carry)
    carry := Mux(LSDFHelper.newExample(count, stages), UInt(1), cout)
    res
  }
} 

object LSDFMul {
  def apply(a : UInt, b : UInt, stages : Int, delay : Int) : UInt = {
    val digitWidth = a.getWidth()
    // Result
    val aReg = Reg(init=UInt(0, width=digitWidth*stages))
    val bReg = Reg(init=UInt(0, width=digitWidth*stages))
    val cReg = Vec.fill(digitWidth*stages){Reg(init=UInt(0, width=1))}

    // Stage LSDFHelper.Counter
    val countInit = if(delay ==0) 0 else stages - delay
    val count = LSDFHelper.counter(UInt(stages - 1), UInt(1), countInit)

    // Digit LSDFHelper.Counter
    val digitInit = if(delay == 0) 0 else digitWidth*stages - delay*digitWidth
    val digitCount = LSDFHelper.counter(UInt(digitWidth*(stages - 1)), UInt(digitWidth), digitInit)

    val workingB = Vec(b.toBools.reverse).toBits.toUInt
    // New Values for a and b
    val newA = (a << digitCount) | aReg
    val newB = (bReg << digitWidth) | workingB


    // Calculate the Multiplication
    val aAndB = Vec.fill(digitWidth){Vec.fill(digitWidth*stages){UInt(width=1)}}

    // Need to Create aAndB
    for (i <- 0 until digitWidth) {
      for (j <- 0 until digitWidth*stages) {
        val bIndex = j + i
        val theB = if (bIndex >= digitWidth*stages) {
          UInt(0)
        } else {
          newB(bIndex)
        }
        aAndB(digitWidth - 1 - i)(digitWidth*stages - 1 - j) := theB & newA(j)
      }
    }

    // Main Bulk of the work
    val result = new ArrayBuffer[UInt]
    val carry = Vec.fill(digitWidth + 1){Vec.fill(digitWidth*stages){UInt(width=1)}}
    for (i <- 0 until digitWidth*stages) {
      carry(0)(i) := cReg(i)
    }
    for (i <- 0 until digitWidth) {
     val res = Vec.fill(digitWidth*stages + 1){UInt(width=1)}
     for (j <- 0 until digitWidth*stages) {
      val (r, c) = FullAdder(aAndB(i)(j), res(j), carry(i)(j))
      res(j+1) := r
      carry(i+1)(j) := c
     }
     result.append(res(digitWidth*stages)) 
    }

    // // Update the Register's
    aReg := Mux(LSDFHelper.newExample(count, stages), UInt(0), newA)
    bReg := Mux(LSDFHelper.newExample(count, stages), UInt(0), newB) 
    for (i <- 0 until digitWidth*stages) {
      cReg(i) := Mux(LSDFHelper.newExample(count, stages), UInt(0), carry(digitWidth)(i))
    }
    Vec(result).toBits.toUInt
  }
}
