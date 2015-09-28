/*
 Copyright (c) 2011, 2012, 2013, 2014 The Regents of the University of
 California (Regents). All Rights Reserved.  Redistribution and use in
 source and binary forms, with or without modification, are permitted
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

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore


import Chisel._

class DSASuite extends TestSuite {

  /* Helper Functions and Objects */    
  def getStages(totalWidth : Int, digit : Int) = scala.math.round(totalWidth.toDouble/digit.toDouble).toInt

  def newExample(count : UInt, stages : Int) : Bool = Mux(count === UInt(stages - 1), Bool(true), Bool(false))

  def wrapAround(n : UInt, max : UInt) = Mux(n > max, UInt(0), n)

  def counter(max : UInt, amt : UInt, init : Int = 0) : UInt = {
    val x = Reg(init = UInt(init, max.getWidth()))
    x := wrapAround(x + amt, max)
    x
  }

  object FullAdder {
    def apply(a : UInt, b : UInt, cin : UInt) : (UInt, UInt) = ((a^b)^cin, (a&b)|(a&cin)|(b&cin))
  }

  object CarrySaveAdder {
    def apply(a : UInt, b : UInt, c : UInt, cin : UInt) : (UInt, UInt) = {
      val vc = Vec.fill(b.getWidth() + 1){UInt(width=1)}
      vc(0) := cin
      val vs = Vec.fill(b.getWidth()){UInt(width=1)}
      for (i <- 0 until b.getWidth()) {
        val (r, cs) = FullAdder(a(i), b(i), c(i))
        vs(i) := r
        vc(i + 1) := cs
      }
      (vs.toBits.toUInt, vc.toBits.toUInt)
    }

    def apply(a : UInt, b : UInt) : (UInt, UInt) = CarrySaveAdder(a, b, UInt(0), UInt(0))
    def apply(a : UInt, b : UInt, c : UInt) : (UInt, UInt) = CarrySaveAdder(a, b, c, UInt(0))
  }

  object RippleCarryAdder {
    def apply(a : UInt, b : UInt, cin : UInt) : (UInt, UInt) = {
      val cout = Vec.fill(b.getWidth() + 1){UInt(width=1)}
      cout(0) := cin
      val res = Vec.fill(b.getWidth()){UInt(width=1)}
      for (i <- 0 until b.getWidth()) {
        val (r, c) = FullAdder(a(i), b(i), cout(i))
        res(i) := r
        cout(i+1) := c
      }
      (res.toBits.toUInt, cout(b.getWidth()))
    }
  }


  val trials = 10
  val r = scala.util.Random

  /* Least Significant Digit Implementations */

  val totalWidth = 16
  val digit = 4
  val n = totalWidth/digit

  object LSDFAdd {
    def apply(a : UInt, b : UInt, stages : Int, delay : Int) : UInt = {
      val countInit = if(delay ==0) 0 else stages - delay
      val count = counter(UInt(stages - 1), UInt(1), countInit)
      val carry = Reg(init=UInt(0))
      val (res, cout) = RippleCarryAdder(a, b, carry)
      carry := Mux(newExample(count, stages), UInt(0), cout)
      res
    }
  }

  object LSDFSub {
    def apply(a : UInt, b : UInt, stages : Int, delay : Int) : UInt = {
      val countInit = if(delay ==0) 0 else stages - delay
      val count = counter(UInt(stages - 1), UInt(1), countInit)
      val carry = Reg(init=UInt(1))
      val (res, cout) = RippleCarryAdder(a, ~b, carry)
      carry := Mux(newExample(count, stages), UInt(1), cout)
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

      // Stage Counter
      val countInit = if(delay ==0) 0 else stages - delay
      val count = counter(UInt(stages - 1), UInt(1), countInit)

      // Digit Counter
      val digitInit = if(delay == 0) 0 else digitWidth*stages - delay*digitWidth
      val digitCount = counter(UInt(digitWidth*(stages - 1)), UInt(digitWidth), digitInit)

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
      aReg := Mux(newExample(count, stages), UInt(0), newA)
      bReg := Mux(newExample(count, stages), UInt(0), newB) 
      for (i <- 0 until digitWidth*stages) {
        cReg(i) := Mux(newExample(count, stages), UInt(0), carry(digitWidth)(i))
      }
      Vec(result).toBits.toUInt
    }
  }

  // @Test def testLSDFAdd() {
  //   class LSDFAddTest extends Module {
  //     val io = new Bundle {
  //       val a = UInt(INPUT, digit)
  //       val b = UInt(INPUT, digit)
  //       val c = UInt(OUTPUT, digit)
  //     }
  //     io.c := LSDFAdd(io.a, io.b, n, 0)
  //   }

  //   class LSDFAddTests(c : LSDFAddTest) extends Tester(c) {
  //     for (i <- 0 until trials) {
  //       val inA = BigInt(r.nextInt(1 << totalWidth - 2))
  //       val inB = BigInt(r.nextInt(1 << totalWidth - 2))
  //       var result = BigInt(0)
  //       for (j <- 0 until n) {
  //         val currA = (inA & BigInt(0xF << (digit*j))) >> (digit*j)
  //         val currB = (inB & BigInt(0xF << (digit*j))) >> (digit*j)
  //         poke(c.io.a, currA)
  //         poke(c.io.b, currB)
  //         val res = peek(c.io.c)
  //         step(1)
  //         for (k <- 0 until digit) {
  //           val set = if ((res & BigInt(1 << k)) == BigInt(scala.math.pow(2, k).toInt)) true else false
  //           result = if (set) result.setBit(digit*j + k) else result
  //         }
  //       }
  //       val expectedResult = inA + inB
  //       val expected = if(expectedResult == result) true else false
  //       expect(expected, "Expected: " + expectedResult.toString + "\tGot: " + result.toString)
  //     }
  //   }

  //   launchCppTester((c : LSDFAddTest) => new LSDFAddTests(c))
  // }

  // @Test def testLSDFSub() {
  //   class LSDFSubTest extends Module {
  //     val io = new Bundle {
  //       val a = UInt(INPUT, digit)
  //       val b = UInt(INPUT, digit)
  //       val c = UInt(OUTPUT, digit)
  //     }
  //     io.c := LSDFSub(io.a, io.b, n, 0)
  //   }

  //   class LSDFSubTests(c : LSDFSubTest) extends Tester(c) {
  //     for (i <- 0 until trials) {
  //       var inA = BigInt(r.nextInt(1 << totalWidth - 2))
  //       var inB = BigInt(r.nextInt(1 << totalWidth - 2))
  //       while (inB > inA) {
  //         inA = BigInt(r.nextInt(1 << totalWidth - 2))
  //         inB = BigInt(r.nextInt(1 << totalWidth - 2))
  //       }
  //       var result = BigInt(0)
  //       for (j <- 0 until n) {
  //         val currA = (inA & BigInt(0xF << (digit*j))) >> (digit*j)
  //         val currB = (inB & BigInt(0xF << (digit*j))) >> (digit*j)
  //         poke(c.io.a, currA)
  //         poke(c.io.b, currB)
  //         val res = peek(c.io.c)
  //         step(1)
  //         for (k <- 0 until digit) {
  //           val set = if ((res & BigInt(1 << k)) == BigInt(scala.math.pow(2, k).toInt)) true else false
  //           result = if (set) result.setBit(digit*j + k) else result
  //         }
  //       }
  //       val expectedResult = inA - inB
  //       val expected = if(expectedResult == result) true else false
  //       expect(expected, "Expected: " + expectedResult.toString + "\tGot: " + result.toString)
  //     }
  //   }

  //   launchCppTester((c : LSDFSubTest) => new LSDFSubTests(c))
  // }

  // @Test def testLSDFMul() {
  //   class LSDFMulTest extends Module {
  //     val io = new Bundle {
  //       val a = UInt(INPUT, digit)
  //       val b = UInt(INPUT, digit)
  //       val c = UInt(OUTPUT, digit)
  //     }
  //     io.c := LSDFMul(io.a, io.b, n, 0)
  //   }

  //   class LSDFMulTests(c : LSDFMulTest) extends Tester(c) {
  //     for (i <- 0 until trials) {
  //       val inA = BigInt(r.nextInt(1 << totalWidth/2 - 2))
  //       val inB = BigInt(r.nextInt(1 << totalWidth/2 - 2))
  //       var result = BigInt(0)
  //       for (j <- 0 until n) {
  //         val currA = (inA & BigInt(0xF << (digit*j))) >> (digit*j)
  //         val currB = (inB & BigInt(0xF << (digit*j))) >> (digit*j)
  //         poke(c.io.a, currA)
  //         poke(c.io.b, currB)
  //         val res = peek(c.io.c)
  //         step(1)
  //         for (k <- 0 until digit) {
  //           val set = if ((res & BigInt(1 << k)) == BigInt(scala.math.pow(2, k).toInt)) true else false
  //           result = if (set) result.setBit(digit*j + k) else result
  //         }
  //       }
  //       val expectedResult = inA * inB
  //       val expected = if(expectedResult == result) true else false
  //       expect(expected, "Expected: " + expectedResult.toString + "\tGot: " + result.toString)
  //     }
  //   }

  //   launchCppTester((c : LSDFMulTest) => new LSDFMulTests(c))
  // }

  /* Most Significant Digit Implementations */

  /* Signed Digit Representation
   * a = 1, r = 2
   * S = {-1, 0, 1}
   *   00 = 0
   *   01 = -1
   *   10 = 1
   *   11 = 0
   */

  def signedToDouble(in : List[Int]) : Double = {
    var out : Double = 0.0
    for (i <- 0 until in.length) {
      out += in(i).toDouble*scala.math.pow(2, -(i+1))
    }
    out
  }

  def doubleToSigned(in : Double, width : Int) : List[Int] = {
    val out = new ArrayBuffer[Int]
    var currIn = in
    for (i <- 0 until width) {
      if (scala.math.pow(2, -(i+1)) <= currIn) {
        out.append(1)
        currIn = currIn - scala.math.pow(2, -(i+1))
      } else {
        out.append(0)
      }
    }
    out.toList
  }

  def signedListToUInt(in : List[Int]) : String = {
    val out = in.map(x => if (x == 1) "10" else if (x == -1) "01" else "00")
    "b" + out.mkString("")
  } 

  def toSignedDigit(a : List[Int]) : BigInt = {
    val signedEle = a.map(toSignedDigit(_))
    var res : BigInt = BigInt(0)
    for (i <- signedEle.length - 1 to 0 by -1) {
      res += (signedEle(i) << ((signedEle.length - 1 - i)*2))
    }
    res
  }

  def toSignedDigit(a : Int) : BigInt = if (a == -1) BigInt(1) else toSignedDigit(BigInt(a))
  def toSignedDigit(a : BigInt) : BigInt = {
    var count = 0
    var res : BigInt = BigInt(0)
    for (i <- 0 until a.bitLength) {
      if (a.testBit(i)) {
        res = res.setBit(count+1)
      }
      count += 2
    }
    res
  }

  def signedArraytoInt(signedDigits : ArrayBuffer[Int]) : BigInt = {
    var res : BigInt = BigInt(0)
    for (i <- 0 until signedDigits.length) {
      res += signedDigits(i)*scala.math.pow(2, i).toInt
    }
    res
  }

  // def fromSignedDigit(a : Int) : Int = fromSignedDigit(BigInt(a)).toInt
  def fromSignedDigit(a : BigInt) : ArrayBuffer[Int] = fromSignedDigit(a, 1)
  def fromSignedDigit(a : BigInt, expectedDigits : Int) : ArrayBuffer[Int] = {
    var signedDigits = new ArrayBuffer[Int]
    var i = 0
    while (i < a.bitLength) {
      if(a.testBit(i+1) && a.testBit(i)) {
        signedDigits.prepend(0)
      } else if(a.testBit(i+1) && !a.testBit(i)) {
        signedDigits.prepend(1)
      } else if(!a.testBit(i+1) && a.testBit(i)) {
        signedDigits.prepend(-1)
      } else {
        signedDigits.prepend(0)
      }
      i += 2
    }
    if (signedDigits.length != expectedDigits) {
      for (i <- 0 until (expectedDigits - signedDigits.length))
        signedDigits.prepend(0)
    }
    signedDigits
  }

  object fourToTwoAdder {
    def apply(a : UInt, b : UInt, c : UInt, d : UInt, cin1 : UInt, cin2 : UInt) : (UInt, UInt) =  {
      val vs = Vec.fill(a.getWidth()){UInt(width=1)}
      val vc = Vec.fill(a.getWidth() + 1){UInt(width=1)}
      vc(0) := cin1

      val cT = Vec.fill(a.getWidth() + 1){UInt(width=1)}
      val sT = Vec.fill(a.getWidth()){UInt(width=1)}
      cT(0) := cin2

      // Two Layers of Adders
      for (i <- 0 until a.getWidth()) {
        val (s0, c0) = FullAdder(a(i), b(i), c(i))
        val (s1, c1) = FullAdder(s0, d(i), cT(i))
        cT(i+1) := c0
        vs(i) := s1
        vc(i+1) := c1
      }
      val vcF = Vec.fill(a.getWidth()){UInt(width=1)}
      for (i <- 0 until a.getWidth()) {
        vcF(i) := vc(i)
      }

      (vs.toBits.toUInt, vcF.toBits.toUInt)
    }

    def apply(a : UInt, b : UInt, c : UInt, d : UInt, cin : UInt) : (UInt, UInt) = fourToTwoAdder(a, b, c, d, cin, UInt(0))
    def apply(a : UInt, b : UInt, c : UInt, d : UInt) : (UInt, UInt) = fourToTwoAdder(a, b, c, d, UInt(0), UInt(0))
  }

  object threeToTwoAdder {
    def apply(a : UInt, b : UInt, c : UInt, cin : UInt) : (UInt, UInt) =  {
      val vc = Vec.fill(b.getWidth() + 1){UInt(width=1)}
      vc(0) := cin
      val vs = Vec.fill(b.getWidth()){UInt(width=1)}
      for (i <- 0 until b.getWidth()) {
        val (r, cs) = FullAdder(a(i), b(i), c(i))
        vs(i) := r
        vc(i + 1) := cs
      }

      val vcF = Vec.fill(a.getWidth()){UInt(width=1)}
      for (i <- 0 until a.getWidth()) {
        vcF(i) := vc(i)
      }

      (vs.toBits.toUInt, vcF.toBits.toUInt)
    }

    def apply(a : UInt, b : UInt, c : UInt) : (UInt, UInt) = fourToTwoAdder(a, b, c, UInt(0))
  }

  object SignedDigitAdder {
    def apply(a : UInt, b : UInt) : UInt = {
      
      // Create a and b pos and neg
      val ap = Vec.fill(a.getWidth()/2){UInt(width=1)}
      val an = Vec.fill(a.getWidth()/2){UInt(width=1)}
      val bp = Vec.fill(b.getWidth()/2){UInt(width=1)}
      val bn = Vec.fill(b.getWidth()/2){UInt(width=1)}
      val sp = Vec.fill(b.getWidth()/2){UInt(width=1)}
      val sn = Vec.fill(b.getWidth()/2 + 1){UInt(width=1)}
      sn(0) := UInt(0)
      var count = 0
      for (i <- 0 until a.getWidth()) {
        if (i%2 == 0) {
          an(count) := a(i)
          bn(count) := b(i)
        } else {
          ap(count) := a(i)
          bp(count) := b(i)
          count += 1
        }

      }

      // Now we hook up the adders
      val h = Vec.fill(a.getWidth()/2 + 1){UInt(width=1)}
      h(0) := UInt(0)
      for (i <- 0 until a.getWidth()/2) {
        val (v0, h1) = FullAdder(ap(i), ~an(i), bp(i))
        h(i+1) := h1
        val (w0, t1) = FullAdder(v0, ~bn(i), h(i))
        sn(i+1) := ~t1
        sp(i) := w0
      }
      val s = Vec.fill(a.getWidth()){UInt(width=1)}
      count = 0
      for (i <- 0 until a.getWidth()) {
        if (i%2 == 0) {
          s(i) := sn(count)
        } else {
          s(i) := sp(count)
          count += 1
        }
      }
      s.toBits.toUInt
    }
  }

  object MSDFSub {
    def apply(a : UInt, b : UInt, start : Bool) = MSDFAdd(a, ~b, start)
  }

  object MSDFAdd {
    def apply(a : UInt, b : UInt, start : Bool) = {
      val nextStart = Reg(next=start)
      // This needs to be done in three stages
      // First we split the two numbers into the positive and negative parts
      val ap = Vec.fill(a.getWidth()/2){UInt(width=1)}
      val an = Vec.fill(a.getWidth()/2){UInt(width=1)}
      val bp = Vec.fill(b.getWidth()/2){UInt(width=1)}
      val bn = Vec.fill(b.getWidth()/2){Reg(init=UInt(0, width=1))}
      var count = 0
      for (i <- a.getWidth() - 1 to 0 by -1) {
        if (i%2 == 0) {
          an(count) := a(i)
          bn(count) := b(i)
          count += 1
        } else {
          ap(count) := a(i)
          bp(count) := b(i)
        }
      }

      // Now we do the first stage
      val h = Vec.fill(a.getWidth()/2 - 1){Reg(init=UInt(0, width=1))}
      val v = Vec.fill(a.getWidth()/2){Reg(init=UInt(0, width=1))}
      val hTrans = UInt(width=1)
      val (v0, h1) = FullAdder(ap(0), ~an(0), bp(0))
      v(0) := v0
      hTrans := h1
      for (i <- 1 until a.getWidth()/2) {
        val (v0, h1) = FullAdder(ap(i), ~an(i), bp(i))
        h(i - 1) := h1
        v(i) := v0
      }      

      // Do the second stage
      val t = Vec.fill(a.getWidth()/2 - 1){Reg(init=UInt(0, width=1))}
      val w = Vec.fill(a.getWidth()/2){Reg(init=UInt(0, width=1))}
      val tTrans = UInt(width=1)
      val (w0, t1) = if (a.getWidth()/2 != 1) {
        FullAdder(v(0), ~bn(0), h(0))
      } else {
        FullAdder(v(0), ~bn(0), hTrans)
      }
      tTrans := ~t1
      w(0) := w0

      for (i <- 1 until a.getWidth()/2 - 1) {
        val (w1, t1) = FullAdder(v(i), ~bn(i), h(i))
        t(i - 1) := ~t1
        w(i) := w1
      }
      if (a.getWidth()/2 != 1) {
        val (w2, t2) = FullAdder(v(a.getWidth()/2 - 1), ~bn(a.getWidth()/2 - 1), hTrans)
        t(a.getWidth()/2 - 2) := ~t2
        w(a.getWidth()/2 - 1) := w2
      }

      // Do the third stage
      val s = Vec.fill(a.getWidth()){UInt(width=1)}
      count = 0
      for (i <- a.getWidth() - 1 to 2 by -1) {
        if (i%2 == 0) {
          s(i) := t(count)
          count += 1
        } else {
          s(i) := w(count)
        }
      }
      s(1) := w(a.getWidth/2 - 1)
      s(0) := Mux(nextStart, UInt(0), tTrans)
      s.toBits.toUInt

    }
  }


  /* Most Significant Digit Division
   * 1. [Initialize]
   *   x[-4] = d[-4] = w[-4] = q[0] = 0
   *   for j = -4 ... -1
   *     d[j + 1] = CA(d[j], dj5)
   *     v[j]     = 2w[j] + xj5*2^-4
   *     w[j + 1] = v[j]
   *   end for
   * 2. [Recurrence]
   *   for j = 0 ... n - 1
   *     d[j + 1] = CA(d[j], dj5)
   *     v[j]     = 2w[j] + xj5*2^-4 - q[j]dj5*2^-4
   *     qj1      = SELD(v[j])
   *     w[j + 1] = v[j] - qj1*d[j + 1]
   *     q[j + 1] = CA(q[j], qj1)
   *     Qout     = qj1
   *   end for
   */
   object MSDFDiv {
    def apply(x : UInt, d : UInt, start : Bool) : UInt = {
      val caQ = Reg(init=UInt(0, width=14))
      val caD = Reg(init=UInt(0, width=14))
      val ws = Reg(init=UInt(0, width=14))
      val wc = Reg(init=UInt(0, width=14))
      val nxtQ = Reg(init=UInt(0, width=2))

      // d[j + 1] = CA(d[j], dj5)
      val newD = UInt(width=14)
      newD := SDOnlineConversion(d, start)
      caD := newD

      // D Selector : -qj1*d[j + 1]
      val inD = UInt(width=14)
      inD := MuxCase(UInt(0), Array(
        (nxtQ === UInt("b10")) -> newD,
        (nxtQ === UInt("b01")) -> ~newD
        ))

      // Q Selector : -q[j]*dj5*2^-4
      val inQ = UInt(width=14)
      inQ := MuxCase(UInt(0), Array(
        (d === UInt("b10")) -> caQ,
        (d === UInt("b01")) -> ~caQ
        )) >> 4

      // xj5*2^-4 - q[j]*dk5*2^-4
      val inWS = UInt(width=14)
      inWS := Cat(U(x, d, nxtQ), ws(ws.getWidth() - 6, 0))
      val inWC = UInt(width=14)
      inWC := Cat(Fill(6, UInt(0)), wc(wc.getWidth() - 6, 0))

      // Carry cd
      val cD = Mux( ((nxtQ === UInt("b10")) & (d === UInt("b10"))) | ((nxtQ === UInt("b01")) & (d === UInt("b01"))), UInt(1), UInt(0))

      // v[j] = 2w[j] + xj5*2^-4 - q[j]*dj5*2^-4
      val (s1, c1) = threeToTwoAdder(inWS, inWC, inQ, cD)

      // qj1 = SELD(v[j])
      val v = s1(s1.getWidth() - 1, s1.getWidth() - 5) + c1(c1.getWidth() - 1, c1.getWidth() - 5)
      val nQ = SEL(v)

      nxtQ := nQ

      // q[j + 1] = CA(q[j], qj1)
      caQ := SDOnlineConversion(nQ, start)

      // Carry cq
      val cQ = Mux( ((nxtQ === UInt("b10")) & (d === UInt("b10"))) | ((nxtQ === UInt("b01")) & (d === UInt("b01"))), UInt(1), UInt(0))

      // w[j + 1] = v[j] - qj1*d[j + 1]
      val (s2, c2) = threeToTwoAdder(s1, c1, inD, cQ)

      ws := s2 << 1
      wc := c2 << 1

      nQ
    }

    def U(x : UInt, d : UInt, q : UInt) : UInt = {
      // Gate Network Implementation
      // // x Values
      // val xP = x(1)
      // val xN = x(0)
      // val xZ = !(xP ^ xN)

      // // d Value
      // val dP = d(1)
      // val dN = d(0)
      // val dZ = !(dP ^ dN)

      // // q Sign
      // val qS = q

      // val ui = xN + (xZ & ((dP & ~qS) + (dN & qS)))
      // val u4 = (~xN & dZ) + (~xZ & ((dP & qS) + (dN & ~qS))) + (xZ & ((dP & ~qS) + (dN & qS)))
      // Cat(Fill(ui, 5), u4)

      // Mux Implementation
      MuxCase(UInt("b000000"), Array(
        
        ((x === UInt("b10")) & (d === UInt("b10")) & (q === UInt("b01"))) -> UInt("b000001"), // 1 (Neg Q)
        ((x === UInt("b10")) & (d === UInt("b10"))) -> UInt("b000000"), // 1
        ((x === UInt("b10")) & (d === UInt("b00"))) -> UInt("b000001"), // 2
        ((x === UInt("b10")) & (d === UInt("b11"))) -> UInt("b000001"), // 2
        ((x === UInt("b10")) & (d === UInt("b01")) & (q === UInt("b01"))) -> UInt("b000000"), // 3
        ((x === UInt("b10")) & (d === UInt("b01"))) -> UInt("b000001"), // 3
        
        ((x === UInt("b00")) & (d === UInt("b10")) & (q === UInt("b01"))) -> UInt("b000000"), // 4
        ((x === UInt("b00")) & (d === UInt("b10"))) -> UInt("b111111"), // 4
        ((x === UInt("b00")) & (d === UInt("b00"))) -> UInt("b000000"), // 5
        ((x === UInt("b00")) & (d === UInt("b11"))) -> UInt("b000000"), // 5
        ((x === UInt("b00")) & (d === UInt("b01")) & (q === UInt("b01"))) -> UInt("b111111"), // 6
        ((x === UInt("b00")) & (d === UInt("b01"))) -> UInt("b000000"), // 6
        
        ((x === UInt("b11")) & (d === UInt("b10")) & (q === UInt("b01"))) -> UInt("b000000"), // 4
        ((x === UInt("b11")) & (d === UInt("b10"))) -> UInt("b111111"), // 4
        ((x === UInt("b11")) & (d === UInt("b00"))) -> UInt("b000000"), // 5
        ((x === UInt("b11")) & (d === UInt("b11"))) -> UInt("b000000"), // 5
        ((x === UInt("b11")) & (d === UInt("b01")) & (q === UInt("b01"))) -> UInt("b111111"), // 6
        ((x === UInt("b11")) & (d === UInt("b01"))) -> UInt("b000000"), // 6


        ((x === UInt("b01")) & (d === UInt("b10")) & (q === UInt("b01"))) -> UInt("b111111"), // 7
        ((x === UInt("b01")) & (d === UInt("b10"))) -> UInt("b111110"), // 7
        ((x === UInt("b01")) & (d === UInt("b00"))) -> UInt("b111111"), // 8
        ((x === UInt("b01")) & (d === UInt("b11"))) -> UInt("b111111"), // 8
        ((x === UInt("b01")) & (d === UInt("b01")) & (q === UInt("b01"))) -> UInt("b111110"),  // 9
        ((x === UInt("b01")) & (d === UInt("b01"))) -> UInt("b111111")  // 9
        ))
    }

    // Optimise by removing the LSB of a
    def SEL(a : UInt) : UInt = {
      MuxCase(UInt("b01"), Array(
        (a === UInt("b01111")) -> UInt("b10"), //  1.875
        (a === UInt("b01110")) -> UInt("b10"), //  1.750
        (a === UInt("b01101")) -> UInt("b10"), //  1.625
        (a === UInt("b01100")) -> UInt("b10"), //  1.500
        (a === UInt("b01011")) -> UInt("b10"), //  1.375
        (a === UInt("b01010")) -> UInt("b10"), //  1.250
        (a === UInt("b01001")) -> UInt("b10"), //  1.125
        (a === UInt("b01000")) -> UInt("b10"), //  1.000
        (a === UInt("b00111")) -> UInt("b10"), //  0.875
        (a === UInt("b00110")) -> UInt("b10"), //  0.750
        (a === UInt("b00101")) -> UInt("b10"), //  0.625
        (a === UInt("b00100")) -> UInt("b10"), //  0.500
        (a === UInt("b00011")) -> UInt("b10"), //  0.375
        (a === UInt("b00010")) -> UInt("b10"), //  0.250
        (a === UInt("b00001")) -> UInt("b00"), //  0.125
        (a === UInt("b00000")) -> UInt("b00"), //  0.000
        (a === UInt("b11111")) -> UInt("b00"), // -0.125
        (a === UInt("b11110")) -> UInt("b00")  // -0.250
      ))
    }
   }

  /* Most Significant Digit Multiplication
   * 1. [Initialize]
   *   x[-3] = y[-3] = w[-3] = 0
   *   for j = -3, -2, -1
   *     x[j + 1] = CA(x[j], xj4)
   *     y[j + 1] = CA(y[j], yj4)
   *     v[j]     = 2w[j] + (x[j]*yj4 + y[j + 1]xj4)*2^-3
   *     w[j + 1] = v[j]
   *   end for
   * 2. [Recurrnce]
   *   for j = 0 ... n - 1
   *     x[j + 1] = CA(x[j], xj4)
   *     y[j + 1] = CA(y[j], yj4)
   *     v[j]     = 2w[j] + (x[j]*yj4 + y[j + 1]xj4)*2^-3
   *     pj1      = SELM(v[j])
   *     w[j + 1] = v[j] - pj1
   *     Pout     = pj1
   *   end for
   */
  object MSDFMul {
    def apply(a : UInt, b : UInt, start : Bool) : UInt = {
      val caA = Reg(init=UInt(0, width=14))
      val ws = Reg(init=UInt(0, width=14))
      val wc = Reg(init=UInt(0, width=14))

      // CA(x[j], xj4)
      caA := (SDOnlineConversion(a, start) >> 3)

      // CA(y[j], yj4)
      val newCAb = UInt(width=14)
      newCAb := (SDOnlineConversion(b, start) >> 3)

      // x[j]*yj4
      val inA = UInt(width=14)
      inA := Mux(start, UInt(0), MuxCase(UInt(0), Array(
        (b === UInt("b01")) -> ~caA,
        (b === UInt("b10")) -> caA
        )))

      // y[j + 1]*xj4
      val inB = UInt(width=14)
      inB := MuxCase(UInt(0), Array(
        (a === UInt("b01")) -> ~newCAb,
        (a === UInt("b10")) -> newCAb
        ))

      // cA = 1 if a < 0
      val cA = Mux(a === UInt("b01"), UInt(1), UInt(0))
      
      // cB = 1 if b < 0
      val cB = Mux(b === UInt("b01"), UInt(1), UInt(0))
      val nextStart = Bool()
      nextStart := start
      val inWC = Mux(nextStart, UInt(0), wc)
      val inWS = Mux(nextStart, UInt(0), ws)

      // 2w[j] + (x[j]*yj4 + y[j + 1]*xj4)*2^-3
      val (newWS, newWC) = fourToTwoAdder(inWS, inA, inB, inWC, cA, cB)

      // val vS = newWS + newWC 
      val v = newWS(newWS.getWidth() - 1, newWS.getWidth() - 4) + newWC(newWC.getWidth() - 1, newWC.getWidth() - 4)
      val p = SEL(v(v.getWidth() - 1, v.getWidth() - 3))
      val magP = Mux(p === UInt(0), UInt("b0"), UInt("b1"))

      val newWSTop = UInt(width=14)
      newWSTop := Cat(UInt(1), magP^v(v.getWidth() - 2), v(v.getWidth() - 3, v.getWidth() - 4), newWS(newWS.getWidth() - 5, 0))

      ws := (newWSTop << 1)
      wc := (newWC(newWC.getWidth() - 5, 0) << 1)

      p
    }


    def SEL(a : UInt) : UInt = {
      MuxCase(UInt("b00"), Array(
        (a === UInt("b001")) -> UInt("b10"),
        (a === UInt("b010")) -> UInt("b10"),
        (a === UInt("b011")) -> UInt("b10"),
        (a === UInt("b100")) -> UInt("b01"),
        (a === UInt("b101")) -> UInt("b01"),
        (a === UInt("b110")) -> UInt("b01")
        ))
    }
  }

  object MSDFLiteral {
    def apply(a : UInt, initialIndex : Int = 0) : UInt = {
      val literalWidth = a.getWidth()/2
      val litArray = new ArrayBuffer[UInt]
      for (i <- a.getWidth() - 1 to 1 by -2) {
        val reg = Reg(init=a(i, i - 1))
        litArray.append(reg)
      }
      val litRegs = Vec(litArray)
      for (i <- 0 until litRegs.length - 1) {
        litRegs(i) := litRegs(i + 1)
      }
      litRegs(litRegs.length - 1) := litRegs(0)
      litRegs(initialIndex)
    }
  }


  object SDOnlineConversion {
    def apply(a : UInt, start : Bool) : UInt = {
      val q = Reg(init=UInt(0, width=totalWidth))
      val qm = Reg(init=UInt(0, width=totalWidth))
      val counter = Reg(init=UInt(12, width=totalWidth))

      val negOne = a === UInt("b01")
      val one = a === UInt("b10")
      val zero = !negOne & !one

      // Q and QM reset
      val inQ = Mux(start, UInt(0), q)
      val inQM = Mux(start, UInt(0), qm)

      // Counter reset
      val inCounter = Mux(start, UInt(11), counter - UInt(1))

      val nextQM = MuxCase(UInt(0), Array(
        one -> Cat(inQ, UInt(0)),
        zero -> Cat(inQM, UInt(1)),
        negOne -> Cat(inQM, UInt(0))
      ))

      val nextQ = MuxCase(UInt(0), Array(
        one -> Cat(inQ, UInt(1)),
        zero -> Cat(inQ, UInt(0)),
        negOne -> Cat(inQM, UInt(1))
      ))

      q := nextQ
      qm := nextQM
      counter := inCounter

      nextQ << inCounter
    }

    def apply(in : UInt, inQ : UInt, inQM : UInt, currCounter : UInt) : (UInt, UInt, UInt, UInt) = {
      val negOne = in === UInt("b01")
      val one = in === UInt("b10")
      val zero = !negOne & !one

      val nextQM = MuxCase(UInt(0), Array(
        one -> Cat(inQ, UInt(0)),
        zero -> Cat(inQM, UInt(1)),
        negOne -> Cat(inQM, UInt(0))
        ))

      val nextQ = MuxCase(UInt(0), Array(
        one -> Cat(inQ, UInt(1)),
        zero -> Cat(inQ, UInt(0)),
        negOne -> Cat(inQM, UInt(1))
        ))

      (nextQ << currCounter, nextQ, nextQM, currCounter - UInt(1))
    }
  }

  @Test def testMSDFMulSEL() {
    class MSDFMulSELTest extends Module {
      val io = new Bundle {
        val a = UInt(INPUT, 3)
        val c = UInt(OUTPUT, 2)
      }
      io.c := MSDFMul.SEL(io.a)
    }

    class MSDFMulSELTests(c : MSDFMulSELTest) extends Tester(c) {
      val sel = List(
        UInt("b000").litValue(), 
        UInt("b001").litValue(), 
        UInt("b010").litValue(), 
        UInt("b011").litValue(), 
        UInt("b100").litValue(),
        UInt("b101").litValue(),
        UInt("b110").litValue(),
        UInt("b111").litValue()
        )
      val selAns = List(
        UInt("b0").litValue(), 
        UInt("b10").litValue(), 
        UInt("b10").litValue(), 
        UInt("b10").litValue(), 
        UInt("b1").litValue(),
        UInt("b1").litValue(),
        UInt("b1").litValue(),
        UInt("b0").litValue()
        )

      for (i <- 0 until sel.length) {
        poke(c.io.a, sel(i))
        expect(c.io.c, selAns(i))
      }
    }

    launchCppTester((c : MSDFMulSELTest) => new MSDFMulSELTests(c))
  }

  @Test def testMSDFDivSEL() {
    class MSDFDivSELTest extends Module {
      val io = new Bundle {
        val a = UInt(INPUT, 5)
        val c = UInt(OUTPUT, 2)
      }
      io.c := MSDFDiv.SEL(io.a)
    }

    class MSDFDivSELTests(c : MSDFDivSELTest) extends Tester(c) {
      val sel = List(
        UInt("b00000").litValue(), 
        UInt("b00001").litValue(), 
        UInt("b00010").litValue(), 
        UInt("b00011").litValue(), 
        UInt("b00100").litValue(),
        UInt("b00101").litValue(),
        UInt("b00110").litValue(),
        UInt("b00111").litValue(),
        UInt("b01000").litValue(), 
        UInt("b01001").litValue(), 
        UInt("b01010").litValue(), 
        UInt("b01011").litValue(), 
        UInt("b01100").litValue(),
        UInt("b01101").litValue(),
        UInt("b01110").litValue(),
        UInt("b01111").litValue(),
        UInt("b10000").litValue(), 
        UInt("b10001").litValue(), 
        UInt("b10010").litValue(), 
        UInt("b10011").litValue(), 
        UInt("b10100").litValue(),
        UInt("b10101").litValue(),
        UInt("b10110").litValue(),
        UInt("b10111").litValue(),
        UInt("b11000").litValue(), 
        UInt("b11001").litValue(), 
        UInt("b11010").litValue(), 
        UInt("b11011").litValue(), 
        UInt("b11100").litValue(),
        UInt("b11101").litValue(),
        UInt("b11110").litValue(),
        UInt("b11111").litValue()
        )
      val selAns = List(
        UInt("b00").litValue(), 
        UInt("b00").litValue(), 
        UInt("b10").litValue(), 
        UInt("b10").litValue(), 
        UInt("b10").litValue(), 
        UInt("b10").litValue(), 
        UInt("b10").litValue(), 
        UInt("b10").litValue(), 
        UInt("b10").litValue(), 
        UInt("b10").litValue(), 
        UInt("b10").litValue(), 
        UInt("b10").litValue(), 
        UInt("b10").litValue(), 
        UInt("b10").litValue(), 
        UInt("b10").litValue(), 
        UInt("b10").litValue(), 
        UInt("b01").litValue(),
        UInt("b01").litValue(),
        UInt("b01").litValue(),
        UInt("b01").litValue(),
        UInt("b01").litValue(),
        UInt("b01").litValue(),
        UInt("b01").litValue(),
        UInt("b01").litValue(),
        UInt("b01").litValue(),
        UInt("b01").litValue(),
        UInt("b01").litValue(),
        UInt("b01").litValue(),
        UInt("b01").litValue(),
        UInt("b01").litValue(),
        UInt("b00").litValue(),
        UInt("b00").litValue()
        )

      for (i <- 0 until sel.length) {
        poke(c.io.a, sel(i))
        expect(c.io.c, selAns(i))
      }
    }

    launchCppTester((c : MSDFDivSELTest) => new MSDFDivSELTests(c))
  }

  @Test def testMSDFDivU() {
    class MSDFDivUTest extends Module {
      val io = new Bundle {
        val x = UInt(INPUT, 2)
        val d = UInt(INPUT, 2)
        val c = UInt(OUTPUT, 6)
      }
      io.c := MSDFDiv.U(io.x, io.d, UInt("b0"))
    }

    class MSDFDivUTests(c : MSDFDivUTest) extends Tester(c) {
      val u = List(
        List(UInt("b10").litValue(), UInt("b10").litValue()),
        List(UInt("b10").litValue(), UInt("b00").litValue()),
        List(UInt("b10").litValue(), UInt("b01").litValue()),
        List(UInt("b10").litValue(), UInt("b11").litValue()),
        List(UInt("b00").litValue(), UInt("b10").litValue()),
        List(UInt("b00").litValue(), UInt("b00").litValue()),
        List(UInt("b00").litValue(), UInt("b01").litValue()),
        List(UInt("b00").litValue(), UInt("b11").litValue()),
        List(UInt("b01").litValue(), UInt("b10").litValue()),
        List(UInt("b01").litValue(), UInt("b00").litValue()),
        List(UInt("b01").litValue(), UInt("b01").litValue()),
        List(UInt("b01").litValue(), UInt("b11").litValue()),
        List(UInt("b11").litValue(), UInt("b10").litValue()),
        List(UInt("b11").litValue(), UInt("b00").litValue()),
        List(UInt("b11").litValue(), UInt("b01").litValue()),
        List(UInt("b11").litValue(), UInt("b11").litValue())
        )
      val uAns = List(
        UInt("b000000").litValue(), 
        UInt("b000001").litValue(), 
        UInt("b000001").litValue(), 
        UInt("b000001").litValue(), 
        UInt("b111111").litValue(), 
        UInt("b000000").litValue(), 
        UInt("b000000").litValue(), 
        UInt("b000000").litValue(), 
        UInt("b111110").litValue(), 
        UInt("b111111").litValue(), 
        UInt("b111111").litValue(), 
        UInt("b111111").litValue(), 
        UInt("b111111").litValue(), 
        UInt("b000000").litValue(), 
        UInt("b000000").litValue(), 
        UInt("b000000").litValue() 
        )

      for (i <- 0 until u.length) {
        poke(c.io.x, u(i)(0))
        poke(c.io.d, u(i)(1))
        expect(c.io.c, uAns(i))
      }
    }

    launchCppTester((c : MSDFDivUTest) => new MSDFDivUTests(c))
  }

  @Test def testMSDFDiv() {
    class MSDFDivTest extends Module {
      val io = new Bundle {
        val x = UInt(INPUT, 2)
        val d = UInt(INPUT, 2)
        val start = Bool(INPUT)
        val c = UInt(OUTPUT, 2)
      }
      io.c := MSDFDiv(io.x, io.d, io.start)
    }

    class MSDFDivTests(c : MSDFDivTest) extends Tester(c) {

      for (i <- 0 until trials) {
        var dX = r.nextDouble()/2
        var dD = r.nextDouble()/2
        while (dX/dD > 1.0) {
          dX = r.nextDouble()/2
          dD = r.nextDouble()/2
        }
        val x = doubleToSigned(dX, 8)
        val d = doubleToSigned(dD, 8)
        val res = new ArrayBuffer[Int]
        for (j <- 0 until x.length + 4) {
          val inX = if(j < x.length) x(j) else 0
          val inD = if(j < d.length) d(j) else 0
          poke(c.io.x, toSignedDigit(inX))
          poke(c.io.d, toSignedDigit(inD))
          val start = if (j == 0) BigInt(1) else BigInt(0)
          poke(c.io.start, start)
          if (j >= 4)
            res ++= fromSignedDigit(peek(c.io.c).toInt)
          step(1)
        }
        val expectedRes = signedToDouble(x)/signedToDouble(d)
        val dRes = signedToDouble(res.toList)
        val err = scala.math.abs(expectedRes - dRes)
        val correct = if (err > scala.math.pow(2, -8)) false else true
        expect(correct, "Expected: " + expectedRes.toString + "(" + (dX*dD).toString + ")" + "\tGot: " + dRes.toString + "\tError: " + err.toString)
      }
    }
    launchCppTester((c : MSDFDivTest) => new MSDFDivTests(c))
  }

  @Test def testSDOnlineConversion() {
    class SDOnlineConversionTest extends Module {
      val io = new Bundle {
        val a = UInt(INPUT, 2)
        val c = UInt(OUTPUT, 12)
      }
      io.c := SDOnlineConversion(io.a, Bool(false))
    }

    class SDOnlineConversionTests(c : SDOnlineConversionTest) extends Tester(c) {
      val q = List(1, 1, 0, 1, -1, 0, 0, -1, 1, 0, 1, 0)
      val qAns = List(
        UInt("b100000000000").litValue(), 
        UInt("b110000000000").litValue(), 
        UInt("b110000000000").litValue(), 
        UInt("b110100000000").litValue(), 
        UInt("b110010000000").litValue(),
        UInt("b110010000000").litValue(),
        UInt("b110010000000").litValue(),
        UInt("b110001110000").litValue(),
        UInt("b110001111000").litValue(),
        UInt("b110001111000").litValue(),
        UInt("b110001111010").litValue(),
        UInt("b110001111010").litValue())

      for (i <- 0 until q.length) {
        poke(c.io.a, toSignedDigit(q(i)))
        expect(c.io.c, qAns(i))
        step(1)
      }
    }

    launchCppTester((c : SDOnlineConversionTest) => new SDOnlineConversionTests(c))
  }

  // @Test def testToSignedDigit() {
  //   val a = BigInt(1)
  //   val b = toSignedDigit(a)
  //   assertTrue(b == BigInt(2))

  //   val c = 3
  //   val d = toSignedDigit(c)
  //   assertTrue(d == BigInt(10))
  // }

  // @Test def testFromSignedDigit() {
  //   val a = BigInt(1)
  //   val b = fromSignedDigit(a)
  //   assertTrue(b == BigInt(-1))

  //   val c = 3
  //   val d = fromSignedDigit(c)
  //   assertTrue(d == BigInt(0))

  //   val e = 2
  //   val f = fromSignedDigit(e)
  //   assertTrue(f == BigInt(1))
  // }

  // @Test def testConversion() {
  //   for (i <- 0 until trials) {
  //     val a = BigInt(r.nextInt(100000))
  //     val b = fromSignedDigit(toSignedDigit(a))
  //     assertTrue(a == b)
  //   }
  // }

  @Test def testCarrySaveAdder() {
    class CarrySaveAdderTest extends Module {
      val io = new Bundle {
        val a = UInt(INPUT, totalWidth)
        val b = UInt(INPUT, totalWidth)
        val c = UInt(OUTPUT, totalWidth)
      }
      val (r, cout) = CarrySaveAdder(io.a, io.b, UInt(0, width=totalWidth), UInt(0))
      io.c := r + cout
    }

    class CarrySaveAdderTests(c : CarrySaveAdderTest) extends Tester(c) {
      for (i <- 0 until trials) {
        val inA = BigInt(totalWidth, r)
        val inB = BigInt(totalWidth, r)
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        expect(c.io.c, inA + inB)
      }
    }

    launchCppTester((c : CarrySaveAdderTest) => new CarrySaveAdderTests(c))
  }

  @Test def testthreeToTwoAdderOneCarry() {
    class threeToTwoAdderOneCarryTest extends Module {
      val io = new Bundle {
        val a = UInt(INPUT, totalWidth)
        val b = UInt(INPUT, totalWidth)
        val c = UInt(INPUT, totalWidth)
        val cin = UInt(INPUT, 1)
        val e = UInt(OUTPUT, totalWidth)
      }
      val (r, cout) = threeToTwoAdder(io.a, io.b, io.c, io.cin)
      io.e := r + cout
    }

    class threeToTwoAdderOneCarryTests(c : threeToTwoAdderOneCarryTest) extends Tester(c) {
      for (i <- 0 until trials) {
        val inA = BigInt(totalWidth, r)
        val inB = BigInt(totalWidth, r)
        val inC = BigInt(totalWidth, r)
        val inCin = BigInt(1, r)
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        poke(c.io.c, inC)
        poke(c.io.cin, inCin)
        expect(c.io.e, inA + inB + inC + inCin)
      }
    }

    launchCppTester((c : threeToTwoAdderOneCarryTest) => new threeToTwoAdderOneCarryTests(c))
  }

  @Test def testfourToTwoAdder() {
    class fourToTwoAdderTest extends Module {
      val io = new Bundle {
        val a = UInt(INPUT, totalWidth)
        val b = UInt(INPUT, totalWidth)
        val c = UInt(INPUT, totalWidth)
        val d = UInt(INPUT, totalWidth)
        val e = UInt(OUTPUT, totalWidth)
      }
      val (r, cout) = fourToTwoAdder(io.a, io.b, io.c, io.d)
      io.e := r + cout
    }

    class fourToTwoAdderTests(c : fourToTwoAdderTest) extends Tester(c) {
      for (i <- 0 until trials) {
        val inA = BigInt(totalWidth, r)
        val inB = BigInt(totalWidth, r)
        val inC = BigInt(totalWidth, r)
        val inD = BigInt(totalWidth, r)
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        poke(c.io.c, inC)
        poke(c.io.d, inD)
        expect(c.io.e, inA + inB + inC + inD)
      }
    }

    launchCppTester((c : fourToTwoAdderTest) => new fourToTwoAdderTests(c))
  }

  @Test def testfourToTwoAdderOneCarry() {
    class fourToTwoAdderOneCarryTest extends Module {
      val io = new Bundle {
        val a = UInt(INPUT, totalWidth)
        val b = UInt(INPUT, totalWidth)
        val c = UInt(INPUT, totalWidth)
        val d = UInt(INPUT, totalWidth)
        val cin = UInt(INPUT, 1)
        val e = UInt(OUTPUT, totalWidth)
      }
      val (r, cout) = fourToTwoAdder(io.a, io.b, io.c, io.d, io.cin)
      io.e := r + cout
    }

    class fourToTwoAdderOneCarryTests(c : fourToTwoAdderOneCarryTest) extends Tester(c) {
      for (i <- 0 until trials) {
        val inA = BigInt(totalWidth, r)
        val inB = BigInt(totalWidth, r)
        val inC = BigInt(totalWidth, r)
        val inD = BigInt(totalWidth, r)
        val inCin = BigInt(1, r)
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        poke(c.io.c, inC)
        poke(c.io.d, inD)
        poke(c.io.cin, inCin)
        expect(c.io.e, inA + inB + inC + inD + inCin)
      }
    }

    launchCppTester((c : fourToTwoAdderOneCarryTest) => new fourToTwoAdderOneCarryTests(c))
  }

  @Test def testfourToTwoAdderTwoCarry() {
    class fourToTwoAdderTwoCarryTest extends Module {
      val io = new Bundle {
        val a = UInt(INPUT, totalWidth)
        val b = UInt(INPUT, totalWidth)
        val c = UInt(INPUT, totalWidth)
        val d = UInt(INPUT, totalWidth)
        val cin1 = UInt(INPUT, 1)
        val cin2 = UInt(INPUT, 1)
        val e = UInt(OUTPUT, totalWidth)
      }
      val (r, cout) = fourToTwoAdder(io.a, io.b, io.c, io.d, io.cin1, io.cin2)
      io.e := r + cout
    }

    class fourToTwoAdderTwoCarryTests(c : fourToTwoAdderTwoCarryTest) extends Tester(c) {
      for (i <- 0 until trials) {
        val inA = BigInt(totalWidth, r)
        val inB = BigInt(totalWidth, r)
        val inC = BigInt(totalWidth, r)
        val inD = BigInt(totalWidth, r)
        val inCin1 = BigInt(1, r)
        val inCin2 = BigInt(1, r)
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        poke(c.io.c, inC)
        poke(c.io.d, inD)
        poke(c.io.cin1, inCin1)
        poke(c.io.cin2, inCin2)
        expect(c.io.e, inA + inB + inC + inD + inCin1 + inCin2)
      }
    }

    launchCppTester((c : fourToTwoAdderTwoCarryTest) => new fourToTwoAdderTwoCarryTests(c))
  }

  // @Test def testSignedDigitAdder() {
  //   class SignedDigitAdderTest extends Module {
  //     val io = new Bundle {
  //       val a = UInt(INPUT, totalWidth)
  //       val b = UInt(INPUT, totalWidth)
  //       val c = UInt(OUTPUT, totalWidth)
  //     }
  //     io.c := SignedDigitAdder(io.a, io.b)
  //   }

  //   class SignedDigitAdderTests(c : SignedDigitAdderTest) extends Tester(c) {
  //     for (i <- 0 until trials) {
  //       val inA = BigInt(totalWidth/4, r)
  //       val inB = BigInt(totalWidth/4, r)
  //       poke(c.io.a, toSignedDigit(inA))
  //       poke(c.io.b, toSignedDigit(inB))
  //       val res = fromSignedDigit(peek(c.io.c))
  //       expect(res == inA + inB, "Expected: " + (inA + inB).toString + "\tGot: " + res.toString)
  //     }
  //   }

  //   launchCppTester((c : SignedDigitAdderTest) => new SignedDigitAdderTests(c))
  // }

  // @Test def testSignedDigitAdder2() {
  //   class SignedDigitAdder2Test extends Module {
  //     val io = new Bundle {
  //       val a = UInt(INPUT, totalWidth)
  //       val b = UInt(INPUT, totalWidth)
  //       val c = UInt(INPUT, totalWidth)
  //       val d = UInt(OUTPUT, totalWidth)
  //     }
  //     val temp = SignedDigitAdder(io.a, io.b)
  //     io.d := SignedDigitAdder(temp, io.c)
  //   }

  //   class SignedDigitAdder2Tests(c : SignedDigitAdder2Test) extends Tester(c) {
  //     for (i <- 0 until trials) {
  //       val inA = BigInt(totalWidth/4, r)
  //       val inB = BigInt(totalWidth/4, r)
  //       val inC = BigInt(totalWidth/4, r)
  //       poke(c.io.a, toSignedDigit(inA))
  //       poke(c.io.b, toSignedDigit(inB))
  //       poke(c.io.c, toSignedDigit(inC))
  //       val res = fromSignedDigit(peek(c.io.d))
  //       expect(res == inA + inB + inC, "Expected: " + (inA + inB + inC).toString + "\tGot: " + res.toString)
  //     }
  //   }

  //   launchCppTester((c : SignedDigitAdder2Test) => new SignedDigitAdder2Tests(c))
  // }

  @Test def testSignedListToUInt() {
    val lst = List(1, -1, 0, 1, -1, 0)
    val expected = "b100100100100"
    val res = signedListToUInt(lst)
    assertTrue("Expected and Result is not equal", expected == res)

  }

  @Test def testMSDFAddMul() {
    class MSDFAddMulTest extends Module {
      val io = new Bundle {
        val a = UInt(INPUT, 2)
        val b = UInt(INPUT, 2)
        val start = Bool(INPUT)
        val c = UInt(OUTPUT, 2)
      }
      val addRes = MSDFAdd(io.a, io.b, io.start)
      val mulStart = ShiftRegister(io.start, 2)
      io.c := MSDFMul(addRes, addRes, mulStart)
    }

    class MSDFAddMulTests(c : MSDFAddMulTest) extends Tester(c) {
      for (i <- 0 until trials) {
        val dA = r.nextDouble()/4
        val dB = r.nextDouble()/4
        val a = doubleToSigned(dA, 8)
        val b = doubleToSigned(dB, 8)
        val res = new ArrayBuffer[Int]
        // Delay = 5 Mul + Add
        for (j <- 0 until a.length + 5) {
          val inA = if(j < a.length) a(j) else 0
          val inB = if(j < b.length) b(j) else 0
          poke(c.io.a, toSignedDigit(inA))
          poke(c.io.b, toSignedDigit(inB))
          val start = if (j == 0) BigInt(1) else BigInt(0)
          poke(c.io.start, start)
          if (j >= 5)
            res ++= fromSignedDigit(peek(c.io.c).toInt)
          step(1)
        }
        
        val aAddB = signedToDouble(a)+signedToDouble(b)
        val expectedRes = aAddB*aAddB
        val dRes = signedToDouble(res.toList)
        val err = scala.math.abs(expectedRes - dRes)
        val correct = if (err > scala.math.pow(2, -8)) false else true
        expect(correct, "Expected: " + expectedRes.toString  + "\tGot: " + dRes.toString + "\tError: " + err.toString)
      }
    }

    launchCppTester((c : MSDFAddMulTest) => new MSDFAddMulTests(c))
  }

  @Test def testMSDFMulAdd() {
    class MSDFMulAddTest extends Module {
      val io = new Bundle {
        val a = UInt(INPUT, 2)
        val b = UInt(INPUT, 2)
        val start = Bool(INPUT)
        val c = UInt(OUTPUT, 2)
      }
      val mulRes = MSDFMul(io.a, io.b, io.start)
      val addStart = ShiftRegister(io.start, 3)
      io.c := MSDFAdd(mulRes, mulRes, addStart)

    }

    class MSDFMulAddTests(c : MSDFMulAddTest) extends Tester(c) {
      for (i <- 0 until trials) {
        val dA = r.nextDouble()/2
        val dB = r.nextDouble()/2
        val a = doubleToSigned(dA, 8)
        val b = doubleToSigned(dB, 8)
        val res = new ArrayBuffer[Int]
        // Delay = 5 Mul + Add
        for (j <- 0 until a.length + 5) {
          val inA = if(j < a.length) a(j) else 0
          val inB = if(j < b.length) b(j) else 0
          poke(c.io.a, toSignedDigit(inA))
          poke(c.io.b, toSignedDigit(inB))
          val start = if (j == 0) BigInt(1) else BigInt(0)
          poke(c.io.start, start)
          if (j >= 5)
            res ++= fromSignedDigit(peek(c.io.c).toInt)
          step(1)
        }
        
        val aMulB = signedToDouble(a)*signedToDouble(b)
        val expectedRes = aMulB+aMulB
        val dRes = signedToDouble(res.toList)
        val err = scala.math.abs(expectedRes - dRes)
        val correct = if (err > scala.math.pow(2, -8)) false else true
        expect(correct, "Expected: " + expectedRes.toString  + "\tGot: " + dRes.toString + "\tError: " + err.toString)
      }
    }

    launchCppTester((c : MSDFMulAddTest) => new MSDFMulAddTests(c))
  }
  
  @Test def testMSDFAdd() {
    class MSDFAddTest extends Module {
      val io = new Bundle {
        val a = UInt(INPUT, 2)
        val b = UInt(INPUT, 2)
        val start = Bool(INPUT)
        val c = UInt(OUTPUT, 2)
      }
      io.c := MSDFAdd(io.a, io.b, io.start)
    }

    class MSDFAddTests(c : MSDFAddTest) extends Tester(c) {
      val digitNumber = 1
      for (i <- 0 until trials) {
        val dA = r.nextDouble()/2
        val dB = r.nextDouble()/2
        val a = doubleToSigned(dA, 8)
        val b = doubleToSigned(dB, 8)
        val res = new ArrayBuffer[Int]
        for (j <- 0 until 8 + 2*digitNumber by digitNumber) {
          val inA = if(j < a.length) a.slice(j, j+digitNumber) else List.fill(digitNumber){0}
          val inB = if(j < b.length) b.slice(j, j+digitNumber) else List.fill(digitNumber){0}
          poke(c.io.a, toSignedDigit(inA))
          poke(c.io.b, toSignedDigit(inB))
          val start = if (j == 0) BigInt(1) else BigInt(0)
          peek(c.io.c)
          poke(c.io.start, start)
          if (j >= 2*digitNumber)
            res ++= fromSignedDigit(peek(c.io.c).toInt, digitNumber)
          step(1)
        }
        val expectedRes = signedToDouble(a)+signedToDouble(b)
        val dRes = signedToDouble(res.toList)
        val err = scala.math.abs(expectedRes - dRes)
        val correct = if (err > scala.math.pow(2, -8)) false else true
        expect(correct, "Expected: " + expectedRes.toString + "(" + (dA+dB).toString + ")" + "\tGot: " + dRes.toString + "\tError: " + err.toString)
      }
    }

    launchCppTester((c : MSDFAddTest) => new MSDFAddTests(c))
  }

  @Test def testMSDFSub() {
    class MSDFSubTest extends Module {
      val io = new Bundle {
        val a = UInt(INPUT, 2)
        val b = UInt(INPUT, 2)
        val start = Bool(INPUT)
        val c = UInt(OUTPUT, 2)
      }
      io.c := MSDFSub(io.a, io.b, io.start)
    }

    class MSDFSubTests(c : MSDFSubTest) extends Tester(c) {
      val digitNumber = 1
      for (i <- 0 until trials) {
        val dA = r.nextDouble()/2
        val dB = r.nextDouble()/2
        val a = doubleToSigned(dA, 8)
        val b = doubleToSigned(dB, 8)
        val res = new ArrayBuffer[Int]
        for (j <- 0 until 8 + 2*digitNumber by digitNumber) {
          val inA = if(j < a.length) a.slice(j, j+digitNumber) else List.fill(digitNumber){0}
          val inB = if(j < b.length) b.slice(j, j+digitNumber) else List.fill(digitNumber){0}
          poke(c.io.a, toSignedDigit(inA))
          poke(c.io.b, toSignedDigit(inB))
          val start = if (j == 0) BigInt(1) else BigInt(0)
          peek(c.io.c)
          poke(c.io.start, start)
          if (j >= 2*digitNumber)
            res ++= fromSignedDigit(peek(c.io.c).toInt, digitNumber)
          step(1)
        }
        val expectedRes = signedToDouble(a)-signedToDouble(b)
        val dRes = signedToDouble(res.toList)
        val err = scala.math.abs(expectedRes - dRes)
        val correct = if (err > scala.math.pow(2, -8)) false else true
        expect(correct, "Expected: " + expectedRes.toString + "(" + (dA+dB).toString + ")" + "\tGot: " + dRes.toString + "\tError: " + err.toString)
      }
    }

    launchCppTester((c : MSDFSubTest) => new MSDFSubTests(c))
  }

  @Test def testMSDFMul() {
    class MSDFMulTest extends Module {
      val io = new Bundle {
        val a = UInt(INPUT, 2)
        val b = UInt(INPUT, 2)
        val start = Bool(INPUT)
        val c = UInt(OUTPUT, 2)
      }
      io.c := MSDFMul(io.a, io.b, io.start)
    }

    class MSDFMulTests(c : MSDFMulTest) extends Tester(c) {

      for (i <- 0 until trials) {
        val dA = r.nextDouble()/2
        val dB = r.nextDouble()/2
        val a = doubleToSigned(dA, 8)
        val b = doubleToSigned(dB, 8)
        val res = new ArrayBuffer[Int]
        for (j <- 0 until a.length + 3) {
          val inA = if(j < a.length) a(j) else 0
          val inB = if(j < b.length) b(j) else 0
          poke(c.io.a, toSignedDigit(inA))
          poke(c.io.b, toSignedDigit(inB))
          val start = if (j == 0) BigInt(1) else BigInt(0)
          poke(c.io.start, start)
          peek(c.io.c)
          if (j >= 3)
            res ++= fromSignedDigit(peek(c.io.c).toInt)
          step(1)
        }
        val expectedRes = signedToDouble(a)*signedToDouble(b)
        val dRes = signedToDouble(res.toList)
        val err = scala.math.abs(expectedRes - dRes)
        val correct = if (err > scala.math.pow(2, -8)) false else true
        expect(correct, "Expected: " + expectedRes.toString + "(" + (dA*dB).toString + ")" + "\tGot: " + dRes.toString + "\tError: " + err.toString)
      }
    }
    launchCppTester((c : MSDFMulTest) => new MSDFMulTests(c))
  }

  @Test def testMSDFDotProduct() {
    class MSDFDotProductTest extends Module {
      val io = new Bundle {
        val a = Vec.fill(2){UInt(INPUT, 2)}
        val b = Vec.fill(2){UInt(INPUT, 2)}
        val start = Bool(INPUT)
        val c = UInt(OUTPUT, 2)
      }

      val threeDelay = ShiftRegister(io.start, 3)
      val dotProduct = (io.a, io.b).zipped.map((ai, bi) => MSDFMul(ai, bi, io.start)).reduce((r, c) => MSDFAdd(r, c, threeDelay))

      io.c := dotProduct
    }

    class MSDFDotProductTests(c : MSDFDotProductTest) extends Tester(c) {

      for (i <- 0 until trials) {
        val dA = List.fill(2){r.nextDouble()/2}
        val dB = List.fill(2){r.nextDouble()/2}
        val a = dA.map(in => doubleToSigned(in, 8))
        val b = dB.map(in => doubleToSigned(in, 8))
        val res = new ArrayBuffer[Int]
        for (j <- 0 until a(0).length + 5) {
          for (k <- 0 until 2) {
            val inA = if(j < a(k).length) a(k)(j) else 0
            val inB = if(j < b(k).length) b(k)(j) else 0
            poke(c.io.a(k), toSignedDigit(inA))
            poke(c.io.b(k), toSignedDigit(inB))
          }
          val start = if (j == 0) BigInt(1) else BigInt(0)
          poke(c.io.start, start)
          peek(c.io.c)
          if (j >= 5)
            res ++= fromSignedDigit(peek(c.io.c).toInt)
          step(1)
        }
        val expectedRes = (dA, dB).zipped.map(_*_).reduce(_+_)
        val dRes = signedToDouble(res.toList)
        val err = scala.math.abs(expectedRes - dRes)
        val correct = if (err > scala.math.pow(2, -7)) false else true
        expect(correct, "Expected: " + expectedRes.toString + "\tGot: " + dRes.toString + "\tError: " + err.toString)
      }
    }
    launchCppTester((c : MSDFDotProductTest) => new MSDFDotProductTests(c))
  }

  @Test def testMSDFLiteral() {
    class MSDFLiteralTest extends Module {
      val io = new Bundle {
        val c = UInt(OUTPUT, 2)
      }
      io.c := MSDFLiteral(UInt("b10011001"))
    }

    class MSDFLiteralTests(c : MSDFLiteralTest) extends Tester(c) {
      for (i <- 0 until trials) {
        val res = List(2, 1, 2, 1)
        for (j <- 0 until 4) {
          expect(c.io.c, res(j))
          step(1)
        }
      }
    }
    launchCppTester((c : MSDFLiteralTest) => new MSDFLiteralTests(c))
  }

  @Test def testMSDFLiteralInitial() {
    class MSDFLiteralInitialTest extends Module {
      val io = new Bundle {
        val c = UInt(OUTPUT, 2)
      }
      io.c := MSDFLiteral(UInt("b10011001"), 1)
    }

    class MSDFLiteralInitialTests(c : MSDFLiteralInitialTest) extends Tester(c) {
      for (i <- 0 until trials) {
        val res = List(1, 2, 1, 2)
        for (j <- 0 until 4) {
          expect(c.io.c, res(j))
          step(1)
        }
      }
    }
    launchCppTester((c : MSDFLiteralInitialTest) => new MSDFLiteralInitialTests(c))
  }


  object MSDFRegister {
    def apply(a : UInt) : Vec[UInt] = {
      val literalWidth = a.getWidth()/2
      val litArray = new ArrayBuffer[UInt]
      for (i <- a.getWidth() - 1 to 1 by -2) {
        val reg = Reg(init=a(i, i - 1))
        litArray.append(reg)
      }
      Vec(litArray)
    }

    def apply(a : Vec[UInt]) : UInt = {
      val literalWidth = a.length
      val counter = Reg(init=UInt(0, width=log2Up(literalWidth)))
      counter := counter + UInt(1)
      a(counter)
    }

    def apply(in : Vec[UInt], index : UInt) = in(index)
  }

  @Test def testMSDFRegister() {
    class MSDFRegisterTest extends Module {
      val io = new Bundle {
        val a = UInt(INPUT, 2)
        val update = Bool(INPUT)
        val c = UInt(OUTPUT, 2)
      }
      val wVec = MSDFRegister(UInt("b0010011000011000"))
      val w = MSDFRegister(wVec)
      when (io.update) { 
        w := io.a 
        wVec(UInt(0)) := UInt(1)
      }
      io.c := w
    }

    class MSDFRegisterTests(c : MSDFRegisterTest) extends Tester(c) {
      val res1 = List(0, 2, 1, 2, 0, 1, 2, 0)
      for (j <- 0 until 8) {
        poke(c.io.a, 1)
        if (j == 3) poke(c.io.update, 1) else poke(c.io.update, 0)
        expect(c.io.c, res1(j))
        step(1)
      }
      val res2 = List(1, 2, 1, 1, 0, 1, 2, 0)
      for (j <- 0 until 8) {
        poke(c.io.a, 1)
        poke(c.io.update, 0)
        expect(c.io.c, res2(j))
        step(1)
      }
    }
    launchCppTester((c : MSDFRegisterTest) => new MSDFRegisterTests(c))
  }


  object updateCounter {
    def apply(rst : UInt, w : Int) : UInt = {
      val counter = Reg(init=UInt(0, width=w))
      when (rst === UInt(1)) {counter := UInt(0)}
      counter := counter + UInt(1)
      counter
    }
  }

  object nextStart {
    def apply(start : Bool, delay : Int) : UInt = {
      val nsr = Reg(init=Cat(UInt(1), Fill(UInt(0), delay - 1)))
      when (start) {
        nsr := Cat(UInt(1), Fill(UInt(0), delay - 2))
      } .otherwise {
        nsr := nsr >> 1
      }
      nsr 
    }
  }

  @Test def testMSDFAddAccum() {
    class MSDFAddAccumTest extends Module {
      val io = new Bundle {
        val a = UInt(INPUT, 2)
        val start = Bool(INPUT)
        val c = UInt(OUTPUT, 2)
      }
      val wVec = MSDFRegister(UInt("b0000000000000000"))
      val w = MSDFRegister(wVec)
      val addRes = MSDFAdd(io.a, w, io.start)
      val nextAddStart = nextStart(io.start, 2)
      val wCounter = updateCounter(nextAddStart, wVec.length)
      wVec(wCounter) := addRes
      io.c := addRes
    }

    class MSDFAddAccumTests(c : MSDFAddAccumTest) extends Tester(c) {
      var dB = 0.0
      for (i <- 0 until 5) {
        val dA = r.nextDouble()/8
        val a = doubleToSigned(dA, 8)
        val b = doubleToSigned(dB, 8)
        val res = new ArrayBuffer[Int]
        for (j <- 0 until 8 + 2) {
          val inA = if(j < a.length) a(j) else 0
          poke(c.io.a, toSignedDigit(inA))
          val start = if (j == 0) BigInt(1) else BigInt(0)
          poke(c.io.start, start)
          if (j >= 2)
            res ++= fromSignedDigit(peek(c.io.c).toInt)
          step(1)
        }
        val expectedRes = signedToDouble(a)+signedToDouble(b)
        val dRes = signedToDouble(res.toList)
        dB = dRes
        val err = scala.math.abs(expectedRes - dRes)
        val correct = if (err > scala.math.pow(2, -8)) false else true
        expect(correct, "Expected: " + expectedRes.toString + "(" + (dA+dB).toString + ")" + "\tGot: " + dRes.toString + "\tError: " + err.toString)
      }
    }
    launchCppTester((c : MSDFAddAccumTest) => new MSDFAddAccumTests(c))
  }

  @Test def testMSDFLMSForward() {
    class MSDFLMSTest extends Module {
      val io = new Bundle {
        val x = Vec.fill(2){UInt(INPUT, 2)}
        val inW = Vec.fill(2){UInt(INPUT, 2)}
        val y = UInt(INPUT, 2)
        val stepSize = UInt(INPUT, 2)
        val start = Bool(INPUT)
        val ybar = UInt(OUTPUT, 2)
        val outErr = UInt(OUTPUT, 2)
        val outStep = UInt(OUTPUT, 2)
        val outW = Vec.fill(2){UInt(OUTPUT, 2)}
      }
      val threeDelay = ShiftRegister(io.start, 3)
      val dotProduct = (io.x, io.inW).zipped.map((ai, bi) => MSDFMul(ai, bi, io.start)).reduce((r, c) => MSDFAdd(r, c, threeDelay)) // 5 Delay

      io.ybar := dotProduct

      val yDelay = UInt(width=2)
      yDelay := ShiftRegister(io.y, 5)
      val fiveDelay = ShiftRegister(io.start, 5)
      val err = MSDFSub(yDelay, dotProduct, fiveDelay) // 2 Delay (7 Total)
      io.outErr := err

      val stepDelay = UInt(width=2)
      stepDelay := ShiftRegister(io.stepSize, 7)
      val sevenDelay = ShiftRegister(io.start, 7)
      val step = MSDFMul(stepDelay, err, sevenDelay) // 3 Delay (10 Total)
      io.outStep := step

      val tenDelay = ShiftRegister(io.start, 10)
      val xDelay = Vec.fill(2){UInt(width=2)}
      xDelay := ShiftRegister(io.x, 10)
      val wDelay = Vec.fill(2){UInt(width=2)}
      wDelay := ShiftRegister(io.inW, 13)
      val thirteenDelay = ShiftRegister(io.start, 13)
      val wUpdate = (wDelay, xDelay.map(x1 => MSDFMul(x1, step, tenDelay))).zipped.map((w1, x1) => MSDFAdd(w1, x1, thirteenDelay)) // 5 Delay (15 Total)

      io.outW := wUpdate

    }

    class MSDFLMSTests(c : MSDFLMSTest) extends Tester(c) {

      def compare(expectedRes : Double, dRes : Double) {
        val err = scala.math.abs(expectedRes - dRes)
        val correct = if (err > scala.math.pow(2, -8)) false else true
        expect(correct, "Expected: " + expectedRes.toString + "\tGot: " + dRes.toString + "\tError: " + err.toString)
      }

      def update(x : List[Double], y : Double, w : List[Double], stepSize : Double) = {
        val yBar = (x, w).zipped.map(_*_).reduce(_+_)
        val err = y - yBar
        val step = stepSize*err
        val wUpdate = (x.map(step*_), w).zipped.map(_+_)
        (yBar, wUpdate, err, step)
      }

      val digitSize = 16
      val ybarDelay = 5
      val errDelay = 7
      val stepDelay = 10
      val wUpdateDelay = 15

      val stepSize : Double = 0.125

      for (i <- 0 until trials) {

        val dX : List[Double] = List.fill(2){r.nextDouble()/2}
        val dW : List[Double] = List.fill(2){r.nextDouble()/2}
        val dY : Double = r.nextDouble()/2
        val x = dX.map(in => doubleToSigned(in, digitSize))
        val w = dW.map(in => doubleToSigned(in, digitSize))
        val y = doubleToSigned(dY, digitSize)
        val stepS = doubleToSigned(stepSize, digitSize)

        val wRes = List.fill(2){new ArrayBuffer[Int]}
        val yRes = new ArrayBuffer[Int]
        val errRes = new ArrayBuffer[Int]
        val stepRes = new ArrayBuffer[Int]

        for (j <- 0 until digitSize + wUpdateDelay) {
          // X & W Input
          for (k <- 0 until 2) {
           val inX = if(j < digitSize) x(k)(j) else 0
           val inW = if(j < digitSize) w(k)(j) else 0
           poke(c.io.x(k), toSignedDigit(inX)) 
           poke(c.io.inW(k), toSignedDigit(inW)) 
          }

          // Y Input
          val inY = if(j < digitSize) y(j) else 0
          poke(c.io.y, toSignedDigit(inY))

          // Step Input
          val inStep = if(j < digitSize) stepS(j) else 0
          poke(c.io.stepSize, toSignedDigit(inStep))

          // Start Input
          val start = if (j == 0) 1 else 0
          poke(c.io.start, start)


          if ((j >= ybarDelay) & (j < ybarDelay+digitSize))
            yRes ++= fromSignedDigit(peek(c.io.ybar).toInt)

          if ((j >= errDelay) & (j < errDelay+digitSize))
            errRes ++= fromSignedDigit(peek(c.io.outErr).toInt)

          if ((j >= stepDelay) & (j < stepDelay+digitSize))
            stepRes ++= fromSignedDigit(peek(c.io.outStep).toInt)

          if ((j >= wUpdateDelay) & (j < wUpdateDelay+digitSize)) {
            for (k <- 0 until 2) {
              wRes(k) ++= fromSignedDigit(peek(c.io.outW(k)).toInt)
            }
          }

          step(1)
        }

        // Expected Results
        val (yBar, wUpdate, lmsErr, lmsStep) = update(dX, dY, dW, stepSize)

        // yBar
        println("yBar:")
        compare(yBar, signedToDouble(yRes.toList))

        // lmsErr
        println("lmsErr:")
        compare(lmsErr, signedToDouble(errRes.toList))

        // lmsStep
        println("lmsStep:")
        compare(lmsStep, signedToDouble(stepRes.toList))

        // w
        for (j <- 0 until 2) {
          println("w(" + j.toString + "): ")
          compare(wUpdate(j), signedToDouble(wRes(j).toList))
        }
      }
    }
    launchCppTester((c : MSDFLMSTest) => new MSDFLMSTests(c))
  }

  @Test def testMSDFLMS() {
    class MSDFLMSTest extends Module {
      val io = new Bundle {
        val x = Vec.fill(2){UInt(INPUT, 2)}
        val inW = Vec.fill(2){UInt(INPUT, 2)}
        val y = UInt(INPUT, 2)
        val stepSize = UInt(INPUT, 2)
        val start = Bool(INPUT)
        val ybar = UInt(OUTPUT, 2)
        val outErr = UInt(OUTPUT, 2)
        val outStep = UInt(OUTPUT, 2)
        val outW = Vec.fill(2){UInt(OUTPUT, 2)}
      }
      val threeDelay = ShiftRegister(io.start, 3)
      val dotProduct = (io.x, io.inW).zipped.map((ai, bi) => MSDFMul(ai, bi, io.start)).reduce((r, c) => MSDFAdd(r, c, threeDelay)) // 5 Delay

      io.ybar := dotProduct

      val yDelay = UInt(width=2)
      yDelay := ShiftRegister(io.y, 5)
      val fiveDelay = ShiftRegister(io.start, 5)
      val err = MSDFSub(yDelay, dotProduct, fiveDelay) // 2 Delay (7 Total)
      io.outErr := err

      val stepDelay = UInt(width=2)
      stepDelay := ShiftRegister(io.stepSize, 7)
      val sevenDelay = ShiftRegister(io.start, 7)
      val step = MSDFMul(stepDelay, err, sevenDelay) // 3 Delay (10 Total)
      io.outStep := step

      val tenDelay = ShiftRegister(io.start, 10)
      val xDelay = Vec.fill(2){UInt(width=2)}
      xDelay := ShiftRegister(io.x, 10)
      val wDelay = Vec.fill(2){UInt(width=2)}
      wDelay := ShiftRegister(io.inW, 13)
      val thirteenDelay = ShiftRegister(io.start, 13)
      val wUpdate = (wDelay, xDelay.map(x1 => MSDFMul(x1, step, tenDelay))).zipped.map((w1, x1) => MSDFAdd(w1, x1, thirteenDelay)) // 5 Delay (15 Total)

      io.outW := wUpdate

    }

    class MSDFLMSTests(c : MSDFLMSTest) extends Tester(c) {

      def compare(expectedRes : Double, dRes : Double, count : Int) {
        if (count != 0) {
          val err = scala.math.abs(expectedRes - dRes)
          val correct = if (err > scala.math.pow(2, -8)) false else true
          expect(correct, "Expected: " + expectedRes.toString + "\tGot: " + dRes.toString + "\tError: " + err.toString)
        }
      }

      def update(x : List[Double], y : Double, w : List[Double], stepSize : Double) = {
        val yBar = (x, w).zipped.map(_*_).reduce(_+_)
        val err = y - yBar
        val step = stepSize*err
        val wUpdate = (x.map(step*_), w).zipped.map(_+_)
        (yBar, wUpdate, err, step)
      }

      val digitSize = 16
      val ybarDelay = 5
      val errDelay = 7
      val stepDelay = 10
      val wUpdateDelay = 15

      val stepSize : Double = 0.125

      val yBarRes = new scala.collection.mutable.Queue[Double]
      val wUpdateRes = new scala.collection.mutable.Queue[List[Double]]
      val lmsErrRes = new scala.collection.mutable.Queue[Double]
      val lmsStepRes = new scala.collection.mutable.Queue[Double]

      val yQ = new scala.collection.mutable.Queue[Double]
      val xQ = new scala.collection.mutable.Queue[List[Double]]

      // Counters
      var yBarCount = digitSize - ybarDelay
      var errCount = digitSize - errDelay
      var stepCount = digitSize - stepDelay 
      var wUpdateCount = digitSize - wUpdateDelay
      var compareCount = 0

      var updateW : Boolean = false
      var updateY : Boolean = false
      var updateErr : Boolean = false
      var updateStep : Boolean = false

      val w = List.fill(2){ArrayBuffer.fill(digitSize){0}}
      var prevW = List.fill(2){ArrayBuffer.fill(digitSize){0}}
      var yRes = new ArrayBuffer[Int]
      var errRes = new ArrayBuffer[Int]
      var stepRes = new ArrayBuffer[Int]

      for (i <- 0 until trials*10) {


        val dX : List[Double] = List.fill(2){r.nextDouble()/2}
        val dY : Double = dX.reduce(_*_)
        val x = dX.map(in => doubleToSigned(in, digitSize))
        xQ.enqueue(x.map(in => signedToDouble(in)))
        val y = doubleToSigned(dY, digitSize)
        yQ.enqueue(signedToDouble(y))
        val stepS = doubleToSigned(stepSize, digitSize)


        // Expected Results
        if (i != 0) {
          val (yBar, wUpdate, lmsErr, lmsStep) = update(xQ.dequeue(), yQ.dequeue(), prevW.map(w1 => signedToDouble(w1.toList)), stepSize)
          yBarRes.enqueue(yBar)
          wUpdateRes.enqueue(wUpdate)
          lmsErrRes.enqueue(lmsErr)
          lmsStepRes.enqueue(lmsStep)
        }

        println("New Input")
        println("yBarCount: " + yBarCount.toString)
        println("errCount: " + errCount.toString)
        println("stepCount: " + stepCount.toString)
        println("wUpdateCount: " + wUpdateCount.toString)

        for (j <- 0 until digitSize) {
          // X & W Input
          for (k <- 0 until 2) {
           val inX = if(j < digitSize) x(k)(j) else 0
           val inW = if(j < digitSize) w(k)(j) else 0
           poke(c.io.x(k), toSignedDigit(inX)) 
           poke(c.io.inW(k), toSignedDigit(inW)) 
          }

          // Y Input
          val inY = if(j < digitSize) y(j) else 0
          poke(c.io.y, toSignedDigit(inY))

          // Step Input
          val inStep = if(j < digitSize) stepS(j) else 0
          poke(c.io.stepSize, toSignedDigit(inStep))

          // Start Input
          val start = if (j == 0) 1 else 0
          poke(c.io.start, start)

          if (updateY) {
            yRes ++= fromSignedDigit(peek(c.io.ybar).toInt)
          }

          if (updateErr) {
            errRes ++= fromSignedDigit(peek(c.io.outErr).toInt)
          }

          if (updateStep){
            stepRes ++= fromSignedDigit(peek(c.io.outStep).toInt)
          }

          if (updateW) {
            for (k <- 0 until 2) {
              w(k)(wUpdateCount) = fromSignedDigit(peek(c.io.outW(k)).toInt)(0)
            }
          }

         yBarCount = if(yBarCount == digitSize -1) 0 else yBarCount + 1
         errCount = if(errCount == digitSize -1) 0 else errCount + 1
         stepCount = if(stepCount == digitSize -1) 0 else stepCount + 1
         wUpdateCount = if(wUpdateCount == digitSize -1) 0 else wUpdateCount + 1
         if(wUpdateCount == digitSize -1) {
          updateW = true
          prevW = w
        }
         if(yBarCount == digitSize -1) updateY = true
         if(errCount == digitSize -1) updateErr = true
         if(stepCount == digitSize -1) updateStep = true

         if (i != 0) {
           if (yBarCount == 0) {
            // yBar
            println("yBar:")
            println(yRes.toList)
            compare(yBarRes.dequeue(), signedToDouble(yRes.toList), compareCount)
            yRes = new ArrayBuffer[Int]
           }
           if (errCount == 0) {
            // lmsErr
            println("lmsErr:")
            println(errRes.toList)
            compare(lmsErrRes.dequeue(), signedToDouble(errRes.toList), compareCount)
            errRes = new ArrayBuffer[Int]
           }

           if (stepCount == 0) {
            // lmsStep
            println("lmsStep:")
            println(stepRes.toList)
            compare(lmsStepRes.dequeue(), signedToDouble(stepRes.toList), compareCount)
            stepRes = new ArrayBuffer[Int]
           }

           if (wUpdateCount == 0) {
            // w
            val wRes = wUpdateRes.dequeue()
            for (j <- 0 until 2) {
              println("w(" + j.toString + "): ")
              println(w(j).toList)
              compare(wRes(j), signedToDouble(w(j).toList), compareCount)
            }
            compareCount += 1
           }
          }

          step(1)
        }


      }
    }
    launchCppTester((c : MSDFLMSTest) => new MSDFLMSTests(c))
  }

}