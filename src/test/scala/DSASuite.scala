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

  def fromSignedDigit(a : Int) : Int = fromSignedDigit(BigInt(a)).toInt
  def fromSignedDigit(a : BigInt) : BigInt = {
    var signedDigits = new ArrayBuffer[Int]
    var i = 0
    while (i < a.bitLength) {
      if(a.testBit(i+1) && a.testBit(i)) {
        signedDigits.append(0)
      } else if(a.testBit(i+1) && !a.testBit(i)) {
        signedDigits.append(1)
      } else if(!a.testBit(i+1) && a.testBit(i)) {
        signedDigits.append(-1)
      } else {
        signedDigits.append(0)
      }
      i += 2
    }
    var res : BigInt = BigInt(0)
    for (i <- 0 until signedDigits.length) {
      res += signedDigits(i)*scala.math.pow(2, i).toInt
    }
    res
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
      val t = Vec.fill(a.getWidth()/2 - 1){Reg(init=UInt(width=1))}
      val w = Vec.fill(a.getWidth()/2){Reg(init=UInt(width=1))}
      val tTrans = UInt(width=1)
      val (w0, t1) = if (a.getWidth()/2 != 1) {
        FullAdder(v(0), ~bn(0), h(0))
      } else {
        FullAdder(v(0), ~bn(0), hTrans)
      }
      tTrans := ~t1
      w(0) := w0

      for (i <- 1 until a.getWidth()/2 - 1) {
        val (w0, t1) = FullAdder(v(i), ~bn(i), h(i))
        t(i - 1) := ~t1
        w(i) := w0
      }
      if (a.getWidth()/2 != 1) {
        val (w1, t2) = FullAdder(v(a.getWidth()/2 - 1), ~bn(a.getWidth()/2 - 1), hTrans)
        t(a.getWidth()/2 - 2) := ~t2
        w(a.getWidth()/2 - 1) := w1
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
      inA := MuxCase(UInt(0), Array(
        (b === UInt("b01")) -> ~caA,
        (b === UInt("b10")) -> caA
        ))

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
      MuxCase(UInt(0), Array(
        (a === UInt("b000")) -> UInt("b00"),
        (a === UInt("b001")) -> UInt("b10"),
        (a === UInt("b010")) -> UInt("b10"),
        (a === UInt("b011")) -> UInt("b10"),
        (a === UInt("b100")) -> UInt("b01"),
        (a === UInt("b101")) -> UInt("b01"),
        (a === UInt("b110")) -> UInt("b01"),
        (a === UInt("b111")) -> UInt("b00")
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

  @Test def testToSignedDigit() {
    val a = BigInt(1)
    val b = toSignedDigit(a)
    assertTrue(b == BigInt(2))

    val c = 3
    val d = toSignedDigit(c)
    assertTrue(d == BigInt(10))
  }

  @Test def testFromSignedDigit() {
    val a = BigInt(1)
    val b = fromSignedDigit(a)
    assertTrue(b == BigInt(-1))

    val c = 3
    val d = fromSignedDigit(c)
    assertTrue(d == BigInt(0))

    val e = 2
    val f = fromSignedDigit(e)
    assertTrue(f == BigInt(1))
  }

  @Test def testConversion() {
    for (i <- 0 until trials) {
      val a = BigInt(r.nextInt(100000))
      val b = fromSignedDigit(toSignedDigit(a))
      assertTrue(a == b)
    }
  }

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

  @Test def testMSDFAdd() {
    class MSDFAddTest extends Module {
      val io = new Bundle {
        val a = UInt(INPUT, digit)
        val b = UInt(INPUT, digit)
        val c = UInt(OUTPUT, digit)
      }
      io.c := MSDFAdd(io.a, io.b)
    }

    class MSDFAddTests(c : MSDFAddTest) extends Tester(c) {
      for (i <- 0 until trials) {
        val a = BigInt(r.nextInt(1 << totalWidth - 2))
        val b = BigInt(r.nextInt(1 << totalWidth - 2))
        val inA = toSignedDigit(a)
        val inB = toSignedDigit(b)
        var result = BigInt(0)
        val bigN = n*2
        for (j <- 0 until bigN + 2) {
          val currA = (inA >> (digit*(bigN - j - 1)) & 0xF)
          val currB = (inB >> (digit*(bigN - j - 1)) & 0xF)
          poke(c.io.a, currA)
          poke(c.io.b, currB)
          val res = peek(c.io.c)
          step(1)
          if (j > 1) {
            for (k <- 0 until digit) {
              val set = if ((res & BigInt(1 << (digit - 1 - k))) == BigInt(scala.math.pow(2, (digit - 1 - k)).toInt)) true else false
              result = if (set) result.setBit(digit*(bigN - j + 2) - k - 1) else result
            }
          }
        }
        val expectedResult = a + b
        val expected = if(expectedResult == fromSignedDigit(result)) true else false
        expect(expected, "Expected: " + expectedResult.toString + "\tGot: " + fromSignedDigit(result).toString)
      }
    }

    launchCppTester((c : MSDFAddTest) => new MSDFAddTests(c))
  }

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
            res.append(fromSignedDigit(peek(c.io.c).toInt))
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
            res.append(fromSignedDigit(peek(c.io.c).toInt))
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
      for (i <- 0 until trials) {
        val dA = r.nextDouble()/2
        val dB = r.nextDouble()/2
        val a = doubleToSigned(dA, 8)
        val b = doubleToSigned(dB, 8)
        val res = new ArrayBuffer[Int]
        for (j <- 0 until 8 + 2) {
          val inA = if(j < a.length) a(j) else 0
          val inB = if(j < b.length) b(j) else 0
          poke(c.io.a, toSignedDigit(inA))
          poke(c.io.b, toSignedDigit(inB))
          val start = if (j == 0) BigInt(1) else BigInt(0)
          poke(c.io.start, start)
          if (j >= 2)
            res.append(fromSignedDigit(peek(c.io.c).toInt))
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
            res.append(fromSignedDigit(peek(c.io.c).toInt))
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

  // @Test def testMSDFDotProduct() {
  //   class MSDFDotProductTest extends Module {
  //     val io = new Bundle {
  //       val a = Vec.fill(2){UInt(INPUT, 2)}
  //       val b = Vec.fill(2){UInt(INPUT, 2)}
  //       val start = Bool(INPUT)
  //       val c = UInt(OUTPUT, 2)
  //     }

  //     val threeDelay = ShiftRegister(io.start, 3)
  //     val dotProduct = (io.a, io.b).zipped.map((ai, bi) => MSDFMul(ai, bi, io.start)).reduce((r, c) => MSDFAdd(r, c, threeDelay))

  //     io.c := dotProduct
  //   }

  //   class MSDFDotProductTests(c : MSDFDotProductTest) extends Tester(c) {

  //     for (i <- 0 until trials) {
  //       val dA = List.fill(2){r.nextDouble()/2}
  //       val dB = List.fill(2){r.nextDouble()/2}
  //       val a = dA.map(in => doubleToSigned(in, 8))
  //       val b = dB.map(in => doubleToSigned(in, 8))
  //       val res = new ArrayBuffer[Int]
  //       for (j <- 0 until a(0).length + 5) {
  //         for (k <- 0 until 2) {
  //           val inA = if(j < a(k).length) a(k)(j) else 0
  //           val inB = if(j < b(k).length) b(k)(j) else 0
  //           poke(c.io.a(k), toSignedDigit(inA))
  //           poke(c.io.b(k), toSignedDigit(inB))
  //         }
  //         val start = if (j == 0) BigInt(1) else BigInt(0)
  //         poke(c.io.start, start)
  //         peek(c.io.c)
  //         if (j >= 5)
  //           res.append(fromSignedDigit(peek(c.io.c).toInt))
  //         step(1)
  //       }
  //       val expectedRes = (dA, dB).zipped.map(_*_).reduce(_+_)
  //       val dRes = signedToDouble(res.toList)
  //       val err = scala.math.abs(expectedRes - dRes)
  //       val correct = if (err > scala.math.pow(2, -8)) false else true
  //       expect(correct, "Expected: " + expectedRes.toString + "\tGot: " + dRes.toString + "\tError: " + err.toString)
  //     }
  //   }
  //   launchCppTester((c : MSDFDotProductTest) => new MSDFDotProductTests(c))
  // }

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
            res.append(fromSignedDigit(peek(c.io.c).toInt))
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

  // @Test def testMSDFDotProduct() {
  //   class MSDFDotProductTest extends Module {
  //     val io = new Bundle {
  //       val x = Vec.fill(2){UInt(INPUT, 2)}
  //       val y = UInt(INPUT, 2)
  //       val start = Bool(INPUT)
  //       val c = UInt(OUTPUT, 2)
  //     }

  //     val wVec = Vec.fill(2){MSDFRegister(UInt("b0000000000000000"))}
  //     val w = Vec(wVec.map(MSDFRegister(_))
  //     val threeDelay = ShiftRegister(io.start, 3)
  //     val dotProduct = (io.a, w).zipped.map((ai, bi) => MSDFMul(ai, bi, io.start)).reduce((r, c) => MSDFAdd(r, c, threeDelay))

  //     val yDelay = ShiftRegister(y, 5)

  //     val err = dotProduct - yDelay
  //     io.c := dotProduct
  //   }

  //   class MSDFDotProductTests(c : MSDFDotProductTest) extends Tester(c) {

  //     for (i <- 0 until trials) {
  //       val dA = List.fill(2){r.nextDouble()/2}
  //       val dB = List.fill(2){r.nextDouble()/2}
  //       val a = dA.map(in => doubleToSigned(in, 8))
  //       val b = dB.map(in => doubleToSigned(in, 8))
  //       val res = new ArrayBuffer[Int]
  //       for (j <- 0 until a(0).length + 5) {
  //         for (k <- 0 until 2) {
  //           val inA = if(j < a(k).length) a(k)(j) else 0
  //           val inB = if(j < b(k).length) b(k)(j) else 0
  //           poke(c.io.a(k), toSignedDigit(inA))
  //           poke(c.io.b(k), toSignedDigit(inB))
  //         }
  //         val start = if (j == 0) BigInt(1) else BigInt(0)
  //         poke(c.io.start, start)
  //         peek(c.io.c)
  //         if (j >= 5)
  //           res.append(fromSignedDigit(peek(c.io.c).toInt))
  //         step(1)
  //       }
  //       val expectedRes = (dA, dB).zipped.map(_*_).reduce(_+_)
  //       val dRes = signedToDouble(res.toList)
  //       val err = scala.math.abs(expectedRes - dRes)
  //       val correct = if (err > scala.math.pow(2, -8)) false else true
  //       expect(correct, "Expected: " + expectedRes.toString + "\tGot: " + dRes.toString + "\tError: " + err.toString)
  //     }
  //   }
  //   launchCppTester((c : MSDFDotProductTest) => new MSDFDotProductTests(c))
  // }

}