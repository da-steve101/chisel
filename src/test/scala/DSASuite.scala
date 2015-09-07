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

  /* Pipelined Adder and Subtractor Implementations */
  
  object FullWidthDigitAdder {
    def apply(a : UInt, b : UInt, digit : Int) : UInt = {
      val stages = a.getWidth()/digit
      val aRegs = Vec.fill(stages - 1){Reg(init=UInt(width=a.getWidth()))}
      val bRegs = Vec.fill(stages - 1){Reg(init=UInt(width=b.getWidth()))}
      val rRegs = Vec.fill(stages - 1){Reg(init=UInt(width=digit*(stages - 1)))}
      val cRegs = Vec.fill(stages - 1){Reg(init=UInt(0, width=1))}
      
      val (r, c) = RippleCarryAdder(a(digit - 1, 0), b(digit - 1, 0), UInt(0))
      rRegs(0) := r
      cRegs(0) := c
      aRegs(0) := a >> UInt(digit)
      bRegs(0) := b >> UInt(digit)

      for (i <- 1 until stages - 1) {
        val (r, c) = RippleCarryAdder(aRegs(i - 1)(digit - 1, 0), bRegs(i - 1)(digit - 1, 0), cRegs(i - 1))
        rRegs(i) := Cat(r, rRegs(i-1)(digit*i - 1, 0))
        cRegs(i) := c
        aRegs(i) := aRegs(i - 1) >> UInt(digit)
        bRegs(i) := bRegs(i - 1) >> UInt(digit)
      }

      val (lr, lc) = RippleCarryAdder(aRegs.last, bRegs.last, cRegs.last)
      Cat(lr, rRegs.last((stages - 1)*digit-1, 0))
    }
  }

  object FullWidthDigitSubtractor {
    def apply(a : UInt, b : UInt, digit : Int) : UInt = {
      val stages = a.getWidth()/digit
      val aRegs = Vec.fill(stages - 1){Reg(init=UInt(width=a.getWidth()))}
      val bRegs = Vec.fill(stages - 1){Reg(init=UInt(width=b.getWidth()))}
      val rRegs = Vec.fill(stages - 1){Reg(init=UInt(width=digit*(stages - 1)))}
      val cRegs = Vec.fill(stages - 1){Reg(init=UInt(0, width=1))}
      
      val (r, c) = RippleCarryAdder(a(digit - 1, 0), ~b(digit - 1, 0), UInt(1))
      rRegs(0) := r
      cRegs(0) := c
      aRegs(0) := a >> UInt(digit)
      bRegs(0) := b >> UInt(digit)

      for (i <- 1 until stages - 1) {
        val (r, c) = RippleCarryAdder(aRegs(i - 1)(digit - 1, 0), ~bRegs(i - 1)(digit - 1, 0), cRegs(i - 1))
        rRegs(i) := Cat(r, rRegs(i-1)(digit*i - 1, 0))
        cRegs(i) := c
        aRegs(i) := aRegs(i - 1) >> UInt(digit)
        bRegs(i) := bRegs(i - 1) >> UInt(digit)
      }

      val (lr, lc) = RippleCarryAdder(aRegs.last, ~bRegs.last, cRegs.last)
      Cat(lr, rRegs.last((stages - 1)*digit-1, 0))
    }
  } 

  @Test def testFullWidthDigitAdder() {
    class DigitAdder extends Module {
      val digit = 8
      val io = new Bundle {
        val a = UInt(INPUT, 16)
        val b = UInt(INPUT, 16)
        val c = UInt(OUTPUT, 16)
      }
      io.c := FullWidthDigitAdder(io.a, io.b, digit)
    }

    class DigitAdderTests(c : DigitAdder) extends Tester(c) {

      // Fill the Pipeline
      val res =  new scala.collection.mutable.Queue[BigInt]
      val stages = c.io.a.getWidth()/c.digit - 1
      var start = 0
      for (j <- 0 until stages) {
        val inA = BigInt(r.nextInt(1 << 14))
        val inB = BigInt(r.nextInt(1 << 14))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        res.enqueue(inA + inB)
        step(1)
      }
      for (i <- 0 until trials) {
        expect(c.io.c, res.dequeue())
        val inA = BigInt(r.nextInt(1 << 14))
        val inB = BigInt(r.nextInt(1 << 14))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        res.enqueue(inA + inB)
        step(1)
      }
    }

    launchCppTester((c: DigitAdder) => new DigitAdderTests(c))
  }

  @Test def testFullWidthDigitSubtractor() {
    class DigitSubtractor extends Module {
      val digit = 8
      val io = new Bundle {
        val a = UInt(INPUT, 16)
        val b = UInt(INPUT, 16)
        val c = UInt(OUTPUT, 16)
      }
      io.c := FullWidthDigitSubtractor(io.a, io.b, digit)
    }

    class DigitSubtractorTests(c : DigitSubtractor) extends Tester(c) {

      // Fill the Pipeline
      val res =  new scala.collection.mutable.Queue[BigInt]
      val stages = c.io.a.getWidth()/c.digit - 1
      var start = 0
      for (j <- 0 until stages) {
        var inA = BigInt(r.nextInt(1 << totalWidth - 2))
        var inB = BigInt(r.nextInt(1 << totalWidth - 2))
        while (inB > inA) {
          inA = BigInt(r.nextInt(1 << totalWidth - 2))
          inB = BigInt(r.nextInt(1 << totalWidth - 2))
        }
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        res.enqueue(inA - inB)
        step(1)
      }
      for (i <- 0 until trials) {
        expect(c.io.c, res.dequeue())
        var inA = BigInt(r.nextInt(1 << totalWidth - 2))
        var inB = BigInt(r.nextInt(1 << totalWidth - 2))
        while (inB > inA) {
          inA = BigInt(r.nextInt(1 << totalWidth - 2))
          inB = BigInt(r.nextInt(1 << totalWidth - 2))
        }
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        res.enqueue(inA - inB)
        step(1)
      }
    }

    launchCppTester((c: DigitSubtractor) => new DigitSubtractorTests(c))
  }

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

  @Test def testLSDFAdd() {
    class LSDFAddTest extends Module {
      val io = new Bundle {
        val a = UInt(INPUT, digit)
        val b = UInt(INPUT, digit)
        val c = UInt(OUTPUT, digit)
      }
      io.c := LSDFAdd(io.a, io.b, n, 0)
    }

    class LSDFAddTests(c : LSDFAddTest) extends Tester(c) {
      for (i <- 0 until trials) {
        val inA = BigInt(r.nextInt(1 << totalWidth - 2))
        val inB = BigInt(r.nextInt(1 << totalWidth - 2))
        var result = BigInt(0)
        for (j <- 0 until n) {
          val currA = (inA & BigInt(0xF << (digit*j))) >> (digit*j)
          val currB = (inB & BigInt(0xF << (digit*j))) >> (digit*j)
          poke(c.io.a, currA)
          poke(c.io.b, currB)
          val res = peek(c.io.c)
          step(1)
          for (k <- 0 until digit) {
            val set = if ((res & BigInt(1 << k)) == BigInt(scala.math.pow(2, k).toInt)) true else false
            result = if (set) result.setBit(digit*j + k) else result
          }
        }
        val expectedResult = inA + inB
        val expected = if(expectedResult == result) true else false
        expect(expected, "Expected: " + expectedResult.toString + "\tGot: " + result.toString)
      }
    }

    launchCppTester((c : LSDFAddTest) => new LSDFAddTests(c))
  }

  @Test def testLSDFSub() {
    class LSDFSubTest extends Module {
      val io = new Bundle {
        val a = UInt(INPUT, digit)
        val b = UInt(INPUT, digit)
        val c = UInt(OUTPUT, digit)
      }
      io.c := LSDFSub(io.a, io.b, n, 0)
    }

    class LSDFSubTests(c : LSDFSubTest) extends Tester(c) {
      for (i <- 0 until trials) {
        var inA = BigInt(r.nextInt(1 << totalWidth - 2))
        var inB = BigInt(r.nextInt(1 << totalWidth - 2))
        while (inB > inA) {
          inA = BigInt(r.nextInt(1 << totalWidth - 2))
          inB = BigInt(r.nextInt(1 << totalWidth - 2))
        }
        var result = BigInt(0)
        for (j <- 0 until n) {
          val currA = (inA & BigInt(0xF << (digit*j))) >> (digit*j)
          val currB = (inB & BigInt(0xF << (digit*j))) >> (digit*j)
          poke(c.io.a, currA)
          poke(c.io.b, currB)
          val res = peek(c.io.c)
          step(1)
          for (k <- 0 until digit) {
            val set = if ((res & BigInt(1 << k)) == BigInt(scala.math.pow(2, k).toInt)) true else false
            result = if (set) result.setBit(digit*j + k) else result
          }
        }
        val expectedResult = inA - inB
        val expected = if(expectedResult == result) true else false
        expect(expected, "Expected: " + expectedResult.toString + "\tGot: " + result.toString)
      }
    }

    launchCppTester((c : LSDFSubTest) => new LSDFSubTests(c))
  }

  @Test def testLSDFMul() {
    class LSDFMulTest extends Module {
      val io = new Bundle {
        val a = UInt(INPUT, digit)
        val b = UInt(INPUT, digit)
        val c = UInt(OUTPUT, digit)
      }
      io.c := LSDFMul(io.a, io.b, n, 0)
    }

    class LSDFMulTests(c : LSDFMulTest) extends Tester(c) {
      for (i <- 0 until trials) {
        val inA = BigInt(r.nextInt(1 << totalWidth/2 - 2))
        val inB = BigInt(r.nextInt(1 << totalWidth/2 - 2))
        var result = BigInt(0)
        for (j <- 0 until n) {
          val currA = (inA & BigInt(0xF << (digit*j))) >> (digit*j)
          val currB = (inB & BigInt(0xF << (digit*j))) >> (digit*j)
          poke(c.io.a, currA)
          poke(c.io.b, currB)
          val res = peek(c.io.c)
          step(1)
          for (k <- 0 until digit) {
            val set = if ((res & BigInt(1 << k)) == BigInt(scala.math.pow(2, k).toInt)) true else false
            result = if (set) result.setBit(digit*j + k) else result
          }
        }
        val expectedResult = inA * inB
        val expected = if(expectedResult == result) true else false
        expect(expected, "Expected: " + expectedResult.toString + "\tGot: " + result.toString)
      }
    }

    launchCppTester((c : LSDFMulTest) => new LSDFMulTests(c))
  }

  /* Most Significant Digit Implementations */

  /* Signed Digit Representation
   * a = 1, r = 2
   * S = {-1, 0, 1}
   *   00 = 0
   *   01 = -1
   *   10 = 1
   *   11 = 0
   */

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
    def apply(a : UInt, b : UInt, c : UInt, d : UInt, cin : UInt) : (UInt, UInt) =  {
      val vs = Vec.fill(a.getWidth()){UInt(width=1)}
      val vc = Vec.fill(a.getWidth() + 1){UInt(width=1)}
      vc(0) := cin

      val cT = Vec.fill(a.getWidth() + 1){UInt(width=1)}
      val sT = Vec.fill(a.getWidth()){UInt(width=1)}
      cT(0) := UInt(0)

      // Two Layers of Adders
      for (i <- 0 until a.getWidth()) {
        val (s0, c0) = FullAdder(a(i), b(i), c(i))
        val (s1, c1) = FullAdder(s0, d(i), cT(i))
        cT(i+1) := c0
        vs(i) := s1
        vc(i+1) := c1
      }

      (vs.toBits.toUInt, vc.toBits.toUInt)
    }

    def apply(a : UInt, b : UInt, c : UInt, d : UInt) : (UInt, UInt) = fourToTwoAdder(a, b, c, d, UInt(0))
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
    def apply(a : UInt, b : UInt) = {
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
      val (w0, t1) = if (a.getWidth() != 1) {
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
      if (a.getWidth() != 1) {
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
      s(0) := tTrans
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

    def SEL(a : UInt) : UInt = {
      MuxCase(UInt(0), Array(
        (a === UInt("b000")) -> UInt(0),
        (a === UInt("b001")) -> UInt(10),
        (a === UInt("b010")) -> UInt(10),
        (a === UInt("b011")) -> UInt(10),
        (a === UInt("b100")) -> UInt(1),
        (a === UInt("b101")) -> UInt(1),
        (a === UInt("b110")) -> UInt(1),
        (a === UInt("b111")) -> UInt(0)
        ))
    }
  }

  object SDOnlineConversion {
    def apply(a : UInt) : UInt = {
      val q = Reg(init=UInt(0, width=totalWidth))
      val qm = Reg(init=UInt(0, width=totalWidth))

      val negOne = a === UInt("b01")
      val one = a === UInt("b10")
      val zero = !negOne & !one

      val nextQM = MuxCase(UInt(0), Array(
        one -> Cat(q, UInt(0)),
        zero -> Cat(qm, UInt(1)),
        negOne -> Cat(qm, UInt(0))
      ))

      val nextQ = MuxCase(UInt(0), Array(
        one -> Cat(q, UInt(1)),
        zero -> Cat(q, UInt(0)),
        negOne -> Cat(qm, UInt(1))
      ))

      q := nextQ
      qm := nextQM

      nextQ
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
        val c = UInt(OUTPUT, totalWidth)
      }
      io.c := SDOnlineConversion(io.a)
    }

    class SDOnlineConversionTests(c : SDOnlineConversionTest) extends Tester(c) {
      val q = List(1, 1, 0, 1, -1, 0, 0, -1, 1, 0, 1, 0)
      val qAns = List(UInt("b1").litValue(), 
        UInt("b11").litValue(), 
        UInt("b110").litValue(), 
        UInt("b1101").litValue(), 
        UInt("b11001").litValue(),
        UInt("b110010").litValue(),
        UInt("b1100100").litValue(),
        UInt("b11000111").litValue(),
        UInt("b110001111").litValue(),
        UInt("b1100011110").litValue(),
        UInt("b11000111101").litValue(),
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
        val e = UInt(OUTPUT, totalWidth)
      }
      val (r, cout) = fourToTwoAdder(io.a, io.b, io.c, io.d, UInt(0))
      io.e := r + cout
    }

    class fourToTwoAdderOneCarryTests(c : fourToTwoAdderOneCarryTest) extends Tester(c) {
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

    launchCppTester((c : fourToTwoAdderOneCarryTest) => new fourToTwoAdderOneCarryTests(c))
  }

  @Test def testSignedDigitAdder() {
    class SignedDigitAdderTest extends Module {
      val io = new Bundle {
        val a = UInt(INPUT, totalWidth)
        val b = UInt(INPUT, totalWidth)
        val c = UInt(OUTPUT, totalWidth)
      }
      io.c := SignedDigitAdder(io.a, io.b)
    }

    class SignedDigitAdderTests(c : SignedDigitAdderTest) extends Tester(c) {
      for (i <- 0 until trials) {
        val inA = BigInt(totalWidth/4, r)
        val inB = BigInt(totalWidth/4, r)
        poke(c.io.a, toSignedDigit(inA))
        poke(c.io.b, toSignedDigit(inB))
        val res = fromSignedDigit(peek(c.io.c))
        expect(res == inA + inB, "Expected: " + (inA + inB).toString + "\tGot: " + res.toString)
      }
    }

    launchCppTester((c : SignedDigitAdderTest) => new SignedDigitAdderTests(c))
  }

  @Test def testSignedDigitAdder2() {
    class SignedDigitAdder2Test extends Module {
      val io = new Bundle {
        val a = UInt(INPUT, totalWidth)
        val b = UInt(INPUT, totalWidth)
        val c = UInt(INPUT, totalWidth)
        val d = UInt(OUTPUT, totalWidth)
      }
      val temp = SignedDigitAdder(io.a, io.b)
      io.d := SignedDigitAdder(temp, io.c)
    }

    class SignedDigitAdder2Tests(c : SignedDigitAdder2Test) extends Tester(c) {
      for (i <- 0 until trials) {
        val inA = BigInt(totalWidth/4, r)
        val inB = BigInt(totalWidth/4, r)
        val inC = BigInt(totalWidth/4, r)
        poke(c.io.a, toSignedDigit(inA))
        poke(c.io.b, toSignedDigit(inB))
        poke(c.io.c, toSignedDigit(inC))
        val res = fromSignedDigit(peek(c.io.d))
        expect(res == inA + inB + inC, "Expected: " + (inA + inB + inC).toString + "\tGot: " + res.toString)
      }
    }

    launchCppTester((c : SignedDigitAdder2Test) => new SignedDigitAdder2Tests(c))
  }

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
}