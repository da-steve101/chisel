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
    
  def toFixedT(x : Double, fracWidth : Int) : BigInt = BigInt((x*scala.math.pow(2, fracWidth)).toInt)
  def toFixed(x : Double, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
  def toDouble(x : BigInt, fracWidth : Int) : Double = x.toDouble/scala.math.pow(2, fracWidth)

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

  val trials = 10
  val r = scala.util.Random

  @Test def testBasic() {
    class Basic extends Module {
      val io = new Bundle {
        val a = Fixed(INPUT, 16, 8)
        val b = Fixed(INPUT, 16, 8)
        val c = Fixed(OUTPUT, 16, 8)
      }
      io.c := io.a + io.b
    }

    class BasicTests(c : Basic) extends Tester(c) {
      for (i <- 0 until trials) {
        val inA = BigInt(r.nextInt(1 << 14))
        val inB = BigInt(r.nextInt(1 << 14))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        expect(c.io.c, inA + inB)
      }
    }

    launchCppTester((c: Basic) => new BasicTests(c))
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

  val totalWidth = 16
  val digit = 4
  val n = totalWidth/digit

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
}