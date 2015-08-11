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


/** This testsuite checks all methods in the Bits class.
*/
class LSDFSuite extends TestSuite {
  val trials = 1
  val digit = 4
  val width = 32
  val n = width/digit

  @Test def testWidth() {
    val res = LSDF(INPUT, width, digit)
    assert(res.getWidth() == digit)
  }

  @Test def testTotalWidth() {
    val res = LSDF(INPUT, width, digit)
    assert(res.getTotalWidth() == width)
  }

  @Test def testWire() {
    class LSDFWire extends Module {
      val io = new Bundle {
        val a = LSDF(INPUT, width, digit)
        val b = LSDF(OUTPUT, width, digit)
      }
      io.b := io.a
    }

    class LSDFWireTests(c : LSDFWire) extends Tester(c) {
      val r = scala.util.Random
      for (i <- 0 until trials) {
        val inA = BigInt(r.nextInt(1 << digit))
        poke(c.io.a, inA)
        expect(c.io.b, inA)
      }
    }

    launchCppTester((c: LSDFWire) => new LSDFWireTests(c))
  }

  @Test def testGT() {
    class LSDFGT extends Module {
      val io = new Bundle {
        val a = LSDF(INPUT, width, digit)
        val b = LSDF(INPUT, width, digit)
        val gt = Bool(OUTPUT)
      }
      io.gt := io.a > io.b
    }

    class LSDFGTTests(c : LSDFGT) extends Tester(c) {
      val r = scala.util.Random
      var prevResult = Bool(true).litValue()
      var count = 0
      for (i <- 0 until trials) {
        val inA = BigInt(r.nextInt(1 << digit))
        val inB = BigInt(r.nextInt(1 << digit))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        prevResult = Bool(inA > inB).litValue() & prevResult
        expect(c.io.gt, prevResult)
        step(1)
        count += 1
        if (count == n) {
          count = 0
          prevResult = Bool(true).litValue()
        }
      }
    }

    launchCppTester((c: LSDFGT) => new LSDFGTTests(c))
  }
  
  @Test def testLT() {
    class LSDFLT extends Module {
      val io = new Bundle {
        val a = LSDF(INPUT, width, digit)
        val b = LSDF(INPUT, width, digit)
        val lt = Bool(OUTPUT)
      }
      io.lt := io.a < io.b
    }

    class LSDFLTTests(c : LSDFLT) extends Tester(c) {
      val r = scala.util.Random
      var prevResult = Bool(true).litValue()
      var count = 0
      for (i <- 0 until trials) {
        val inA = BigInt(r.nextInt(1 << digit))
        val inB = BigInt(r.nextInt(1 << digit))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        prevResult = Bool(inA < inB).litValue() & prevResult
        expect(c.io.lt, prevResult)
        step(1)
        count += 1
        if (count == n) {
          count = 0
          prevResult = Bool(true).litValue()
        }
      }
    }

    launchCppTester((c: LSDFLT) => new LSDFLTTests(c))
  }
  
  @Test def testGTE() {
    class LSDFGTE extends Module {
      val io = new Bundle {
        val a = LSDF(INPUT, width, digit)
        val b = LSDF(INPUT, width, digit)
        val gt = Bool(OUTPUT)
      }
      io.gt := io.a >= io.b
    }

    class LSDFGTETests(c : LSDFGTE) extends Tester(c) {
      val r = scala.util.Random
      var prevResult = Bool(true).litValue()
      var count = 0
      for (i <- 0 until trials) {
        val inA = BigInt(r.nextInt(1 << digit))
        val inB = BigInt(r.nextInt(1 << digit))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        prevResult = Bool(inA >= inB).litValue() & prevResult
        expect(c.io.gt, prevResult)
        step(1)
        count += 1
        if (count == n) {
          count = 0
          prevResult = Bool(true).litValue()
        }
      }
    }

    launchCppTester((c: LSDFGTE) => new LSDFGTETests(c))
  }

  @Test def testLTE() {
    class LSDFLTE extends Module {
      val io = new Bundle {
        val a = LSDF(INPUT, width, digit)
        val b = LSDF(INPUT, width, digit)
        val lt = Bool(OUTPUT)
      }
      io.lt := io.a <= io.b
    }

    class LSDFLTETests(c : LSDFLTE) extends Tester(c) {
      val r = scala.util.Random
      var prevResult = Bool(true).litValue()
      var count = 0
      for (i <- 0 until trials) {
        val inA = BigInt(r.nextInt(1 << digit))
        val inB = BigInt(r.nextInt(1 << digit))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        prevResult = Bool(inA <= inB).litValue() & prevResult
        expect(c.io.lt, prevResult)
        step(1)
        count += 1
        if (count == n) {
          count = 0
          prevResult = Bool(true).litValue()
        }
      }
    }

    launchCppTester((c: LSDFLTE) => new LSDFLTETests(c))
  }

  @Test def testCarry() {

  def fullAdder(a : UInt, b : UInt, cin : UInt) : (UInt, UInt) = ((a ^ b) ^ cin, (a & b) | (a & cin) | (b & cin))

  def carryAdd(a : UInt, b : UInt, cin : UInt) : (UInt, UInt) = {
    val cout = Vec.fill(b.getWidth + 1){UInt(width=1)}
    cout(0) := cin
    val res = Vec.fill(b.getWidth){UInt(width=1)}
    for (i <- 0 until b.getWidth) {
        val (r, c) = fullAdder(a(i), b(i), cout(i))
        res(i) := r
        cout(i+1) := c
    }
    (res.toBits.toUInt, cout(b.getWidth))
  }
    
  class LSDFCarry extends Module {
      val io = new Bundle {
        val a = UInt(INPUT, 2)
        val b = UInt(INPUT, 2)
        val res = UInt(OUTPUT, 2)
        val carry = UInt(OUTPUT, 1)
      }
      val (res, cout) = carryAdd(io.a, io.b, UInt(0))
      io.carry := cout
      io.res := res
    }

    class LSDFCarryTests(c : LSDFCarry) extends Tester(c) {
      val inA = BigInt(0x4)
      val inB = BigInt(0x8)
      poke(c.io.a, inA)
      poke(c.io.b, inB)
      expect(c.io.res, BigInt(0xc))
      expect(c.io.carry, BigInt(0))
    }

    launchCppTester((c: LSDFCarry) => new LSDFCarryTests(c))
  }

  @Test def testAdder() {
    class LSDFAdder extends Module {
      val io = new Bundle {
        val a = LSDF(INPUT, width, digit)
        val b = LSDF(INPUT, width, digit)
        val res = LSDF(OUTPUT, width, digit)
      }
      io.res := io.a + io.b
    }

    class LSDFAdderTests(c : LSDFAdder) extends Tester(c) {
      val r = scala.util.Random
      var prevResult = 0
      var count = 0
      for (i <- 0 until trials) {
        // val inA = BigInt(r.nextInt(1 << digit))
        // val inB = BigInt(r.nextInt(1 << digit))
        val inA = BigInt(0x4)
        val inB = BigInt(0x8)
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        var res = inA + inB + prevResult
        prevResult = if (res >= BigInt(scala.math.pow(2, digit).toInt)) 1 else 0
        expect(c.io.res, res)
        step(1)
        count += 1
        if (count == n) {
          println("Reset")
          count = 0
          prevResult = 0
        }
      }
    }

    launchCppTester((c: LSDFAdder) => new LSDFAdderTests(c))
  }


  @Test def testAdd() {
    class LSDFAdd extends Module {
      val io = new Bundle {
        val a = LSDF(INPUT, width, digit)
        val b = LSDF(INPUT, width, digit)
        val res = LSDF(OUTPUT, width, digit)
      }
      io.res := io.a + io.b
    }

    class LSDFAddTests(c : LSDFAdd) extends Tester(c) {
      val r = scala.util.Random
      var prevResult = 0
      var count = 0
      for (i <- 0 until trials) {
        val inA = BigInt(r.nextInt(1 << width - 2))
        val inB = BigInt(r.nextInt(1 << width - 2))
        var result = BigInt(0)
        for (j <- 0 until n) {
          val currA = (inA & BigInt(0xF << (digit*j))) >> (digit*j)
          val currB = (inB & BigInt(0xF << (digit*j))) >> (digit*j)
          poke(c.io.a, currA)
          poke(c.io.b, currB)
          val res = peek(c.io.res)
          step(1)
          for (k <- 0 until digit) {
            val set = if ((res & BigInt(1 << k)) == BigInt(scala.math.pow(2, k).toInt)) true else false
            result = if (set) result.setBit(digit*j + k) else result
          }
        }
        val expectedResult = inA + inB
        val expected = if ((inA + inB) == result) true else false
        expect(expected, "Expected: " + expectedResult.toString + "\tGot: " + result.toString)
      }
    }

    launchCppTester((c: LSDFAdd) => new LSDFAddTests(c))
  }
  
  @Test def testSub() {
    class LSDFSub extends Module {
      val io = new Bundle {
        val a = LSDF(INPUT, width, digit)
        val b = LSDF(INPUT, width, digit)
        val res = LSDF(OUTPUT, width, digit)
      }
      io.res := io.a - io.b
    }

    class LSDFSubTests(c : LSDFSub) extends Tester(c) {
      val r = scala.util.Random
      var prevResult = 1
      var count = 0
      for (i <- 0 until trials) {
        var inA = BigInt(r.nextInt(1 << width - 2))
        var inB = BigInt(r.nextInt(1 << width - 2))
        // LSDF Currently only supports UInt
        while (inA < inB) {
          inA = BigInt(r.nextInt(1 << width - 2))
          inB = BigInt(r.nextInt(1 << width - 2))
        }
        var result = BigInt(0)
        for (j <- 0 until n) {
          val currA = (inA & BigInt(0xF << (digit*j))) >> (digit*j)
          val currB = (inB & BigInt(0xF << (digit*j))) >> (digit*j)
          poke(c.io.a, currA)
          poke(c.io.b, currB)
          val res = peek(c.io.res)
          step(1)
          for (k <- 0 until digit) {
            val set = if ((res & BigInt(1 << k)) == BigInt(scala.math.pow(2, k).toInt)) true else false
            result = if (set) result.setBit(digit*j + k) else result
          }
        }
        val expectedResult = inA - inB
        val expected = if (expectedResult == result) true else false
        expect(expected, "Expected: " + expectedResult.toString + "\tGot: " + result.toString)
      }
    }
    
    launchCppTester((c: LSDFSub) => new LSDFSubTests(c))
  }

  @Test def testMul() {
    class LSDFMul extends Module {
      val io = new Bundle {
        val a = LSDF(INPUT, width, digit)
        val b = LSDF(INPUT, width, digit)
        val res = LSDF(OUTPUT, width, digit)
      }
      io.res := io.a * io.b
    }
    class LSDFMulTests(c : LSDFMul) extends Tester(c) {
      val r = scala.util.Random
      var prevResult = 1
      var count = 0
      for (i <- 0 until trials) {
        val inA = BigInt(r.nextInt(1 << (width - 1)/2))
        val inB = BigInt(r.nextInt(1 << (width - 1)/2))
        var result = BigInt(0)
        for (j <- 0 until n) {
          val currA = (inA & BigInt(0xF << (digit*j))) >> (digit*j)
          val currB = (inB & BigInt(0xF << (digit*j))) >> (digit*j)
          poke(c.io.a, currA)
          poke(c.io.b, currB)
          val res = peek(c.io.res)
          step(1)
          for (k <- 0 until digit) {
            val set = if ((res & BigInt(1 << k)) == BigInt(scala.math.pow(2, k).toInt)) true else false
            result = if (set) result.setBit(digit*j + k) else result
          }
        }
        val expectedResult = inA * inB
        val expected = if (expectedResult == result) true else false
        expect(expected, "Expected: " + expectedResult.toString + " (" + expectedResult.toInt.toHexString + ")" + "\tGot: " + result.toString + " (" + result.toInt.toHexString + ")")
      }
    }
    
    launchCppTester((c: LSDFMul) => new LSDFMulTests(c))
  }
  
  @Test def testBasic() {
    class LSDFBasic extends Module {
      val io = new Bundle {
        val a = LSDF(INPUT, width, digit)
        val b = LSDF(INPUT, width, digit)
        val res = LSDF(OUTPUT, width, digit)
      }
      val temp = io.a * io.b
      io.res := temp + io.a
    }

    class LSDFBasicTests(c : LSDFBasic) extends Tester(c) {
      val r = scala.util.Random
      var prevResult = 1
      var count = 0
      for (i <- 0 until trials) {
        val inA = BigInt(r.nextInt(1 << (width - 1)/2))
        val inB = BigInt(r.nextInt(1 << (width - 1)/2))
        var result = BigInt(0)
        for (j <- 0 until n) {
          val currA = (inA & BigInt(0xF << (digit*j))) >> (digit*j)
          val currB = (inB & BigInt(0xF << (digit*j))) >> (digit*j)
          poke(c.io.a, currA)
          poke(c.io.b, currB)
          val res = peek(c.io.res)
          step(1)
          for (k <- 0 until digit) {
            val set = if ((res & BigInt(1 << k)) == BigInt(scala.math.pow(2, k).toInt)) true else false
            result = if (set) result.setBit(digit*j + k) else result
          }
        }
        val expectedResult = (inA * inB) + inA
        val expected = if (expectedResult == result) true else false
        expect(expected, "Expected: " + expectedResult.toString + " (" + expectedResult.toInt.toHexString + ")" + "\tGot: " + result.toString + " (" + result.toInt.toHexString + ")")
      }
    }
    
    launchCppTester((c: LSDFBasic) => new LSDFBasicTests(c))
  }

  @Test def testBasicReg() {
    class LSDFBasicReg extends Module {
      val io = new Bundle {
        val a = LSDF(INPUT, width, digit)
        val b = LSDF(INPUT, width, digit)
        val res = LSDF(OUTPUT, width, digit)
      }
      val temp = Reg(next=io.a + io.b)
      val aReg = Reg(next=io.a)

      io.res := temp * aReg
    }

    class LSDFBasicRegTests(c : LSDFBasicReg) extends Tester(c) {
      val r = scala.util.Random
      var prevResult = 1
      var count = 0
      for (i <- 0 until trials) {
        val inA = BigInt(r.nextInt(1 << (width - 1)/2))
        val inB = BigInt(r.nextInt(1 << (width - 1)/2))
        var result = BigInt(0)
        var resultAdd = BigInt(0)
        for (j <- 0 until n) {
          val currA = (inA & BigInt(0xF << (digit*j))) >> (digit*j)
          val currB = (inB & BigInt(0xF << (digit*j))) >> (digit*j)
          poke(c.io.a, currA)
          poke(c.io.b, currB)
          step(1)
          val res = peek(c.io.res)
          for (k <- 0 until digit) {
            val set = if ((res & BigInt(1 << k)) == BigInt(scala.math.pow(2, k).toInt)) true else false
            result = if (set) result.setBit(digit*j + k) else result
          }
        }
        val expectedResult = (inA + inB) * inA
        val expected = if (expectedResult == result) true else false
        expect(expected, "Expected: " + expectedResult.toString + " (" + expectedResult.toInt.toHexString + ")" + "\tGot: " + result.toString + " (" + result.toInt.toHexString + ")")
      }
    }
    
    launchCppTester((c: LSDFBasicReg) => new LSDFBasicRegTests(c))
  }

  @Test def testBundleReg() {
    class LSDFBundleReg extends Module {
      val io = new Bundle {
        val a = LSDF(INPUT, width, digit)
        val b = LSDF(INPUT, width, digit)
        val res = LSDF(OUTPUT, width, digit)
      }
      val temp = Reg(next=io.a + io.b)
      val aReg = Reg(next=io.a)
      val mul = Module(new LSDFMulT()).io
      mul.a := temp
      mul.b := aReg
      io.res := mul.res
    }

    class LSDFMulT extends Module {
      val io = new Bundle {
        val a = LSDF(INPUT, width, digit)
        val b = LSDF(INPUT, width, digit)
        val res = LSDF(OUTPUT, width, digit)
      }
      io.res := io.a * io.b
    }

    class LSDFBundleRegTests(c : LSDFBundleReg) extends Tester(c) {
      val r = scala.util.Random
      var prevResult = 1
      var count = 0
      for (i <- 0 until trials) {
        val inA = BigInt(r.nextInt(1 << (width - 1)/2))
        val inB = BigInt(r.nextInt(1 << (width - 1)/2))
        var result = BigInt(0)
        var resultAdd = BigInt(0)
        for (j <- 0 until n) {
          val currA = (inA & BigInt(0xF << (digit*j))) >> (digit*j)
          val currB = (inB & BigInt(0xF << (digit*j))) >> (digit*j)
          poke(c.io.a, currA)
          poke(c.io.b, currB)
          step(1)
          val res = peek(c.io.res)
          for (k <- 0 until digit) {
            val set = if ((res & BigInt(1 << k)) == BigInt(scala.math.pow(2, k).toInt)) true else false
            result = if (set) result.setBit(digit*j + k) else result
          }
        }
        val expectedResult = (inA + inB) * inA
        val expected = if (expectedResult == result) true else false
        expect(expected, "Expected: " + expectedResult.toString + " (" + expectedResult.toInt.toHexString + ")" + "\tGot: " + result.toString + " (" + result.toInt.toHexString + ")")
      }
    }
    
    launchCppTester((c: LSDFBundleReg) => new LSDFBundleRegTests(c))
  }

  @Test def testGetDelay() {
    class LSDFGetDelay extends Module {



      def breadthFirst(visited : List[Node], end : Node) {
        val startList : List[Node] = visited.last.inputs.toList
        for (node <- startList) {
          if (!visited.contains(node)) {
            if (node._id == end._id) {
              val completePath : List[Node] = visited :+ node
              println("Completed Path Found: " + completePath.filter(_.isReg).length) 
            }
          }
        }

        for (node <- startList) {
          if (!visited.contains(node) && !(node._id == end._id)) {
            val nextPath = visited :+ node
            breadthFirst(nextPath, end)
          }
        }
      }


      def getDelay(a : Node, b : Node) : Int = {
        println("Finding Node Delay")
        println("Node a ID: " + a._id + "\tNode b ID: " + b._id)
        breadthFirst(List(a), b)
        1
      }

      val io = new Bundle {
        val a = LSDF(INPUT, width, digit)
        val en = Bool(INPUT)
        val res = LSDF(OUTPUT, width, digit)
      }
      val reg1 = Reg(next=io.a)
      val reg2 = Reg(next=reg1)
      val reg3 = Reg(next=Mux(io.en,reg2, reg1))
      println(getDelay(reg1, io.a))
      println(getDelay(reg2, io.a))
      println(getDelay(reg2, reg1))
      println(getDelay(reg1, reg2))
      println(getDelay(reg3, io.a))
      println(getDelay(io.a, reg3))
      io.res := reg3
    }


    class LSDFGetDelayTests(c : LSDFGetDelay) extends Tester(c) {
      val r = scala.util.Random
      var prevResult = 1
      var count = 0
      for (i <- 0 until trials) {
        val inA = BigInt(r.nextInt(1 << (width - 1)/2))
      }
    }
    
    launchCppTester((c: LSDFGetDelay) => new LSDFGetDelayTests(c))
  }

  @Test def testLiteral {
    class LSDFLiteralTest extends Module {
      val io = new Bundle {
        val a = LSDF(OUTPUT, width, digit)
      }
      io.a := LSDF(1112, width, digit)
    }
    class LSDFLiteralTestTests(c : LSDFLiteralTest) extends Tester(c) {
      val x = BigInt(1112)
      expect(c.io.a, x)
    }
    launchCppTester((c: LSDFLiteralTest) => new LSDFLiteralTestTests(c))
  }
}
