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
  val trials = 100
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

  //def fullAdder(a : UInt, b : UInt, cin : UInt) : (UInt, UInt) = 

  //def carryAdd(a : UInt, b : UInt, cin : UInt) : (UInt, UInt) = {
    //(res, cout(b.getWidth))
  //}
    
  class LSDFCarry extends Module {
      val io = new Bundle {
        val a = UInt(INPUT, 2)
        val b = UInt(INPUT, 2)
        val res = UInt(OUTPUT, 2)
        val carry = UInt(OUTPUT, 1)
      }
    val cout = Vec.fill(io.b.getWidth + 1){Bool()}
    val res = Vec.fill(io.b.getWidth){Bool()}
    for (i <- 0 until io.b.getWidth) {
        val r = (io.a(i) ^ io.b(i)) ^ cout(i)
        val c = (io.a(i) & io.b(i)) | (io.a(i) & cout(i)) | (io.b(i) & cout(i))
        res(i) := r
        cout(i+1) := c
    }
      io.carry := cout(io.b.getWidth).toUInt
      io.res := res.toBits.toUInt
    }

    class LSDFCarryTests(c : LSDFCarry) extends Tester(c) {
      val inA = BigInt(2)
      val inB = BigInt(2)
      poke(c.io.a, inA)
      poke(c.io.b, inB)
      expect(c.io.res, BigInt(0))
      expect(c.io.carry, BigInt(1))
    }

    launchCppTester((c: LSDFCarry) => new LSDFCarryTests(c))
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
        val inA = BigInt(r.nextInt(1 << digit))
        val inB = BigInt(r.nextInt(1 << digit))
        //val inA = BigInt(3)
        //val inB = BigInt(13)
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

    launchCppTester((c: LSDFAdd) => new LSDFAddTests(c))
  }
  
  //@Test def testSub() {
    //class LSDFSub extends Module {
      //val io = new Bundle {
        //val a = LSDF(INPUT, width, digit)
        //val b = LSDF(INPUT, width, digit)
        //val res = LSDF(OUTPUT, width, digit)
      //}
      //io.res := io.a - io.b
    //}

    //class LSDFSubTests(c : LSDFSub) extends Tester(c) {
      //val r = scala.util.Random
      //var prevResult = 1
      //var count = 0
      //for (i <- 0 until trials) {
        //val inA = ArrayBuffer[Int].fill(n){r.nextInt(1)}
        //val inB = ArrayBuffer[Int].fill(n){r.nextInt(1)}
        //val res = ArrayBuffer[Int].fill(n){0}
        ////val inA = BigInt(r.nextInt(1 << digit))
        ////val inB = BigInt(r.nextInt(1 << digit))
        //for (j <- 0 until n) {
            //poke(c.io.a, inA(j))
            //poke(c.io.b, inB(j))
            //res(j) = peek(c.io.res)
            //step(1)
        //}
        //poke(c.io.a, inA)
        //poke(c.io.b, inB)
        //var res = inA - inB - prevResult
        //prevResult = if (res >= BigInt(scala.math.pow(2, digit).toInt)) 1 else 0
        //expect(c.io.res, res)
        //step(1)
        //count += 1
        //if (count == n) {
          //println("Reset")
          //count = 0
          //prevResult = 1
        //}
      //}
    //}

    //launchCppTester((c: LSDFSub) => new LSDFSubTests(c))
  //}
}
