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

class PrimitivesSuite extends TestSuite {
  
  val trials = 10
  val r = scala.util.Random
  val totalWidth = 16



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
}