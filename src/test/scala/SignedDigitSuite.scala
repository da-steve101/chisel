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

class SignedDigitSuite extends TestSuite {
  
  val trials = 10
  val r = scala.util.Random
  val totalWidth = 16
  val digit = 4



  @Test def testToSignedDigit() {
    val a = BigInt(1)
    val b = SignedDigit.toSignedDigit(a)
    assertTrue(b == BigInt(2))

    val c = 3
    val d = SignedDigit.toSignedDigit(c)
    assertTrue(d == BigInt(10))
  }

  @Test def testFromSignedDigit() {
    val a = BigInt(1)
    val b = SignedDigit.fromSignedDigitInt(a)
    assertTrue(b == BigInt(-1))

    val c = 3
    val d = SignedDigit.fromSignedDigitInt(c)
    assertTrue(d == BigInt(0))

    val e = 2
    val f = SignedDigit.fromSignedDigitInt(e)
    assertTrue(f == BigInt(1))
  }

  @Test def testConversion() {
    for (i <- 0 until trials) {
      val a = BigInt(r.nextInt(100000))
      val b = SignedDigit.fromSignedDigitInt(SignedDigit.toSignedDigit(a))
      assertTrue(a == b)
    }
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
        poke(c.io.a, SignedDigit.toSignedDigit(inA))
        poke(c.io.b, SignedDigit.toSignedDigit(inB))
        val res = SignedDigit.fromSignedDigitInt(peek(c.io.c))
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
        poke(c.io.a, SignedDigit.toSignedDigit(inA))
        poke(c.io.b, SignedDigit.toSignedDigit(inB))
        poke(c.io.c, SignedDigit.toSignedDigit(inC))
        val res = SignedDigit.fromSignedDigitInt(peek(c.io.d))
        expect(res == inA + inB + inC, "Expected: " + (inA + inB + inC).toString + "\tGot: " + res.toString)
      }
    }

    launchCppTester((c : SignedDigitAdder2Test) => new SignedDigitAdder2Tests(c))
  }

  @Test def testSignedListToUInt() {
    val lst = List(1, -1, 0, 1, -1, 0)
    val expected = "b100100100100"
    val res = SignedDigit.signedListToUInt(lst)
    assertTrue("Expected and Result is not equal", expected == res)

  }

}