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

class LSDFSuite extends TestSuite {

  val trials = 10
  val r = scala.util.Random

  /* Least Significant Digit Implementations */

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