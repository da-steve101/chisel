/*
 Copyright (c) 2011, 2012, 2013, 2014 The Regents of the University of
 Sydney. All Rights Reserved.  Redistribution and use in
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
import scala.util.Random
import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore

import Chisel._

/** This test suite for Pipelined Operations
  */
class PipelinedOperationsSuite extends TestSuite {
  val r = scala.util.Random
  val trials = 10

 @Test def testPipelinedOperationAdder() {
    class PipelinedAdder extends Module {
      val digit = 8
      val io = new Bundle {
        val a = UInt(INPUT, 16)
        val b = UInt(INPUT, 16)
        val c = UInt(OUTPUT, 16)
      }
      io.c := PipelinedOperations.Adder(io.a, io.b, digit)
    }

    class PipelinedAdderTests(c : PipelinedAdder) extends Tester(c) {
      // Fill the Pipeline
      val res =  new scala.collection.mutable.Queue[BigInt]
      val stages = c.io.a.getWidth()/c.digit - 1
      var start = 0
      for (j <- 0 until stages) {
        val inA = BigInt(r.nextInt(1 << c.io.a.getWidth() - 2))
        val inB = BigInt(r.nextInt(1 << c.io.b.getWidth() - 2))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        res.enqueue(inA + inB)
        step(1)
      }
      for (i <- 0 until trials) {
        expect(c.io.c, res.dequeue())
        val inA = BigInt(r.nextInt(1 << c.io.a.getWidth() - 2))
        val inB = BigInt(r.nextInt(1 << c.io.b.getWidth() - 2))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        res.enqueue(inA + inB)
        step(1)
      }
    }

    launchCppTester((c: PipelinedAdder) => new PipelinedAdderTests(c))
  }

  @Test def testPipelinedOperationSubtractor() {
    class PipelinedSubtractor extends Module {
      val digit = 8
      val io = new Bundle {
        val a = UInt(INPUT, 16)
        val b = UInt(INPUT, 16)
        val c = UInt(OUTPUT, 16)
      }
      io.c := PipelinedOperations.Subtractor(io.a, io.b, digit)
    }

    class PipelinedSubtractorTests(c : PipelinedSubtractor) extends Tester(c) {
      // Fill the Pipeline
      val res =  new scala.collection.mutable.Queue[BigInt]
      val stages = c.io.a.getWidth()/c.digit - 1
      var start = 0
      for (j <- 0 until stages) {
        var inA = BigInt(r.nextInt(1 << c.io.a.getWidth() - 2))
        var inB = BigInt(r.nextInt(1 << c.io.b.getWidth() - 2))
        while (inB < inA) {
          inA = BigInt(r.nextInt(1 << c.io.a.getWidth() - 2))
          inB = BigInt(r.nextInt(1 << c.io.b.getWidth() - 2))
        }
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        res.enqueue(inA - inB)
        step(1)
      }
      for (i <- 0 until trials) {
        expect(c.io.c, res.dequeue())
        var inA = BigInt(r.nextInt(1 << c.io.a.getWidth() - 2))
        var inB = BigInt(r.nextInt(1 << c.io.b.getWidth() - 2))
        while (inB > inA) {
          inA = BigInt(r.nextInt(1 << c.io.a.getWidth() - 2))
          inB = BigInt(r.nextInt(1 << c.io.b.getWidth() - 2))
        }
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        res.enqueue(inA - inB)
        step(1)
      }
    }

    launchCppTester((c: PipelinedSubtractor) => new PipelinedSubtractorTests(c))
  } 

}
