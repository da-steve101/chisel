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
import Chisel.PipelinedOperations._

/** This test suite for Pipelined Operations
  */
class PipelinedOperationsSuite extends TestSuite {
  val r = scala.util.Random
  val trials = 10
  val maxStages = 20
  val bitWidth = 16
  val fracWidth = 4 // fractional width for fixed tests

 @Test def testPipelinedOperationAdderUInt() {
    class PipelinedAdderUInt(val stages : Int) extends Module {
      val io = new Bundle {
        val a = UInt(INPUT, bitWidth)
        val b = UInt(INPUT, bitWidth)
        val c = UInt(OUTPUT, bitWidth)
      }
      io.c := plAdder(io.a, io.b, stages)
    }

    class PipelinedAdderUIntTests(c : PipelinedAdderUInt) extends Tester(c) {
      // Fill the Pipeline
      val res =  new scala.collection.mutable.Queue[BigInt]
      var start = 0
      for (j <- 0 until c.stages) {
        val inA = BigInt(r.nextInt(1 << c.io.a.getWidth() - 2))
        val inB = BigInt(r.nextInt(1 << c.io.b.getWidth() - 2))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        res.enqueue(inA + inB)
        step(1)
      }
      if ( c.stages == 0 ) {
        val inA = BigInt(r.nextInt(1 << c.io.a.getWidth() - 2))
        val inB = BigInt(r.nextInt(1 << c.io.b.getWidth() - 2))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        res.enqueue(inA + inB)
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

   for ( stages <- 0 until maxStages ) {
     println("stages = " + stages )
     chiselMainTest(Array[String]("--backend", "c",
       "--targetDir", dir.getPath.toString(), "--genHarness", "--compile", "--test"),
       () => Module(new PipelinedAdderUInt(stages))) {m => new PipelinedAdderUIntTests(m)}
   }
  }

 @Test def testPipelinedOperationAdderSInt() {
    class PipelinedAdderSInt(val stages : Int) extends Module {
        val io = new Bundle {
        val a = SInt(INPUT, bitWidth)
        val b = SInt(INPUT, bitWidth)
        val c = SInt(OUTPUT, bitWidth)
      }
      io.c := plAdder(io.a, io.b, stages)
    }

    class PipelinedAdderSIntTests(c : PipelinedAdderSInt) extends Tester(c) {
      // Fill the Pipeline
      val res =  new scala.collection.mutable.Queue[BigInt]
      var start = 0
      for (j <- 0 until c.stages) {
        val inA = BigInt(r.nextInt(1 << c.io.a.getWidth() - 2))
        val inB = BigInt(r.nextInt(1 << c.io.b.getWidth() - 2))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        res.enqueue(inA + inB)
        step(1)
      }
      if ( c.stages == 0 ) {
        val inA = BigInt(r.nextInt(1 << c.io.a.getWidth() - 2))
        val inB = BigInt(r.nextInt(1 << c.io.b.getWidth() - 2))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        res.enqueue(inA + inB)
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

   for ( stages <- 0 until maxStages ) {
     println("stages = " + stages )
     chiselMainTest(Array[String]("--backend", "c",
       "--targetDir", dir.getPath.toString(), "--genHarness", "--compile", "--test"),
       () => Module(new PipelinedAdderSInt(stages))) {m => new PipelinedAdderSIntTests(m)}
   }
 }

  def toFixedT(x : Double, fracWidth : Int) : BigInt = BigInt((x*scala.math.pow(2, fracWidth)).toInt)
  def toFixed(x : Double, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
  def toDouble(x : BigInt, fracWidth : Int) : Double = x.toDouble/scala.math.pow(2, fracWidth)

 @Test def testPipelinedOperationAdderFixed() {
    class PipelinedAdderFixed( val stages : Int ) extends Module {
      val io = new Bundle {
        val a = Fixed(INPUT, bitWidth, fracWidth)
        val b = Fixed(INPUT, bitWidth, fracWidth)
        val c = Fixed(OUTPUT, bitWidth, fracWidth)
      }
      io.c := plAdder(io.a, io.b, stages)
    }

    class PipelinedAdderFixedTests(c : PipelinedAdderFixed) extends Tester(c) {
      // Fill the Pipeline
      val res =  new scala.collection.mutable.Queue[BigInt]
      var start = 0
      for (j <- 0 until c.stages) {
        val inA = BigInt(r.nextInt(1 << c.io.a.getWidth() - 2))
        val inB = BigInt(r.nextInt(1 << c.io.b.getWidth() - 2))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        res.enqueue(toFixed(toDouble(inA, fracWidth) + toDouble(inB, fracWidth), fracWidth))
        step(1)
      }
      if ( c.stages == 0 ) {
        val inA = BigInt(r.nextInt(1 << c.io.a.getWidth() - 2))
        val inB = BigInt(r.nextInt(1 << c.io.b.getWidth() - 2))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        res.enqueue(inA + inB)
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

   for ( stages <- 0 until maxStages ) {
     println("stages = " + stages )
     chiselMainTest(Array[String]("--backend", "c",
       "--targetDir", dir.getPath.toString(), "--genHarness", "--compile", "--test"),
       () => Module(new PipelinedAdderFixed(stages))) {m => new PipelinedAdderFixedTests(m)}
   }
  }

  @Test def testPipelinedOperationSubtractorUInt() {
    class PipelinedSubtractorUInt( val stages : Int ) extends Module {
      val io = new Bundle {
        val a = UInt(INPUT, bitWidth)
        val b = UInt(INPUT, bitWidth)
        val c = UInt(OUTPUT, bitWidth)
      }
      io.c := plSubtractor(io.a, io.b, stages)
    }

    class PipelinedSubtractorUIntTests(c : PipelinedSubtractorUInt) extends Tester(c) {
      // Fill the Pipeline
      val res =  new scala.collection.mutable.Queue[BigInt]
      var start = 0
      for (j <- 0 until c.stages) {
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
      if ( c.stages == 0 ) {
        var inA = BigInt(r.nextInt(1 << c.io.a.getWidth() - 2))
        var inB = BigInt(r.nextInt(1 << c.io.b.getWidth() - 2))
        while (inB < inA) {
          inA = BigInt(r.nextInt(1 << c.io.a.getWidth() - 2))
          inB = BigInt(r.nextInt(1 << c.io.b.getWidth() - 2))
        }
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        res.enqueue(inA - inB)
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

   for ( stages <- 0 until maxStages ) {
     println("stages = " + stages )
     chiselMainTest(Array[String]("--backend", "c",
       "--targetDir", dir.getPath.toString(), "--genHarness", "--compile", "--test"),
       () => Module(new PipelinedSubtractorUInt(stages))) {m => new PipelinedSubtractorUIntTests(m)}
   }
  } 

  @Test def testPipelinedOperationSubtractorSInt() {
    class PipelinedSubtractorSInt( val stages : Int ) extends Module {
      val io = new Bundle {
        val a = SInt(INPUT, bitWidth)
        val b = SInt(INPUT, bitWidth)
        val c = SInt(OUTPUT, bitWidth)
      }
      io.c := plSubtractor(io.a, io.b, stages)
    }

    class PipelinedSubtractorSIntTests(c : PipelinedSubtractorSInt) extends Tester(c) {
      // Fill the Pipeline
      val res =  new scala.collection.mutable.Queue[BigInt]
      var start = 0
      for (j <- 0 until c.stages) {
        var inA = BigInt(r.nextInt(1 << c.io.a.getWidth() - 2))
        var inB = BigInt(r.nextInt(1 << c.io.b.getWidth() - 2))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        res.enqueue(inA - inB)
        step(1)
      }
      if ( c.stages == 0 ) {
        val inA = BigInt(r.nextInt(1 << c.io.a.getWidth() - 2))
        val inB = BigInt(r.nextInt(1 << c.io.b.getWidth() - 2))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        res.enqueue(inA - inB)
      }
      for (i <- 0 until trials) {
        expect(c.io.c, res.dequeue())
        var inA = BigInt(r.nextInt(1 << c.io.a.getWidth() - 2))
        var inB = BigInt(r.nextInt(1 << c.io.b.getWidth() - 2))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        res.enqueue(inA - inB)
        step(1)
      }
    }

   for ( stages <- 0 until maxStages ) {
     println("stages = " + stages )
     chiselMainTest(Array[String]("--backend", "c",
       "--targetDir", dir.getPath.toString(), "--genHarness", "--compile", "--test"),
       () => Module(new PipelinedSubtractorSInt(stages))) {m => new PipelinedSubtractorSIntTests(m)}
   }
  } 

  @Test def testPipelinedOperationSubtractorFixed() {
    class PipelinedSubtractorFixed( val stages : Int ) extends Module {
      val io = new Bundle {
        val a = Fixed(INPUT, bitWidth, fracWidth)
        val b = Fixed(INPUT, bitWidth, fracWidth)
        val c = Fixed(OUTPUT, bitWidth, fracWidth)
      }
      io.c := plSubtractor(io.a, io.b, stages)
    }

    class PipelinedSubtractorFixedTests(c : PipelinedSubtractorFixed) extends Tester(c) {
      // Fill the Pipeline
      val res =  new scala.collection.mutable.Queue[BigInt]
      var start = 0
      for (j <- 0 until c.stages) {
        var inA = BigInt(r.nextInt(1 << c.io.a.getWidth() - 2))
        var inB = BigInt(r.nextInt(1 << c.io.b.getWidth() - 2))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        res.enqueue(toFixed(toDouble(inA, fracWidth) - toDouble(inB, fracWidth), fracWidth))
        step(1)
      }
      if ( c.stages == 0 ) {
        val inA = BigInt(r.nextInt(1 << c.io.a.getWidth() - 2))
        val inB = BigInt(r.nextInt(1 << c.io.b.getWidth() - 2))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        res.enqueue(inA - inB)
      }
      for (i <- 0 until trials) {
        expect(c.io.c, res.dequeue())
        var inA = BigInt(r.nextInt(1 << c.io.a.getWidth() - 2))
        var inB = BigInt(r.nextInt(1 << c.io.b.getWidth() - 2))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        res.enqueue(inA - inB)
        step(1)
      }
    }

    for ( stages <- 0 until maxStages ) {
      println("stages = " + stages )
      chiselMainTest(Array[String]("--backend", "c",
        "--targetDir", dir.getPath.toString(), "--genHarness", "--compile", "--test"),
        () => Module(new PipelinedSubtractorFixed(stages))) {m => new PipelinedSubtractorFixedTests(m)}
    }
  }


 @Test def testPipelinedOperationMultiplierUInt() {
    class PipelinedMultiplierUInt(val stages : Int) extends Module {
      val io = new Bundle {
        val a = UInt(INPUT, bitWidth)
        val b = UInt(INPUT, bitWidth)
        val c = UInt(OUTPUT, bitWidth*2)
      }
      io.c := plMultiplier(io.a, io.b, stages)
    }

    class PipelinedMultiplierUIntTests(c : PipelinedMultiplierUInt) extends Tester(c) {
      // Fill the Pipeline
      val res =  new scala.collection.mutable.Queue[BigInt]
      var start = 0
      for (j <- 0 until c.stages) {
        val inA = BigInt(r.nextInt(1 << c.io.a.getWidth()/2 - 2))
        val inB = BigInt(r.nextInt(1 << c.io.b.getWidth()/2 - 2))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        res.enqueue(inA * inB)
        step(1)
      }
      if ( c.stages == 0 ) {
        val inA = BigInt(r.nextInt(1 << c.io.a.getWidth()/2 - 2))
        val inB = BigInt(r.nextInt(1 << c.io.b.getWidth()/2 - 2))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        res.enqueue(inA * inB)
      }
      for (i <- 0 until trials) {
        expect(c.io.c, res.dequeue())
        val inA = BigInt(r.nextInt(1 << c.io.a.getWidth()/2 - 2))
        val inB = BigInt(r.nextInt(1 << c.io.b.getWidth()/2 - 2))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        res.enqueue(inA * inB)
        step(1)
      }
    }

   for ( stages <- 0 until maxStages ) {
     println("stages = " + stages )
     chiselMainTest(Array[String]("--backend", "c",
       "--targetDir", dir.getPath.toString(), "--genHarness", "--compile", "--test"),
       () => Module(new PipelinedMultiplierUInt(stages))) {m => new PipelinedMultiplierUIntTests(m)}
   }
  }

 @Test def testPipelinedOperationMultiplierSInt() {
    class PipelinedMultiplierSInt(val stages : Int) extends Module {
      val io = new Bundle {
        val a = SInt(INPUT, bitWidth)
        val b = SInt(INPUT, bitWidth)
        val c = SInt(OUTPUT, bitWidth*2)
      }
      io.c := plMultiplier(io.a, io.b, stages)
    }

    class PipelinedMultiplierSIntTests(c : PipelinedMultiplierSInt) extends Tester(c) {
      // Fill the Pipeline
      val res =  new scala.collection.mutable.Queue[BigInt]
      var start = 0
      for (j <- 0 until c.stages) {
        val inA = BigInt(r.nextInt(1 << c.io.a.getWidth()/2 - 2))
        val inB = BigInt(r.nextInt(1 << c.io.b.getWidth()/2 - 2))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        res.enqueue(inA * inB)
        step(1)
      }
      if ( c.stages == 0 ) {
        val inA = BigInt(r.nextInt(1 << c.io.a.getWidth()/2 - 2))
        val inB = BigInt(r.nextInt(1 << c.io.b.getWidth()/2 - 2))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        res.enqueue(inA * inB)
      }
      for (i <- 0 until trials) {
        expect(c.io.c, res.dequeue())
        val inA = BigInt(r.nextInt(1 << c.io.a.getWidth()/2 - 2))
        val inB = BigInt(r.nextInt(1 << c.io.b.getWidth()/2 - 2))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        res.enqueue(inA * inB)
        step(1)
      }
    }

   for ( stages <- 0 until maxStages ) {
     println("stages = " + stages )
     chiselMainTest(Array[String]("--backend", "c",
       "--targetDir", dir.getPath.toString(), "--genHarness", "--compile", "--test"),
       () => Module(new PipelinedMultiplierSInt(stages))) {m => new PipelinedMultiplierSIntTests(m)}
   }
  }

 @Test def testPipelinedOperationMultiplierFixed() {
    class PipelinedMultiplierFixed( val stages : Int ) extends Module {
      val io = new Bundle {
        val a = Fixed(INPUT, bitWidth, fracWidth)
        val b = Fixed(INPUT, bitWidth, fracWidth)
        val c = Fixed(OUTPUT, bitWidth*2, fracWidth*2)
      }
      io.c := plMultiplier(io.a, io.b, stages)
    }

    class PipelinedMultiplierFixedTests(c : PipelinedMultiplierFixed) extends Tester(c) {
      // Fill the Pipeline
      val res =  new scala.collection.mutable.Queue[BigInt]
      var start = 0
      for (j <- 0 until c.stages) {
        val inA = BigInt(r.nextInt(1 << c.io.a.getWidth()/2 - 2))
        val inB = BigInt(r.nextInt(1 << c.io.b.getWidth()/2 - 2))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        res.enqueue(toFixed(toDouble(inA, fracWidth) * toDouble(inB, fracWidth), fracWidth*2))
        step(1)
      }
      if ( c.stages == 0 ) {
        val inA = BigInt(r.nextInt(1 << c.io.a.getWidth()/2 - 2))
        val inB = BigInt(r.nextInt(1 << c.io.b.getWidth()/2 - 2))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        res.enqueue(toFixed(toDouble(inA, fracWidth) * toDouble(inB, fracWidth), fracWidth*2))
      }
      for (i <- 0 until trials) {
        expect(c.io.c, res.dequeue())
        val inA = BigInt(r.nextInt(1 << c.io.a.getWidth()/2 - 2))
        val inB = BigInt(r.nextInt(1 << c.io.b.getWidth()/2 - 2))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        res.enqueue(toFixed(toDouble(inA, fracWidth) * toDouble(inB, fracWidth), fracWidth*2))
        step(1)
      }
    }

   for ( stages <- 0 until maxStages ) {
     println("stages = " + stages )
     chiselMainTest(Array[String]("--backend", "c",
       "--targetDir", dir.getPath.toString(), "--genHarness", "--compile", "--test"),
       () => Module(new PipelinedMultiplierFixed(stages))) {m => new PipelinedMultiplierFixedTests(m)}
   }
  }
}
