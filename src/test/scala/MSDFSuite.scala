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

class MSDFSuite extends TestSuite {

  val trials = 1
  val r = scala.util.Random
  val totalWidth = 16

  val digit = 4
  val n = totalWidth/digit
  
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

  @Test def testMSDFDivSEL() {
    class MSDFDivSELTest extends Module {
      val io = new Bundle {
        val a = UInt(INPUT, 5)
        val c = UInt(OUTPUT, 2)
      }
      io.c := MSDFDiv.SEL(io.a)
    }

    class MSDFDivSELTests(c : MSDFDivSELTest) extends Tester(c) {
      val sel = List(
        UInt("b00000").litValue(), 
        UInt("b00001").litValue(), 
        UInt("b00010").litValue(), 
        UInt("b00011").litValue(), 
        UInt("b00100").litValue(),
        UInt("b00101").litValue(),
        UInt("b00110").litValue(),
        UInt("b00111").litValue(),
        UInt("b01000").litValue(), 
        UInt("b01001").litValue(), 
        UInt("b01010").litValue(), 
        UInt("b01011").litValue(), 
        UInt("b01100").litValue(),
        UInt("b01101").litValue(),
        UInt("b01110").litValue(),
        UInt("b01111").litValue(),
        UInt("b10000").litValue(), 
        UInt("b10001").litValue(), 
        UInt("b10010").litValue(), 
        UInt("b10011").litValue(), 
        UInt("b10100").litValue(),
        UInt("b10101").litValue(),
        UInt("b10110").litValue(),
        UInt("b10111").litValue(),
        UInt("b11000").litValue(), 
        UInt("b11001").litValue(), 
        UInt("b11010").litValue(), 
        UInt("b11011").litValue(), 
        UInt("b11100").litValue(),
        UInt("b11101").litValue(),
        UInt("b11110").litValue(),
        UInt("b11111").litValue()
        )
      val selAns = List(
        UInt("b00").litValue(), 
        UInt("b00").litValue(), 
        UInt("b10").litValue(), 
        UInt("b10").litValue(), 
        UInt("b10").litValue(), 
        UInt("b10").litValue(), 
        UInt("b10").litValue(), 
        UInt("b10").litValue(), 
        UInt("b10").litValue(), 
        UInt("b10").litValue(), 
        UInt("b10").litValue(), 
        UInt("b10").litValue(), 
        UInt("b10").litValue(), 
        UInt("b10").litValue(), 
        UInt("b10").litValue(), 
        UInt("b10").litValue(), 
        UInt("b01").litValue(),
        UInt("b01").litValue(),
        UInt("b01").litValue(),
        UInt("b01").litValue(),
        UInt("b01").litValue(),
        UInt("b01").litValue(),
        UInt("b01").litValue(),
        UInt("b01").litValue(),
        UInt("b01").litValue(),
        UInt("b01").litValue(),
        UInt("b01").litValue(),
        UInt("b01").litValue(),
        UInt("b01").litValue(),
        UInt("b01").litValue(),
        UInt("b00").litValue(),
        UInt("b00").litValue()
        )

      for (i <- 0 until sel.length) {
        poke(c.io.a, sel(i))
        expect(c.io.c, selAns(i))
      }
    }

    launchCppTester((c : MSDFDivSELTest) => new MSDFDivSELTests(c))
  }

  @Test def testMSDFDivU() {
    class MSDFDivUTest extends Module {
      val io = new Bundle {
        val x = UInt(INPUT, 2)
        val d = UInt(INPUT, 2)
        val c = UInt(OUTPUT, 6)
      }
      io.c := MSDFDiv.U(io.x, io.d, UInt("b0"))
    }

    class MSDFDivUTests(c : MSDFDivUTest) extends Tester(c) {
      val u = List(
        List(UInt("b10").litValue(), UInt("b10").litValue()),
        List(UInt("b10").litValue(), UInt("b00").litValue()),
        List(UInt("b10").litValue(), UInt("b01").litValue()),
        List(UInt("b10").litValue(), UInt("b11").litValue()),
        List(UInt("b00").litValue(), UInt("b10").litValue()),
        List(UInt("b00").litValue(), UInt("b00").litValue()),
        List(UInt("b00").litValue(), UInt("b01").litValue()),
        List(UInt("b00").litValue(), UInt("b11").litValue()),
        List(UInt("b01").litValue(), UInt("b10").litValue()),
        List(UInt("b01").litValue(), UInt("b00").litValue()),
        List(UInt("b01").litValue(), UInt("b01").litValue()),
        List(UInt("b01").litValue(), UInt("b11").litValue()),
        List(UInt("b11").litValue(), UInt("b10").litValue()),
        List(UInt("b11").litValue(), UInt("b00").litValue()),
        List(UInt("b11").litValue(), UInt("b01").litValue()),
        List(UInt("b11").litValue(), UInt("b11").litValue())
        )
      val uAns = List(
        UInt("b000000").litValue(), 
        UInt("b000001").litValue(), 
        UInt("b000001").litValue(), 
        UInt("b000001").litValue(), 
        UInt("b111111").litValue(), 
        UInt("b000000").litValue(), 
        UInt("b000000").litValue(), 
        UInt("b000000").litValue(), 
        UInt("b111110").litValue(), 
        UInt("b111111").litValue(), 
        UInt("b111111").litValue(), 
        UInt("b111111").litValue(), 
        UInt("b111111").litValue(), 
        UInt("b000000").litValue(), 
        UInt("b000000").litValue(), 
        UInt("b000000").litValue() 
        )

      for (i <- 0 until u.length) {
        poke(c.io.x, u(i)(0))
        poke(c.io.d, u(i)(1))
        expect(c.io.c, uAns(i))
      }
    }

    launchCppTester((c : MSDFDivUTest) => new MSDFDivUTests(c))
  }

  @Test def testMSDFDiv() {
    class MSDFDivTest extends Module {
      val io = new Bundle {
        val x = UInt(INPUT, 2)
        val d = UInt(INPUT, 2)
        val start = Bool(INPUT)
        val c = UInt(OUTPUT, 14)
      }
      io.c := MSDFDiv(io.x, io.d, io.start)
    }

    class MSDFDivTests(c : MSDFDivTest) extends Tester(c) {

      for (i <- 0 until trials) {
        var dX = r.nextDouble()/2
        var dD = r.nextDouble()/2
        while (dX/dD > 0.5) {
          dX = r.nextDouble()/2
          dD = r.nextDouble()/2
        }
        val x = SignedDigit.doubleToSigned(dX, 8)
        val d = SignedDigit.doubleToSigned(dD, 8)
        val res = new ArrayBuffer[Int]
        for (j <- 0 until x.length + 4) {
          val inX = if(j < x.length) x(j) else 0
          val inD = if(j < d.length) d(j) else 0
          poke(c.io.x, SignedDigit.toSignedDigit(inX))
          poke(c.io.d, SignedDigit.toSignedDigit(inD))
          val start = if (j == 0) BigInt(1) else BigInt(0)
          poke(c.io.start, start)
          peek(c.io.c)
          if (j >= 4)
            res ++= SignedDigit.fromSignedDigit(peek(c.io.c).toInt)
          step(1)
        }
        val expectedRes = SignedDigit.signedToDouble(x)/SignedDigit.signedToDouble(d)
        val dRes = SignedDigit.signedToDouble(res.toList)
        val err = scala.math.abs(expectedRes - dRes)
        val correct = if (err > scala.math.pow(2, -8)) false else true
        expect(correct, "Expected: " + expectedRes.toString + "(" + (dX*dD).toString + ")" + "\tGot: " + dRes.toString + "\tError: " + err.toString)
      }
    }
    launchCppTester((c : MSDFDivTest) => new MSDFDivTests(c))
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
        poke(c.io.a, SignedDigit.toSignedDigit(q(i)))
        expect(c.io.c, qAns(i))
        step(1)
      }
    }

    launchCppTester((c : SDOnlineConversionTest) => new SDOnlineConversionTests(c))
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
        val a = SignedDigit.doubleToSigned(dA, 8)
        val b = SignedDigit.doubleToSigned(dB, 8)
        val res = new ArrayBuffer[Int]
        // Delay = 5 Mul + Add
        for (j <- 0 until a.length + 5) {
          val inA = if(j < a.length) a(j) else 0
          val inB = if(j < b.length) b(j) else 0
          poke(c.io.a, SignedDigit.toSignedDigit(inA))
          poke(c.io.b, SignedDigit.toSignedDigit(inB))
          val start = if (j == 0) BigInt(1) else BigInt(0)
          poke(c.io.start, start)
          if (j >= 5)
            res ++= SignedDigit.fromSignedDigit(peek(c.io.c).toInt)
          step(1)
        }
        
        val aAddB = SignedDigit.signedToDouble(a)+SignedDigit.signedToDouble(b)
        val expectedRes = aAddB*aAddB
        val dRes = SignedDigit.signedToDouble(res.toList)
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
        val a = SignedDigit.doubleToSigned(dA, 8)
        val b = SignedDigit.doubleToSigned(dB, 8)
        val res = new ArrayBuffer[Int]
        // Delay = 5 Mul + Add
        for (j <- 0 until a.length + 5) {
          val inA = if(j < a.length) a(j) else 0
          val inB = if(j < b.length) b(j) else 0
          poke(c.io.a, SignedDigit.toSignedDigit(inA))
          poke(c.io.b, SignedDigit.toSignedDigit(inB))
          val start = if (j == 0) BigInt(1) else BigInt(0)
          poke(c.io.start, start)
          if (j >= 5)
            res ++= SignedDigit.fromSignedDigit(peek(c.io.c).toInt)
          step(1)
        }
        
        val aMulB = SignedDigit.signedToDouble(a)*SignedDigit.signedToDouble(b)
        val expectedRes = aMulB+aMulB
        val dRes = SignedDigit.signedToDouble(res.toList)
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
      val digitNumber = 1
      for (i <- 0 until trials) {
        val dA = r.nextDouble()/2
        val dB = r.nextDouble()/2
        val a = SignedDigit.doubleToSigned(dA, 8)
        val b = SignedDigit.doubleToSigned(dB, 8)
        val res = new ArrayBuffer[Int]
        for (j <- 0 until 8 + 2*digitNumber by digitNumber) {
          val inA = if(j < a.length) a.slice(j, j+digitNumber) else List.fill(digitNumber){0}
          val inB = if(j < b.length) b.slice(j, j+digitNumber) else List.fill(digitNumber){0}
          poke(c.io.a, SignedDigit.toSignedDigit(inA))
          poke(c.io.b, SignedDigit.toSignedDigit(inB))
          val start = if (j == 0) BigInt(1) else BigInt(0)
          peek(c.io.c)
          poke(c.io.start, start)
          if (j >= 2*digitNumber)
            res ++= SignedDigit.fromSignedDigit(peek(c.io.c).toInt, digitNumber)
          step(1)
        }
        val expectedRes = SignedDigit.signedToDouble(a)+SignedDigit.signedToDouble(b)
        val dRes = SignedDigit.signedToDouble(res.toList)
        val err = scala.math.abs(expectedRes - dRes)
        val correct = if (err > scala.math.pow(2, -8)) false else true
        expect(correct, "Expected: " + expectedRes.toString + "(" + (dA+dB).toString + ")" + "\tGot: " + dRes.toString + "\tError: " + err.toString)
      }
    }

    launchCppTester((c : MSDFAddTest) => new MSDFAddTests(c))
  }

  @Test def testMSDFSub() {
    class MSDFSubTest extends Module {
      val io = new Bundle {
        val a = UInt(INPUT, 2)
        val b = UInt(INPUT, 2)
        val start = Bool(INPUT)
        val c = UInt(OUTPUT, 2)
      }
      io.c := MSDFSub(io.a, io.b, io.start)
    }

    class MSDFSubTests(c : MSDFSubTest) extends Tester(c) {
      val digitNumber = 1
      for (i <- 0 until trials) {
        val dA = r.nextDouble()/2
        val dB = r.nextDouble()/2
        val a = SignedDigit.doubleToSigned(dA, 8)
        val b = SignedDigit.doubleToSigned(dB, 8)
        val res = new ArrayBuffer[Int]
        for (j <- 0 until 8 + 2*digitNumber by digitNumber) {
          val inA = if(j < a.length) a.slice(j, j+digitNumber) else List.fill(digitNumber){0}
          val inB = if(j < b.length) b.slice(j, j+digitNumber) else List.fill(digitNumber){0}
          poke(c.io.a, SignedDigit.toSignedDigit(inA))
          poke(c.io.b, SignedDigit.toSignedDigit(inB))
          val start = if (j == 0) BigInt(1) else BigInt(0)
          peek(c.io.c)
          poke(c.io.start, start)
          if (j >= 2*digitNumber)
            res ++= SignedDigit.fromSignedDigit(peek(c.io.c).toInt, digitNumber)
          step(1)
        }
        val expectedRes = SignedDigit.signedToDouble(a)-SignedDigit.signedToDouble(b)
        val dRes = SignedDigit.signedToDouble(res.toList)
        val err = scala.math.abs(expectedRes - dRes)
        val correct = if (err > scala.math.pow(2, -8)) false else true
        expect(correct, "Expected: " + expectedRes.toString + "(" + (dA+dB).toString + ")" + "\tGot: " + dRes.toString + "\tError: " + err.toString)
      }
    }

    launchCppTester((c : MSDFSubTest) => new MSDFSubTests(c))
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
        val a = SignedDigit.doubleToSigned(dA, 8)
        val b = SignedDigit.doubleToSigned(dB, 8)
        val res = new ArrayBuffer[Int]
        for (j <- 0 until a.length + 3) {
          val inA = if(j < a.length) a(j) else 0
          val inB = if(j < b.length) b(j) else 0
          poke(c.io.a, SignedDigit.toSignedDigit(inA))
          poke(c.io.b, SignedDigit.toSignedDigit(inB))
          val start = if (j == 0) BigInt(1) else BigInt(0)
          poke(c.io.start, start)
          peek(c.io.c)
          if (j >= 3)
            res ++= SignedDigit.fromSignedDigit(peek(c.io.c).toInt)
          step(1)
        }
        val expectedRes = SignedDigit.signedToDouble(a)*SignedDigit.signedToDouble(b)
        val dRes = SignedDigit.signedToDouble(res.toList)
        val err = scala.math.abs(expectedRes - dRes)
        val correct = if (err > scala.math.pow(2, -8)) false else true
        expect(correct, "Expected: " + expectedRes.toString + "(" + (dA*dB).toString + ")" + "\tGot: " + dRes.toString + "\tError: " + err.toString)
      }
    }
    launchCppTester((c : MSDFMulTest) => new MSDFMulTests(c))
  }

  @Test def testMSDFDotProduct() {
    class MSDFDotProductTest extends Module {
      val io = new Bundle {
        val a = Vec.fill(2){UInt(INPUT, 2)}
        val b = Vec.fill(2){UInt(INPUT, 2)}
        val start = Bool(INPUT)
        val c = UInt(OUTPUT, 2)
      }

      val threeDelay = ShiftRegister(io.start, 3)
      val dotProduct = (io.a, io.b).zipped.map((ai, bi) => MSDFMul(ai, bi, io.start)).reduce((r, c) => MSDFAdd(r, c, threeDelay))

      io.c := dotProduct
    }

    class MSDFDotProductTests(c : MSDFDotProductTest) extends Tester(c) {

      for (i <- 0 until trials) {
        val dA = List.fill(2){r.nextDouble()/2}
        val dB = List.fill(2){r.nextDouble()/2}
        val a = dA.map(in => SignedDigit.doubleToSigned(in, 8))
        val b = dB.map(in => SignedDigit.doubleToSigned(in, 8))
        val res = new ArrayBuffer[Int]
        for (j <- 0 until a(0).length + 5) {
          for (k <- 0 until 2) {
            val inA = if(j < a(k).length) a(k)(j) else 0
            val inB = if(j < b(k).length) b(k)(j) else 0
            poke(c.io.a(k), SignedDigit.toSignedDigit(inA))
            poke(c.io.b(k), SignedDigit.toSignedDigit(inB))
          }
          val start = if (j == 0) BigInt(1) else BigInt(0)
          poke(c.io.start, start)
          peek(c.io.c)
          if (j >= 5)
            res ++= SignedDigit.fromSignedDigit(peek(c.io.c).toInt)
          step(1)
        }
        val expectedRes = (dA, dB).zipped.map(_*_).reduce(_+_)
        val dRes = SignedDigit.signedToDouble(res.toList)
        val err = scala.math.abs(expectedRes - dRes)
        val correct = if (err > scala.math.pow(2, -7)) false else true
        expect(correct, "Expected: " + expectedRes.toString + "\tGot: " + dRes.toString + "\tError: " + err.toString)
      }
    }
    launchCppTester((c : MSDFDotProductTest) => new MSDFDotProductTests(c))
  }

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
        val a = SignedDigit.doubleToSigned(dA, 8)
        val b = SignedDigit.doubleToSigned(dB, 8)
        val res = new ArrayBuffer[Int]
        for (j <- 0 until 8 + 2) {
          val inA = if(j < a.length) a(j) else 0
          poke(c.io.a, SignedDigit.toSignedDigit(inA))
          val start = if (j == 0) BigInt(1) else BigInt(0)
          poke(c.io.start, start)
          if (j >= 2)
            res ++= SignedDigit.fromSignedDigit(peek(c.io.c).toInt)
          step(1)
        }
        val expectedRes = SignedDigit.signedToDouble(a)+SignedDigit.signedToDouble(b)
        val dRes = SignedDigit.signedToDouble(res.toList)
        dB = dRes
        val err = scala.math.abs(expectedRes - dRes)
        val correct = if (err > scala.math.pow(2, -8)) false else true
        expect(correct, "Expected: " + expectedRes.toString + "(" + (dA+dB).toString + ")" + "\tGot: " + dRes.toString + "\tError: " + err.toString)
      }
    }
    launchCppTester((c : MSDFAddAccumTest) => new MSDFAddAccumTests(c))
  }

  @Test def testMSDFLMSForward() {
    class MSDFLMSTest extends Module {
      val io = new Bundle {
        val x = Vec.fill(2){UInt(INPUT, 2)}
        val inW = Vec.fill(2){UInt(INPUT, 2)}
        val y = UInt(INPUT, 2)
        val stepSize = UInt(INPUT, 2)
        val start = Bool(INPUT)
        val ybar = UInt(OUTPUT, 2)
        val outErr = UInt(OUTPUT, 2)
        val outStep = UInt(OUTPUT, 2)
        val outW = Vec.fill(2){UInt(OUTPUT, 2)}
      }
      val threeDelay = ShiftRegister(io.start, 3)
      val dotProduct = (io.x, io.inW).zipped.map((ai, bi) => MSDFMul(ai, bi, io.start)).reduce((r, c) => MSDFAdd(r, c, threeDelay)) // 5 Delay

      io.ybar := dotProduct

      val yDelay = UInt(width=2)
      yDelay := ShiftRegister(io.y, 5)
      val fiveDelay = ShiftRegister(io.start, 5)
      val err = MSDFSub(yDelay, dotProduct, fiveDelay) // 2 Delay (7 Total)
      io.outErr := err

      val stepDelay = UInt(width=2)
      stepDelay := ShiftRegister(io.stepSize, 7)
      val sevenDelay = ShiftRegister(io.start, 7)
      val step = MSDFMul(stepDelay, err, sevenDelay) // 3 Delay (10 Total)
      io.outStep := step

      val tenDelay = ShiftRegister(io.start, 10)
      val xDelay = Vec.fill(2){UInt(width=2)}
      xDelay := ShiftRegister(io.x, 10)
      val wDelay = Vec.fill(2){UInt(width=2)}
      wDelay := ShiftRegister(io.inW, 13)
      val thirteenDelay = ShiftRegister(io.start, 13)
      val wUpdate = (wDelay, xDelay.map(x1 => MSDFMul(x1, step, tenDelay))).zipped.map((w1, x1) => MSDFAdd(w1, x1, thirteenDelay)) // 5 Delay (15 Total)

      io.outW := wUpdate

    }

    class MSDFLMSTests(c : MSDFLMSTest) extends Tester(c) {

      def compare(expectedRes : Double, dRes : Double) {
        val err = scala.math.abs(expectedRes - dRes)
        val correct = if (err > scala.math.pow(2, -8)) false else true
        expect(correct, "Expected: " + expectedRes.toString + "\tGot: " + dRes.toString + "\tError: " + err.toString)
      }

      def update(x : List[Double], y : Double, w : List[Double], stepSize : Double) = {
        val yBar = (x, w).zipped.map(_*_).reduce(_+_)
        val err = y - yBar
        val step = stepSize*err
        val wUpdate = (x.map(step*_), w).zipped.map(_+_)
        (yBar, wUpdate, err, step)
      }

      val digitSize = 16
      val ybarDelay = 5
      val errDelay = 7
      val stepDelay = 10
      val wUpdateDelay = 15

      val stepSize : Double = 0.125

      for (i <- 0 until trials) {

        val dX : List[Double] = List.fill(2){r.nextDouble()/8}
        val dW : List[Double] = List.fill(2){r.nextDouble()/8}
        val dY : Double = r.nextDouble()/8
        val x = dX.map(in => SignedDigit.doubleToSigned(in, digitSize))
        val w = dW.map(in => SignedDigit.doubleToSigned(in, digitSize))
        val y = SignedDigit.doubleToSigned(dY, digitSize)
        val stepS = SignedDigit.doubleToSigned(stepSize, digitSize)

        val wRes = List.fill(2){new ArrayBuffer[Int]}
        val yRes = new ArrayBuffer[Int]
        val errRes = new ArrayBuffer[Int]
        val stepRes = new ArrayBuffer[Int]

        for (j <- 0 until digitSize + wUpdateDelay) {
          // X & W Input
          for (k <- 0 until 2) {
           val inX = if(j < digitSize) x(k)(j) else 0
           val inW = if(j < digitSize) w(k)(j) else 0
           poke(c.io.x(k), SignedDigit.toSignedDigit(inX)) 
           poke(c.io.inW(k), SignedDigit.toSignedDigit(inW)) 
          }

          // Y Input
          val inY = if(j < digitSize) y(j) else 0
          poke(c.io.y, SignedDigit.toSignedDigit(inY))

          // Step Input
          val inStep = if(j < digitSize) stepS(j) else 0
          poke(c.io.stepSize, SignedDigit.toSignedDigit(inStep))

          // Start Input
          val start = if (j == 0) 1 else 0
          poke(c.io.start, start)


          if ((j >= ybarDelay) & (j < ybarDelay+digitSize))
            yRes ++= SignedDigit.fromSignedDigit(peek(c.io.ybar).toInt)

          if ((j >= errDelay) & (j < errDelay+digitSize))
            errRes ++= SignedDigit.fromSignedDigit(peek(c.io.outErr).toInt)

          if ((j >= stepDelay) & (j < stepDelay+digitSize))
            stepRes ++= SignedDigit.fromSignedDigit(peek(c.io.outStep).toInt)

          if ((j >= wUpdateDelay) & (j < wUpdateDelay+digitSize)) {
            for (k <- 0 until 2) {
              wRes(k) ++= SignedDigit.fromSignedDigit(peek(c.io.outW(k)).toInt)
            }
          }

          step(1)
        }

        // Expected Results
        val (yBar, wUpdate, lmsErr, lmsStep) = update(dX, dY, dW, stepSize)

        // yBar
        println("yBar:")
        compare(yBar, SignedDigit.signedToDouble(yRes.toList))

        // lmsErr
        println("lmsErr:")
        compare(lmsErr, SignedDigit.signedToDouble(errRes.toList))

        // lmsStep
        println("lmsStep:")
        compare(lmsStep, SignedDigit.signedToDouble(stepRes.toList))

        // w
        for (j <- 0 until 2) {
          println("w(" + j.toString + "): ")
          compare(wUpdate(j), SignedDigit.signedToDouble(wRes(j).toList))
        }
      }
    }
    launchCppTester((c : MSDFLMSTest) => new MSDFLMSTests(c))
  }

  @Test def testMSDFLMS() {
    class MSDFLMSTest extends Module {
      val io = new Bundle {
        val x = Vec.fill(2){UInt(INPUT, 2)}
        val inW = Vec.fill(2){UInt(INPUT, 2)}
        val y = UInt(INPUT, 2)
        val stepSize = UInt(INPUT, 2)
        val start = Bool(INPUT)
        val ybar = UInt(OUTPUT, 2)
        val outErr = UInt(OUTPUT, 2)
        val outStep = UInt(OUTPUT, 2)
        val outW = Vec.fill(2){UInt(OUTPUT, 2)}
      }
      val threeDelay = ShiftRegister(io.start, 3)
      val dotProduct = (io.x, io.inW).zipped.map((ai, bi) => MSDFMul(ai, bi, io.start)).reduce((r, c) => MSDFAdd(r, c, threeDelay)) // 5 Delay

      io.ybar := dotProduct

      val yDelay = UInt(width=2)
      yDelay := ShiftRegister(io.y, 5)
      val fiveDelay = ShiftRegister(io.start, 5)
      val err = MSDFSub(yDelay, dotProduct, fiveDelay) // 2 Delay (7 Total)
      io.outErr := err

      val stepDelay = UInt(width=2)
      stepDelay := ShiftRegister(io.stepSize, 7)
      val sevenDelay = ShiftRegister(io.start, 7)
      val step = MSDFMul(stepDelay, err, sevenDelay) // 3 Delay (10 Total)
      io.outStep := step

      val tenDelay = ShiftRegister(io.start, 10)
      val xDelay = Vec.fill(2){UInt(width=2)}
      xDelay := ShiftRegister(io.x, 10)
      val wDelay = Vec.fill(2){UInt(width=2)}
      wDelay := ShiftRegister(io.inW, 13)
      val thirteenDelay = ShiftRegister(io.start, 13)
      val wUpdate = (wDelay, xDelay.map(x1 => MSDFMul(x1, step, tenDelay))).zipped.map((w1, x1) => MSDFAdd(w1, x1, thirteenDelay)) // 5 Delay (15 Total)

      io.outW := wUpdate

    }

    class MSDFLMSTests(c : MSDFLMSTest) extends Tester(c) {

      def compare(expectedRes : Double, dRes : Double, count : Int) {
        if (count != 0) {
          val err = scala.math.abs(expectedRes - dRes)
          val correct = if (err > scala.math.pow(2, -8)) false else true
          expect(correct, "Expected: " + expectedRes.toString + "\tGot: " + dRes.toString + "\tError: " + err.toString)
        }
      }

      def update(x : List[Double], y : Double, w : List[Double], stepSize : Double) = {
        val yBar = (x, w).zipped.map(_*_).reduce(_+_)
        val err = y - yBar
        val step = stepSize*err
        val wUpdate = (x.map(step*_), w).zipped.map(_+_)
        (yBar, wUpdate, err, step)
      }

      val digitSize = 16
      val ybarDelay = 5
      val errDelay = 7
      val stepDelay = 10
      val wUpdateDelay = 15

      val stepSize : Double = 0.125

      val yBarRes = new scala.collection.mutable.Queue[Double]
      val wUpdateRes = new scala.collection.mutable.Queue[List[Double]]
      val lmsErrRes = new scala.collection.mutable.Queue[Double]
      val lmsStepRes = new scala.collection.mutable.Queue[Double]

      val yQ = new scala.collection.mutable.Queue[Double]
      val xQ = new scala.collection.mutable.Queue[List[Double]]

      // Counters
      var yBarCount = digitSize - ybarDelay
      var errCount = digitSize - errDelay
      var stepCount = digitSize - stepDelay 
      var wUpdateCount = digitSize - wUpdateDelay
      var compareCount = 0

      var updateW : Boolean = false
      var updateY : Boolean = false
      var updateErr : Boolean = false
      var updateStep : Boolean = false

      val w = List.fill(2){ArrayBuffer.fill(digitSize){0}}
      var prevW = List.fill(2){ArrayBuffer.fill(digitSize){0}}
      var yRes = new ArrayBuffer[Int]
      var errRes = new ArrayBuffer[Int]
      var stepRes = new ArrayBuffer[Int]

      for (i <- 0 until trials) {


        val dX : List[Double] = List.fill(2){r.nextDouble()/8}
        val dY : Double = dX.reduce(_*_)
        val x = dX.map(in => SignedDigit.doubleToSigned(in, digitSize))
        xQ.enqueue(x.map(in => SignedDigit.signedToDouble(in)))
        val y = SignedDigit.doubleToSigned(dY, digitSize)
        yQ.enqueue(SignedDigit.signedToDouble(y))
        val stepS = SignedDigit.doubleToSigned(stepSize, digitSize)


        // Expected Results
        if (i != 0) {
          val (yBar, wUpdate, lmsErr, lmsStep) = update(xQ.dequeue(), yQ.dequeue(), prevW.map(w1 => SignedDigit.signedToDouble(w1.toList)), stepSize)
          yBarRes.enqueue(yBar)
          wUpdateRes.enqueue(wUpdate)
          lmsErrRes.enqueue(lmsErr)
          lmsStepRes.enqueue(lmsStep)
        }

        for (j <- 0 until digitSize) {
          // X & W Input
          for (k <- 0 until 2) {
           val inX = if(j < digitSize) x(k)(j) else 0
           val inW = if(j < digitSize) w(k)(j) else 0
           poke(c.io.x(k), SignedDigit.toSignedDigit(inX)) 
           poke(c.io.inW(k), SignedDigit.toSignedDigit(inW)) 
          }

          // Y Input
          val inY = if(j < digitSize) y(j) else 0
          poke(c.io.y, SignedDigit.toSignedDigit(inY))

          // Step Input
          val inStep = if(j < digitSize) stepS(j) else 0
          poke(c.io.stepSize, SignedDigit.toSignedDigit(inStep))

          // Start Input
          val start = if (j == 0) 1 else 0
          poke(c.io.start, start)

          if (updateY) {
            yRes ++= SignedDigit.fromSignedDigit(peek(c.io.ybar).toInt)
          }

          if (updateErr) {
            errRes ++= SignedDigit.fromSignedDigit(peek(c.io.outErr).toInt)
          }

          if (updateStep){
            stepRes ++= SignedDigit.fromSignedDigit(peek(c.io.outStep).toInt)
          }

          if (updateW) {
            for (k <- 0 until 2) {
              w(k)(wUpdateCount) = SignedDigit.fromSignedDigit(peek(c.io.outW(k)).toInt)(0)
            }
          }

         yBarCount = if(yBarCount == digitSize -1) 0 else yBarCount + 1
         errCount = if(errCount == digitSize -1) 0 else errCount + 1
         stepCount = if(stepCount == digitSize -1) 0 else stepCount + 1
         wUpdateCount = if(wUpdateCount == digitSize -1) 0 else wUpdateCount + 1
         if(wUpdateCount == digitSize -1) {
          updateW = true
          prevW = w
        }
         if(yBarCount == digitSize -1) updateY = true
         if(errCount == digitSize -1) updateErr = true
         if(stepCount == digitSize -1) updateStep = true

         if (i != 0) {
           if (yBarCount == 0) {
            // yBar
            println("yBar:")
            compare(yBarRes.dequeue(), SignedDigit.signedToDouble(yRes.toList), compareCount)
            yRes = new ArrayBuffer[Int]
           }
           if (errCount == 0) {
            // lmsErr
            println("lmsErr:")
            compare(lmsErrRes.dequeue(), SignedDigit.signedToDouble(errRes.toList), compareCount)
            errRes = new ArrayBuffer[Int]
           }

           if (stepCount == 0) {
            // lmsStep
            println("lmsStep:")
            compare(lmsStepRes.dequeue(), SignedDigit.signedToDouble(stepRes.toList), compareCount)
            stepRes = new ArrayBuffer[Int]
           }

           if (wUpdateCount == 0) {
            // w
            val wRes = wUpdateRes.dequeue()
            for (j <- 0 until 2) {
              println("w(" + j.toString + "): ")
              compare(wRes(j), SignedDigit.signedToDouble(w(j).toList), compareCount)
            }
            compareCount += 1
           }
          }

          step(1)
        }


      }
    }
    launchCppTester((c : MSDFLMSTest) => new MSDFLMSTests(c))
  }
}