 /*
 Copyright (c) 2011, 2012, 2013, 2014 The University of Sydney.
 All Rights Reserved.  Redistribution and use in source and 
 binary forms, with or without modification, are permitted
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

package Chisel

import Node._
import ChiselError._

import scala.collection.mutable.ArrayBuffer

object updateCounter {
  def apply(rst : UInt, w : Int) : UInt = {
    val counter = Reg(init=UInt(0, width=w))
    when (rst === UInt(1)) {counter := UInt(0)}
    counter := counter + UInt(1)
    counter
  }
}

object nextStart {
  def apply(start : Bool, delay : Int) : UInt = {
    val nsr = Reg(init=Cat(UInt(1), Fill(UInt(0), delay - 1)))
    when (start) {
      nsr := Cat(UInt(1), Fill(UInt(0), delay - 2))
    } .otherwise {
      nsr := nsr >> 1
    }
    nsr 
  }
}

object MSDFRegister {
  def apply(a : UInt) : Vec[UInt] = {
    val literalWidth = a.getWidth()/2
    val litArray = new ArrayBuffer[UInt]
    for (i <- a.getWidth() - 1 to 1 by -2) {
      val reg = Reg(init=a(i, i - 1))
      litArray.append(reg)
    }
    Vec(litArray)
  }

  def apply(a : Vec[UInt]) : UInt = {
    val literalWidth = a.length
    val counter = Reg(init=UInt(0, width=log2Up(literalWidth)))
    counter := counter + UInt(1)
    a(counter)
  }

  def apply(in : Vec[UInt], index : UInt) = in(index)
}


object MSDFAdd {
  def apply(a : UInt, b : UInt, start : Bool) = {
    val nextStart = Reg(next=start)
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
    val t = Vec.fill(a.getWidth()/2 - 1){Reg(init=UInt(0, width=1))}
    val w = Vec.fill(a.getWidth()/2){Reg(init=UInt(0, width=1))}
    val tTrans = UInt(width=1)
    val (w0, t1) = if (a.getWidth()/2 != 1) {
      FullAdder(v(0), ~bn(0), h(0))
    } else {
      FullAdder(v(0), ~bn(0), hTrans)
    }
    tTrans := ~t1
    w(0) := w0

    for (i <- 1 until a.getWidth()/2 - 1) {
      val (w1, t1) = FullAdder(v(i), ~bn(i), h(i))
      t(i - 1) := ~t1
      w(i) := w1
    }
    if (a.getWidth()/2 != 1) {
      val (w2, t2) = FullAdder(v(a.getWidth()/2 - 1), ~bn(a.getWidth()/2 - 1), hTrans)
      t(a.getWidth()/2 - 2) := ~t2
      w(a.getWidth()/2 - 1) := w2
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
    s(0) := Mux(nextStart, UInt(0), tTrans)
    s.toBits.toUInt

  }
}


object MSDFSub {
  def apply(a : UInt, b : UInt, start : Bool) = MSDFAdd(a, ~b, start)
}


/* Most Significant Digit Division
 * 1. [Initialize]
 *   x[-4] = d[-4] = w[-4] = q[0] = 0
 *   for j = -4 ... -1
 *     d[j + 1] = CA(d[j], dj5)
 *     v[j]     = 2w[j] + xj5*2^-4
 *     w[j + 1] = v[j]
 *   end for
 * 2. [Recurrence]
 *   for j = 0 ... n - 1
 *     d[j + 1] = CA(d[j], dj5)
 *     v[j]     = 2w[j] + xj5*2^-4 - q[j]dj5*2^-4
 *     qj1      = SELD(v[j])
 *     w[j + 1] = v[j] - qj1*d[j + 1]
 *     q[j + 1] = CA(q[j], qj1)
 *     Qout     = qj1
 *   end for
 */
 object MSDFDiv {
  def apply(x : UInt, d : UInt, start : Bool) : UInt = {
    val caQ = Reg(init=UInt(0, width=14))
    val caD = Reg(init=UInt(0, width=14))
    val ws = Reg(init=UInt(0, width=14))
    val wc = Reg(init=UInt(0, width=14))
    val nxtQ = Reg(init=UInt(0, width=2))

    // d[j + 1] = CA(d[j], dj5)
    val newD = UInt(width=14)
    newD := SDOnlineConversion(d, start)
    caD := newD

    // D Selector : -qj1*d[j + 1]
    val inD = UInt(width=14)
    inD := MuxCase(UInt(0), Array(
      (nxtQ === UInt("b01")) -> newD,
      (nxtQ === UInt("b10")) -> ~newD
      ))

    // Q Selector : -q[j]*dj5*2^-4
    val inQ = UInt(width=14)
    inQ := MuxCase(UInt(0), Array(
      (d === UInt("b01")) -> caQ,
      (d === UInt("b10")) -> ~caQ
      )) >> 4

    // xj5*2^-4 - q[j]*dk5*2^-4
    val inWS = UInt(width=14)
    inWS := Cat(U(x, d, nxtQ), ws(ws.getWidth() - 6, 0))
    val inWC = UInt(width=14)
    inWC := Cat(Fill(6, UInt(0)), wc(wc.getWidth() - 6, 0))

    // Carry cd
    val cD = Mux( ((nxtQ === UInt("b10")) & (d === UInt("b10"))) | ((nxtQ === UInt("b01")) & (d === UInt("b01"))), UInt(1), UInt(0))

    // v[j] = 2w[j] + xj5*2^-4 - q[j]*dj5*2^-4
    val (s1, c1) = threeToTwoAdder(inWS, inWC, inQ, cD)

    // qj1 = SELD(v[j])
    val v = s1(s1.getWidth() - 1, s1.getWidth() - 5) + c1(c1.getWidth() - 1, c1.getWidth() - 5)
    val nQ = SEL(v)

    nxtQ := nQ

    // q[j + 1] = CA(q[j], qj1)
    caQ := SDOnlineConversion(nQ, start)

    // Carry cq
    val cQ = Mux( ((nxtQ === UInt("b10")) & (d === UInt("b10"))) | ((nxtQ === UInt("b01")) & (d === UInt("b01"))), UInt(1), UInt(0))

    // w[j + 1] = v[j] - qj1*d[j + 1]
    val (s2, c2) = threeToTwoAdder(s1, c1, inD, cQ)

    ws := s2 << 1
    wc := c2 << 1

    nQ
  }

  def U(x : UInt, d : UInt, q : UInt) : UInt = {
    // Gate Network Implementation
    // // x Values
    // val xP = x(1)
    // val xN = x(0)
    // val xZ = !(xP ^ xN)

    // // d Value
    // val dP = d(1)
    // val dN = d(0)
    // val dZ = !(dP ^ dN)

    // // q Sign
    // val qS = q

    // val ui = xN + (xZ & ((dP & ~qS) + (dN & qS)))
    // val u4 = (~xN & dZ) + (~xZ & ((dP & qS) + (dN & ~qS))) + (xZ & ((dP & ~qS) + (dN & qS)))
    // Cat(Fill(ui, 5), u4)

    // Mux Implementation
    MuxCase(UInt("b000000"), Array(
      
      ((x === UInt("b10")) & (d === UInt("b10")) & (q === UInt("b01"))) -> UInt("b000001"), // 1 (Neg Q)
      ((x === UInt("b10")) & (d === UInt("b10"))) -> UInt("b000000"), // 1
      ((x === UInt("b10")) & (d === UInt("b00"))) -> UInt("b000001"), // 2
      ((x === UInt("b10")) & (d === UInt("b11"))) -> UInt("b000001"), // 2
      ((x === UInt("b10")) & (d === UInt("b01")) & (q === UInt("b01"))) -> UInt("b000000"), // 3
      ((x === UInt("b10")) & (d === UInt("b01"))) -> UInt("b000001"), // 3
      
      ((x === UInt("b00")) & (d === UInt("b10")) & (q === UInt("b01"))) -> UInt("b000000"), // 4
      ((x === UInt("b00")) & (d === UInt("b10"))) -> UInt("b111111"), // 4
      ((x === UInt("b00")) & (d === UInt("b00"))) -> UInt("b000000"), // 5
      ((x === UInt("b00")) & (d === UInt("b11"))) -> UInt("b000000"), // 5
      ((x === UInt("b00")) & (d === UInt("b01")) & (q === UInt("b01"))) -> UInt("b111111"), // 6
      ((x === UInt("b00")) & (d === UInt("b01"))) -> UInt("b000000"), // 6
      
      ((x === UInt("b11")) & (d === UInt("b10")) & (q === UInt("b01"))) -> UInt("b000000"), // 4
      ((x === UInt("b11")) & (d === UInt("b10"))) -> UInt("b111111"), // 4
      ((x === UInt("b11")) & (d === UInt("b00"))) -> UInt("b000000"), // 5
      ((x === UInt("b11")) & (d === UInt("b11"))) -> UInt("b000000"), // 5
      ((x === UInt("b11")) & (d === UInt("b01")) & (q === UInt("b01"))) -> UInt("b111111"), // 6
      ((x === UInt("b11")) & (d === UInt("b01"))) -> UInt("b000000"), // 6


      ((x === UInt("b01")) & (d === UInt("b10")) & (q === UInt("b01"))) -> UInt("b111111"), // 7
      ((x === UInt("b01")) & (d === UInt("b10"))) -> UInt("b111110"), // 7
      ((x === UInt("b01")) & (d === UInt("b00"))) -> UInt("b111111"), // 8
      ((x === UInt("b01")) & (d === UInt("b11"))) -> UInt("b111111"), // 8
      ((x === UInt("b01")) & (d === UInt("b01")) & (q === UInt("b01"))) -> UInt("b111110"),  // 9
      ((x === UInt("b01")) & (d === UInt("b01"))) -> UInt("b111111")  // 9
      ))
  }

  // Optimise by removing the LSB of a
  def SEL(a : UInt) : UInt = {
    MuxCase(UInt("b01"), Array(
      (a === UInt("b01111")) -> UInt("b10"), //  1.875
      (a === UInt("b01110")) -> UInt("b10"), //  1.750
      (a === UInt("b01101")) -> UInt("b10"), //  1.625
      (a === UInt("b01100")) -> UInt("b10"), //  1.500
      (a === UInt("b01011")) -> UInt("b10"), //  1.375
      (a === UInt("b01010")) -> UInt("b10"), //  1.250
      (a === UInt("b01001")) -> UInt("b10"), //  1.125
      (a === UInt("b01000")) -> UInt("b10"), //  1.000
      (a === UInt("b00111")) -> UInt("b10"), //  0.875
      (a === UInt("b00110")) -> UInt("b10"), //  0.750
      (a === UInt("b00101")) -> UInt("b10"), //  0.625
      (a === UInt("b00100")) -> UInt("b10"), //  0.500
      (a === UInt("b00011")) -> UInt("b10"), //  0.375
      (a === UInt("b00010")) -> UInt("b10"), //  0.250
      (a === UInt("b00001")) -> UInt("b00"), //  0.125
      (a === UInt("b00000")) -> UInt("b00"), //  0.000
      (a === UInt("b11111")) -> UInt("b00"), // -0.125
      (a === UInt("b11110")) -> UInt("b00")  // -0.250
    ))
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
  def apply(a : UInt, b : UInt, start : Bool) : UInt = {
    val caA = Reg(init=UInt(0, width=14))
    val ws = Reg(init=UInt(0, width=14))
    val wc = Reg(init=UInt(0, width=14))

    // CA(x[j], xj4)
    caA := (SDOnlineConversion(a, start) >> 3)

    // CA(y[j], yj4)
    val newCAb = UInt(width=14)
    newCAb := (SDOnlineConversion(b, start) >> 3)

    // x[j]*yj4
    val inA = UInt(width=14)
    inA := Mux(start, UInt(0), MuxCase(UInt(0), Array(
      (b === UInt("b01")) -> ~caA,
      (b === UInt("b10")) -> caA
      )))

    // y[j + 1]*xj4
    val inB = UInt(width=14)
    inB := MuxCase(UInt(0), Array(
      (a === UInt("b01")) -> ~newCAb,
      (a === UInt("b10")) -> newCAb
      ))

    // cA = 1 if a < 0
    val cA = Mux(a === UInt("b01"), UInt(1), UInt(0))
    
    // cB = 1 if b < 0
    val cB = Mux(b === UInt("b01"), UInt(1), UInt(0))
    val nextStart = Bool()
    nextStart := start
    val inWC = Mux(nextStart, UInt(0), wc)
    val inWS = Mux(nextStart, UInt(0), ws)

    // 2w[j] + (x[j]*yj4 + y[j + 1]*xj4)*2^-3
    val (newWS, newWC) = fourToTwoAdder(inWS, inA, inB, inWC, cA, cB)

    // val vS = newWS + newWC 
    val v = newWS(newWS.getWidth() - 1, newWS.getWidth() - 4) + newWC(newWC.getWidth() - 1, newWC.getWidth() - 4)
    val p = SEL(v(v.getWidth() - 1, v.getWidth() - 3))
    val magP = Mux(p === UInt(0), UInt("b0"), UInt("b1"))

    val newWSTop = UInt(width=14)
    newWSTop := Cat(UInt(1), magP^v(v.getWidth() - 2), v(v.getWidth() - 3, v.getWidth() - 4), newWS(newWS.getWidth() - 5, 0))

    ws := (newWSTop << 1)
    wc := (newWC(newWC.getWidth() - 5, 0) << 1)

    p
  }

  def SEL(a : UInt) : UInt = {
    MuxCase(UInt("b00"), Array(
      (a === UInt("b001")) -> UInt("b10"),
      (a === UInt("b010")) -> UInt("b10"),
      (a === UInt("b011")) -> UInt("b10"),
      (a === UInt("b100")) -> UInt("b01"),
      (a === UInt("b101")) -> UInt("b01"),
      (a === UInt("b110")) -> UInt("b01")
      ))
  }
}



/** Most Significat Digit Exponentiation
 * 1. [Initialize]
 *     y[0] = exp^(-0.5); w[0] = x + 0.5
 * 2. [Recurrence]
 *     for j = 0 ... m
 *         s_j = SEL(w[j])
 *         w[j + 1] = 2(w[j] - L_j*2^j)
 *         y[j + 1] = y[j] + y[j]*s_j*2^(-j)
 *     end for
 * 3. [Result]
 *     y[m + 1] = e^x
 */

//object MSDFExp {

  //def apply(a : UInt, start : Bool) : UInt = {
    
  //}

  //def SEL(a : UInt) : UInt = {
    //MuxCase(UInt("b00"), Array(
      //(a === UInt("b011")) -> UInt("b10"), // 0.75
      //(a === UInt("b010")) -> UInt("b10"), // 0.5
      //(a === UInt("b100")) -> UInt("b01")  // -0.75
      //))
  //}

  //def L(sj : UInt, j : UInt) : UInt = {
    //Mux(j > m/2, 
      //sj << j,
      //MuxCase(UInt("b00"), Array(
      //(sj === UInt("b10")) -> ln(1 + 2^(-j)),
      //(sj === UInt("b01")) -> -ln(1 + 2^(-j))
      //)))
  //}
//}


object MSDFLiteral {
  def apply(a : UInt, initialIndex : Int = 0) : UInt = {
    val literalWidth = a.getWidth()/2
    val litArray = new ArrayBuffer[UInt]
    for (i <- a.getWidth() - 1 to 1 by -2) {
      val reg = Reg(init=a(i, i - 1))
      litArray.append(reg)
    }
    val litRegs = Vec(litArray)
    for (i <- 0 until litRegs.length - 1) {
      litRegs(i) := litRegs(i + 1)
    }
    litRegs(litRegs.length - 1) := litRegs(0)
    litRegs(initialIndex)
  }
}


object SDOnlineConversion {
  def apply(a : UInt, start : Bool) : UInt = {
    val q = Reg(init=UInt(0, width=14))
    val qm = Reg(init=UInt(0, width=14))
    val counter = Reg(init=UInt(12, width=14))

    val negOne = a === UInt("b01")
    val one = a === UInt("b10")
    val zero = !negOne & !one

    // Q and QM reset
    val inQ = Mux(start, UInt(0), q)
    val inQM = Mux(start, UInt(0), qm)

    // Counter reset
    val inCounter = Mux(start, UInt(11), counter - UInt(1))

    val nextQM = MuxCase(UInt(0), Array(
      one -> Cat(inQ, UInt(0)),
      zero -> Cat(inQM, UInt(1)),
      negOne -> Cat(inQM, UInt(0))
    ))

    val nextQ = MuxCase(UInt(0), Array(
      one -> Cat(inQ, UInt(1)),
      zero -> Cat(inQ, UInt(0)),
      negOne -> Cat(inQM, UInt(1))
    ))

    q := nextQ
    qm := nextQM
    counter := inCounter

    nextQ << inCounter
  }

  def apply(in : UInt, inQ : UInt, inQM : UInt, currCounter : UInt) : (UInt, UInt, UInt, UInt) = {
    val negOne = in === UInt("b01")
    val one = in === UInt("b10")
    val zero = !negOne & !one

    val nextQM = MuxCase(UInt(0), Array(
      one -> Cat(inQ, UInt(0)),
      zero -> Cat(inQM, UInt(1)),
      negOne -> Cat(inQM, UInt(0))
      ))

    val nextQ = MuxCase(UInt(0), Array(
      one -> Cat(inQ, UInt(1)),
      zero -> Cat(inQ, UInt(0)),
      negOne -> Cat(inQM, UInt(1))
      ))

    (nextQ << currCounter, nextQ, nextQM, currCounter - UInt(1))
  }
}
