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

/* Signed Digit Representation
 * a = 1, r = 2
 * S = {-1, 0, 1}
 *   00 = 0
 *   01 = -1
 *   10 = 1
 *   11 = 0
 */
object SignedDigit {

  def signedToDouble(in : List[Int]) : Double = {
    var out : Double = 0.0
    for (i <- 0 until in.length) {
      out += in(i).toDouble*scala.math.pow(2, -(i+1))
    }
    out
  }

  def doubleToSigned(in : Double, width : Int) : List[Int] = {
    val out = new ArrayBuffer[Int]
    var currIn = in
    for (i <- 0 until width) {
      if (scala.math.pow(2, -(i+1)) <= currIn) {
        out.append(1)
        currIn = currIn - scala.math.pow(2, -(i+1))
      } else {
        out.append(0)
      }
    }
    out.toList
  }

  def signedListToUInt(in : List[Int]) : String = {
    val out = in.map(x => if (x == 1) "10" else if (x == -1) "01" else "00")
    "b" + out.mkString("")
  } 

  def toSignedDigit(a : List[Int]) : BigInt = {
    val signedEle = a.map(toSignedDigit(_))
    var res : BigInt = BigInt(0)
    for (i <- signedEle.length - 1 to 0 by -1) {
      res += (signedEle(i) << ((signedEle.length - 1 - i)*2))
    }
    res
  }

  def toSignedDigit(a : Int) : BigInt = if (a == -1) BigInt(1) else toSignedDigit(BigInt(a))
  def toSignedDigit(a : BigInt) : BigInt = {
    var count = 0
    var res : BigInt = BigInt(0)
    for (i <- 0 until a.bitLength) {
      if (a.testBit(i)) {
        res = res.setBit(count+1)
      }
      count += 2
    }
    res
  }

  def signedArraytoInt(signedDigits : ArrayBuffer[Int]) : BigInt = {
    var res : BigInt = BigInt(0)
    for (i <- 0 until signedDigits.length) {
      res += signedDigits(i)*scala.math.pow(2, i).toInt
    }
    res
  }

  // def fromSignedDigit(a : Int) : Int = fromSignedDigit(BigInt(a)).toInt
  def fromSignedDigit(a : BigInt) : ArrayBuffer[Int] = fromSignedDigit(a, 1)
  def fromSignedDigit(a : BigInt, expectedDigits : Int) : ArrayBuffer[Int] = {
    var signedDigits = new ArrayBuffer[Int]
    var i = 0
    while (i < a.bitLength) {
      if(a.testBit(i+1) && a.testBit(i)) {
        signedDigits.prepend(0)
      } else if(a.testBit(i+1) && !a.testBit(i)) {
        signedDigits.prepend(1)
      } else if(!a.testBit(i+1) && a.testBit(i)) {
        signedDigits.prepend(-1)
      } else {
        signedDigits.prepend(0)
      }
      i += 2
    }
    if (signedDigits.length != expectedDigits) {
      for (i <- 0 until (expectedDigits - signedDigits.length))
        signedDigits.prepend(0)
    }
    signedDigits
  }

  def fromSignedDigitInt(a : BigInt) : BigInt = {
     var signedDigits = new ArrayBuffer[Int]
     var i = 0
     while (i < a.bitLength) {
       if(a.testBit(i+1) && a.testBit(i)) {
         signedDigits.append(0)
       } else if(a.testBit(i+1) && !a.testBit(i)) {
         signedDigits.append(1)
       } else if(!a.testBit(i+1) && a.testBit(i)) {
         signedDigits.append(-1)
       } else {
         signedDigits.append(0)
       }
       i += 2
     }
     var res : BigInt = BigInt(0)
     for (i <- 0 until signedDigits.length) {
       res += signedDigits(i)*scala.math.pow(2, i).toInt
     }
     res
  }
}

object SignedDigitAdder {
  def apply(a : UInt, b : UInt) : UInt = {
    
    // Create a and b pos and neg
    val ap = Vec.fill(a.getWidth()/2){UInt(width=1)}
    val an = Vec.fill(a.getWidth()/2){UInt(width=1)}
    val bp = Vec.fill(b.getWidth()/2){UInt(width=1)}
    val bn = Vec.fill(b.getWidth()/2){UInt(width=1)}
    val sp = Vec.fill(b.getWidth()/2){UInt(width=1)}
    val sn = Vec.fill(b.getWidth()/2 + 1){UInt(width=1)}
    sn(0) := UInt(0)
    var count = 0
    for (i <- 0 until a.getWidth()) {
      if (i%2 == 0) {
        an(count) := a(i)
        bn(count) := b(i)
      } else {
        ap(count) := a(i)
        bp(count) := b(i)
        count += 1
      }

    }

    // Now we hook up the adders
    val h = Vec.fill(a.getWidth()/2 + 1){UInt(width=1)}
    h(0) := UInt(0)
    for (i <- 0 until a.getWidth()/2) {
      val (v0, h1) = FullAdder(ap(i), ~an(i), bp(i))
      h(i+1) := h1
      val (w0, t1) = FullAdder(v0, ~bn(i), h(i))
      sn(i+1) := ~t1
      sp(i) := w0
    }
    val s = Vec.fill(a.getWidth()){UInt(width=1)}
    count = 0
    for (i <- 0 until a.getWidth()) {
      if (i%2 == 0) {
        s(i) := sn(count)
      } else {
        s(i) := sp(count)
        count += 1
      }
    }
    s.toBits.toUInt
  }
}