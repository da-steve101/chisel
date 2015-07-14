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

package Chisel

import Node._
import ChiselError._

object LSDF {

  // Type Constructors
  //def apply[T <: Data](wire : T, digit : Int) : LSDF = apply(wire.dir, wire.getWidth, digit)

  // Base Constructors
  def apply(x : Int, width : Int, digit : Int) : LSDF = apply(BigInt(x), width, digit)
  def apply(x : BigInt, width : Int, digit : Int) : LSDF =  {
    val res = Lit(x, digit){LSDF()}
    res.totalWidth = width
    res
  }

  def apply(dir : IODirection = null, width : Int = -1, digit : Int = 1) : LSDF = {
    val res = new LSDF(width);
    res.create(dir, digit)
   res
  }

}

class LSDF(var totalWidth : Int = 0, var regDelay : Int = 0) extends Bits with Num[LSDF] {
  type T = LSDF

  def getStages(totalWidth : Int, digit : Int) = scala.math.round(totalWidth.toDouble/digit.toDouble).toInt

  def checkAligned(a : LSDF, b : LSDF) {
    if(a.getWidth() != b.getWidth()) ChiselError.error("LSBF Digit-Serial: Digit Arithmetic not Aligned")
    if(a.getTotalWidth() != b.getTotalWidth()) ChiselError.error("LSBF Digit-Serial: Width Miss Match")
  }

  def getTotalWidth() : Int = this.totalWidth

  /* Fixed Factory Method */
  override def fromNode(n : Node): this.type = {
    val res = LSDF(OUTPUT, this.getTotalWidth(), this.getWidth()).asTypeFor(n).asInstanceOf[this.type]
    if (n.isReg) res.regDelay += 1
    println("FromNode")
    res
  }

  override def fromInt(x : Int) : this.type = LSDF(x, this.getTotalWidth(), this.getWidth()).asInstanceOf[this.type]

  override def cloneType: this.type = {
    val res = LSDF(this.dir, this.getTotalWidth(), this.getWidth()).asInstanceOf[this.type]
    res.regDelay += 1
    println("cloneType")
    res
  }

  def fromUInt(s : UInt) : LSDF = chiselCast(s){LSDF(INPUT, this.getTotalWidth(), this.getWidth())}


  def wrapAround(n: UInt, max: UInt) = Mux(n > max, UInt(0), n)

  def counter(max: UInt, amt: UInt, init : Int = 0): UInt = {
    println(init) 
    println(max.litValue())
    val x = Reg(init=UInt(init, max.getWidth()))
    x := wrapAround(x + amt, max)
    x
  }

  // Order Helper Functions
  def isNew(stg : Int) : Bool = Mux(counter(UInt(stg - 1), UInt(1)) === UInt(stg - 1), Bool(true), Bool(false))
  
  def pipeline(wire : Bool, isNew : Bool) : Bool = {
    val x = Reg(init=Bool(true))
    x := Mux(isNew, Bool(true) , x & wire)
    x
  }
  
  def performOOP(a : LSDF, b : LSDF, op :(LSDF, LSDF) => Bool) : Bool = {
    checkAligned(a, b)
    val newExample = isNew(getStages(b.getTotalWidth(), b.getWidth()))
    val opRes = op(a, b)
    val reg = pipeline(opRes, newExample)
    opRes & reg
  }

  // Order Operators
  def > (b : LSDF) : Bool = performOOP(this, b, _.toUInt > _.toUInt)

  def < (b : LSDF) : Bool = performOOP(this, b, _.toUInt < _.toUInt)

  def >= (b : LSDF) : Bool = performOOP(this, b, _.toUInt >= _.toUInt)

  def <= (b : LSDF) : Bool = performOOP(this, b, _.toUInt <= _.toUInt)

  def === (b : LSDF) : Bool = performOOP(this, b, _.toUInt > _.toUInt)

  def >> (b : UInt) : LSDF = {
    fromUInt(this.toUInt >> b)
  }

  // Arithmetic Helper Functions
  def isLast(stg : Int) : Bool = Mux(counter(UInt(stg - 1), UInt(1)) === UInt(stg - 1), Bool(true), Bool(false))
    
  // Arithmetic Operators
  def unary_-() : LSDF = LSDF(0, this.getTotalWidth(), this.getWidth()) - this

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

  def + (b : LSDF) : LSDF = {
    println(this.isReg)
    println(b.isReg)
    checkAligned(this, b)
    val newExample = isLast(getStages(b.getTotalWidth(), b.getWidth()))
    val x = Reg(init=UInt(0))
    val (res, cout) = carryAdd(this.toUInt, b.toUInt, x)
    x := Mux(newExample, UInt(0, width=1), cout)
    val theResult = fromUInt(res)
    theResult
  }
  
  def - (b : LSDF) : LSDF = {
    checkAligned(this, b)
    val newExample = isLast(getStages(b.getTotalWidth(), b.getWidth()))
    val x = Reg(init=UInt(1))
    val (res, cout) = carryAdd(this.toUInt, ~b.toUInt, x)
    x := Mux(newExample, UInt(1, width=1), cout)
    val theResult = fromUInt(res)
    theResult
  }

  def * (b : LSDF) : LSDF = {
    checkAligned(this, b)
    println(this.isReg)
    println(b.isReg)
    val stg = getStages(this.getTotalWidth(), this.getWidth())
    val init = if(this.regDelay == 0) 0 else stg - 1 - regDelay
    val count = counter(UInt(stg - 1), UInt(1), init) 
    val digitInit = if(this.regDelay == 0) 0 else this.getTotalWidth() - 1 - regDelay*this.getWidth()
    println("digitInit: " + digitInit.toString)
    val digitCount = counter(UInt(this.getTotalWidth() - 1), UInt(this.getWidth()), digitInit) 
    val x1 = Reg(init=UInt(0, width=this.getTotalWidth()))
    val x2 = Reg(init=UInt(0, width=b.getTotalWidth()))
    
    val newX1fl = (this.toUInt << digitCount) | x1
    val newX1 = Mux(count === UInt(0), this.toUInt, newX1fl) 
    
    val newX2fl = (b.toUInt << digitCount) | x2
    val newX2 = Mux(count === UInt(0), b.toUInt, newX2fl) 
    
    val res = (newX1 * newX2) >> digitCount 
    x1 := newX1
    x2 := newX2
    val theResult = fromUInt(res)
    theResult
  }

  def / (b : LSDF) : LSDF = {
    fromUInt(this.toUInt / b.toUInt)
  }

  def % (b : LSDF) : LSDF = fromUInt(this.toUInt % b.toUInt)
}
