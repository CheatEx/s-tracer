package org.osll.stracer

import scala.Math._;

class Vector(val x: Double, val y: Double, val z: Double) extends Tuple3(x, y, z) {
  
  def +(that: Vector): Vector = new Vector(x+that.x, y+that.y, z+that.z)
  def -(that: Vector): Vector = new Vector(x-that.x, y-that.y, z-that.z)
  def unary_- : Vector = Utils.CoordinatesOrigin - this
  /**
   * Per-component multipling
   */
  def *(that: Vector): Vector = new Vector(x*that.x, y*that.y, z*that.z)
  
  /**
   * Dot product
   */
  def **(that: Vector): Double = x*that.x + y*that.y + z*that.z
  
  def +(v: Double): Vector = new Vector(x+v, y+v, z+v)
  def -(v: Double): Vector = new Vector(x-v, y-v, z-v)
  def *(v: Double): Vector = new Vector(x*v, y*v, z*v)
  def /(v: Double): Vector = new Vector(x/v, y/v, z/v)
  
  def sumsq: Double = x*x + y*y + z*z
  
  def length = sqrt(sumsq)
  
  def normalize: Vector = this / length
  
  //def map(f: Double => Double): Vector = new Vector(f(x), f(y), f(z))
  
  def foreach(f: Double => Unit): Unit = {f(x);f(y);f(z)}
}

object Utils {
  val CoordinatesOrigin = new Vector(0,0,0)
}
