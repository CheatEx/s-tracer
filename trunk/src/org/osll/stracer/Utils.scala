package org.osll.stracer

import scalala.Scalala._;
import scalala.tensor.Vector;

object Utils {
  def normalize(vector: Vector): Vector = {
    val length: Double = sqrt(sumsq(vector))
    vector / length value
  }
  
  def module(vector: Vector): Double =
    sqrt(sumsq(vector))
}
