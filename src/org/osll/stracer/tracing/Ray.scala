package org.osll.stracer.tracing

import scalala.Scalala._;
import scalala.tensor.Vector;

import org.osll.stracer.scene._

class Ray(val origin: Vector, val direction: Vector)

class ExtendedRay(origin: Vector, direction: Vector, val obj: SceneObject) extends Ray(origin, direction)