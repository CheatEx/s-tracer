package org.osll.stracer.scene

import scalala.Scalala._;
import scalala.tensor.Vector;

import org.osll.stracer.Utils._

class Camera(pos: Vector, at: Vector, upo: Vector) extends DirectedSceneObject(pos, at) {
	val up = normalize(upo)
}
