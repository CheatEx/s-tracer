package org.osll.stracer

import scala.Iterable._

import scalala.Scalala._
import scalala.tensor.Vector

import org.osll.stracer.Utils._

class Scene(val camera: Camera,
            objects: List[MaterialObject],
            val lights: List[Light],
            val ambientLight: Vector,
            val background: Vector) {
  def closestIntersection(ray: Ray): Intersection = Iterable.min(intersectAll(ray))
  
  def intersectAll(ray: Ray): List[Intersection] =
    for (sceneObject <- objects) yield sceneObject intersectionWith ray
}

class Camera(pos: Vector, at: Vector, upo: Vector) extends DirectedSceneObject(pos, at) {
	val up = normalize(upo)
}

abstract class SceneObject(val pos: Vector)

abstract class DirectedSceneObject(pos: Vector, val at: Vector) extends SceneObject(pos)

abstract class MaterialObject(pos: Vector, val material: Material) extends SceneObject(pos) {
  def intersectionWith(ray: Ray): Intersection
}

class Sphere(pos: Vector, material: Material, val radius: Double) extends MaterialObject(pos, material) {
  override def intersectionWith(ray: Ray): Intersection = {
    val t = Vector(ray.origin(0) - pos(0),
    			   ray.origin(1) - pos(1),
    			   ray.origin(2) - pos(2))
    val A = sumsq(ray.direction)
    val B = 2 * ((ray.direction(0) * t(0)) + (ray.direction(1) * t(1))) + (ray.direction(2) * t(2));
	val C = sumsq(t) - (radius * radius);
	var temp = B * B - 4 * A * C;
	
	var t0 = 0: Double
	var t1 = 0: Double
	var t2 = 0: Double
	if (temp<0) {
	  return InfinityIntersection
	} else if (temp > 0) {
	  temp = Math.sqrt(temp);
	  t1 = (-B + temp) / (2 * A);
	  t2 = (-B - temp) / (2 * A);
	  if (t1 > t2) {
		t0 = t1;
		t1 = t2;
		t2 = t0;
	  }

	  t0 = 0.0;
	  if (t1 > 1e-6)
		t0 = t1;
	  if (t2 > 0 && t0 <= 1e-6)
		t0 = t2;
	  if (t0 < 1e-3)
		return InfinityIntersection
	} else {
	  t0 = -B / (2 * A);
	  if (t0 < 1e-3)
		return InfinityIntersection
	}
	val p = (ray.direction * t0) + ray.origin value
	val n = (p - pos) / radius value
   
	new ObjectIntersection(p, n, this, t0)
  }
}

class Triangle(val first: Vector, val second: Vector, val third: Vector) {
  
  def intersectWith(ray: Ray): Intersection = {
		val l = normalize(second - first value)
		val m = normalize(third - first value)
//		val n = normalize(l * m value)

//		val nd = n * ray.direction
//		if (nd > -1e-6 && nd < 1e-6) {
//			return InfinityIntersection
//		}
//
//		double t = -(-n.dot(p0) + n.dot(o)) / nd;
//		if (t <= 1e-6) {
//			return InfinityIntersection
//		}
//
//		Vector3d ip = new Vector3d(d);
//		ip.scaleAdd(t, o); // the intersection point
//
//		// project the triangle on one of the primary planes---the plane with
//		// maximum projection.
//		double u0 = 0, u1 = 0, u2 = 0, w0 = 0, w1 = 0, w2 = 0;
//		double nx = Math.abs(n.x);
//		double ny = Math.abs(n.y);
//		double nz = Math.abs(n.z);
//		if (nx >= ny && nx >= nz) {
//			u0 = ip.y - p0.y;
//			u1 = l.y;
//			u2 = m.y;
//			w0 = ip.z - p0.z;
//			w1 = l.z;
//			w2 = m.z;
//		}
//		if (ny >= nx && ny >= nz) {
//			u0 = ip.x - p0.x;
//			u1 = l.x;
//			u2 = m.x;
//			w0 = ip.z - p0.z;
//			w1 = l.z;
//			w2 = m.z;
//		}
//		if (nz >= nx && nz >= ny) {
//			u0 = ip.x - p0.x;
//			u1 = l.x;
//			u2 = m.x;
//			w0 = ip.y - p0.y;
//			w1 = l.y;
//			w2 = m.y;
//		}
//
//		double t1 = u0 * w2 - w0 * u2;
//		double t2 = u1 * w0 - w1 * u0;
//		double t3 = u1 * w2 - w1 * u2;
//		if (t3 > -1e-6 && t3 < 1e-6) {
//			// singular system
//			return null;
//		}
//
//		double alpha = t1 / t3;
//		double beta = t2 / t3;
//		if (!(alpha >= 0 && beta >= 0 && alpha + beta <= 1)) {
//			// intersected point is outside the triangle, ignore it.
//			return null;
//		}
//
//		// Creating an intersection object
//		Intersect intersection = new Intersect();
//		intersection.setT(t);
//		intersection.setHitPoint(ip);
//		intersection.setHitNormal(n);
//		intersection.setHitObject(this.getPolyObj());
//
//		return intersection;
		InfinityIntersection
  }
}

class Polygon(
  material: Material,
  val vertexes: List[Vector],
  val triangles: List[Tuple3[Int, Int, Int]])
	extends MaterialObject(Vector(), material) { //position don't matter since all actual data in vertexes
	  
   override def intersectionWith(ray: Ray): Intersection = InfinityIntersection
}

class Material(val Ka: Vector,
               val Kd: Vector,
               val Ks: Vector,
               val alpha: Double,
               val rI: Double,
               val rC: Double,
               val tC: Double,
               val lC: Double)

abstract class Light(pos: Vector, val intensity: Vector) extends SceneObject(pos)

class PointLight(pos: Vector, intensity: Vector) extends Light(pos, intensity)

class SpotLight(pos: Vector, val at: Vector, val angle: Double, intensity: Vector)
  extends Light(pos, intensity)