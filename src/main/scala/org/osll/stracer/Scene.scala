package org.osll.stracer

import scala.Iterable._
import scala.Math._

import org.osll.stracer.Utils._

class Scene(val camera: Camera,
            objects: List[MaterialObject],
            val lights: List[Light],
            val ambientLight: Vector,
            val background: Vector) {
  def closestIntersectionWith(ray: Ray): Intersection = Iterable.min(allIntersectionsWith(ray))
  
  def allIntersectionsWith(ray: Ray): List[Intersection] =
    for (sceneObject <- objects) yield sceneObject intersectionWith ray
}

class Camera(at: Vector, upo: Vector, val viewport: Double)
	extends DirectedSceneObject(CoordinatesOrigin, at) {
  val up = upo.normalize
}

abstract class SceneObject(val pos: Vector)

abstract class DirectedSceneObject(pos: Vector, val at: Vector) extends SceneObject(pos)

abstract class MaterialObject(pos: Vector, val material: Material) extends SceneObject(pos) {
  def intersectionWith(ray: Ray): Intersection
}

class Sphere(pos: Vector, material: Material, val radius: Double) extends MaterialObject(pos, material) {
  /**
   * TODO test it!
   */
  override def intersectionWith(ray: Ray): Intersection = {
    val t = new Vector(ray.origin.x - pos.x,
    			   ray.origin.y - pos.y,
    			   ray.origin.z - pos.z)
    val A = ray.direction.sumsq
    val B = 2 * ((ray.direction.x * t.x) + (ray.direction.y * t.y)) + (ray.direction.z * t.z);
		val C = t.sumsq - (radius * radius);
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
		val p = (ray.direction * t0) + ray.origin
		val n = (p - pos) / radius
	   
		new ObjectIntersection(p, n, this, t0)
  }
}

class Triangle(val first: Vector, val second: Vector, val third: Vector) {
  /**
   * TODO implement triangle intersection
   */
  def intersectWith(ray: Ray): Intersection = {
		val l = second - first normalize
		val m = third - first normalize;
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
	extends MaterialObject(new Vector(0,0,0), material) { //position don't matter since all actual data in vertexes

  /**
   * TODO implement polygon intersection algorithm
   */
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

class SpotLight(pos: Vector, val at: Vector, degreesAngle: Double, intensity: Vector)
  extends Light(pos, intensity) {
    val angle = degreesAngle * Pi / 180
  }
