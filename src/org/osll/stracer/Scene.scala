package org.osll.stracer

import scalala.tensor.Vector;
import scala.Iterable._

import org.osll.stracer.Utils._

class Scene(val camera: Camera,
            objects: List[SceneObject],
            lights: List[Light],
            val ambientLight: Vector,
            val background: Vector) {
  def closestIntersection(ray: Ray): Intersection = min(intersectAll(ray))
  def intersectAll(ray: Ray): List[Intersection] = List(InfinityIntersection)
}

class Camera(pos: Vector, at: Vector, upo: Vector) extends DirectedSceneObject(pos, at) {
	val up = normalize(upo)
}

abstract class SceneObject(val pos: Vector)

abstract class DirectedSceneObject(pos: Vector, val at: Vector) extends SceneObject(pos)

class MaterialObject(pos: Vector, val material: Material) extends SceneObject(pos)

class Sphere(pos: Vector, material: Material, val radius: Double) extends MaterialObject(pos, material)

class Triangle(val first: Vector, val second: Vector, val third: Vector)

class Polygon(
  material: Material,
  val vertexes: List[Vector],
  val triangles: List[Tuple3[Int, Int, Int]])
	extends MaterialObject(Vector(), material) //position don't matter since all actual data in vertexes

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