package org.osll.stracer

import scalala.tensor.Vector;
import scala.Iterable._

import org.osll.stracer.Utils._

class Scene(val camera: Camera,
            objects: List[SceneObject],
            lights: List[Light],
            val ambientLight: Vector,
            val background: Vector) {
  def intersectClosest(ray: Ray): Intersection = min(intersectAll(ray))
  def intersectAll(ray: Ray): List[Intersection] = List(InfinityIntersection)
}

class Camera(pos: Vector, at: Vector, upo: Vector) extends DirectedSceneObject(pos, at) {
	val up = normalize(upo)
}

class SceneObject(val pos: Vector)

class DirectedSceneObject(pos: Vector, val at: Vector) extends SceneObject(pos)

class MaterialObject(pos: Vector, val material: Material) extends SceneObject(pos)

class Sphere(pos: Vector, material: Material, val radius: Double) extends MaterialObject(pos, material)

class Triangle(val first: Vector, val second: Vector, val third: Vector)

class Polygon(
  material: Material,
  val vertexes: List[Vector],
  val triangles: List[Tuple3[Int, Int, Int]])
	extends MaterialObject(Vector(), material) //position don't matter since all actual data in vertexes

class Material()

trait Light

class PointLight(pos: Vector) extends SceneObject(pos) with Light

class SpotLight(pos: Vector, at: Vector, val angle: Double) extends DirectedSceneObject(pos, at) with Light