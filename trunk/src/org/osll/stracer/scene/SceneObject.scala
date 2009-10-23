package org.osll.stracer.scene

import scalala.tensor.Vector;

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