package org.osll.stracer.scene

import scalala.tensor.Vector;

class Scene(val camera: Camera,
            objects: List[SceneObject],
            lights: List[Light],
            val ambientLight: Vector,
            val background: Vector) {

}
