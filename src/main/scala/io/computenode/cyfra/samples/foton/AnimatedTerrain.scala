package io.computenode.cyfra.samples.foton

import io.computenode.cyfra.dsl.Algebra.{*, given}
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.dsl.Control.*
import io.computenode.cyfra.dsl.Random
import io.computenode.cyfra.foton.animation.AnimationFunctions.smooth
import io.computenode.cyfra.utility.Color.hex
import io.computenode.cyfra.utility.Units.Milliseconds
import io.computenode.cyfra.foton.*
import io.computenode.cyfra.foton.rt.animation.{AnimatedScene, AnimationRtRenderer}
import io.computenode.cyfra.foton.rt.shapes.{Plane, Shape, Sphere, Box}
import io.computenode.cyfra.foton.rt.{Camera, Material}
import scala.concurrent.duration.DurationInt

import java.nio.file.Paths

object AnimatedTerrain:    
  def distanceGradientDotProduct(grid_x: Int32, grid_y: Int32, x: Float32, y: Float32): Float32 = {
    val random = Random(999)
    val (_, randGradient) = random.next[Vec2[Float32]]
    
    val x_dist = x - grid_x.asFloat
    val y_dist = y - grid_y.asFloat

    x_dist * randGradient.x + y_dist * randGradient.y
  }

  def cubicInterpolation(a: Float32, b: Float32, weight: Float32): Float32 = {
    (b - a) * (3f - weight * 2f) * weight * weight + a
  }

  def perlinNoiseBox(x: Float32, y: Float32, step_size: Int32): Box = {
    val boxMaterial = Material(
      color = (0.3f, 0.3f, 1f),
      emissive = vec3(0f),
      percentSpecular = 0.5f,
      specularColor = (0.3f, 0.3f, 1f) * 0.1f,
      roughness = 0.1f
    )

    // Get x,y of corners
    val x0 = x.asInt
    val y0 = y.asInt
    val x1 = x0 + step_size
    val y1 = y0 + step_size

    // Weights for interpolatiohn
    val weight_x = x - x0.asFloat
    val weight_y = y - y0.asFloat

    val top_a = distanceGradientDotProduct(x0, y0, x, y)
    val top_b = distanceGradientDotProduct(x1, y0, x, y)
    val top_interpolate = cubicInterpolation(top_a, top_b, weight_x)

    val bot_a = distanceGradientDotProduct(x0, y1, x, y)
    val bot_b = distanceGradientDotProduct(x1, y1, x, y)
    val bot_interpolate = cubicInterpolation(bot_a, bot_b, weight_x)

    val noise = cubicInterpolation(top_interpolate, bot_interpolate, weight_y)

    val clippedNoise = when(noise > 1f){
      1f
    }.elseWhen(noise < 0f){
      0f
    }.otherwise(noise)

    println(clippedNoise)

    Box((x0.asFloat, 0f, y0.asFloat), (x1.asFloat, -1f, y1.asFloat), boxMaterial)
  }
  
  def generateTerrain(width: Int, depth: Int, scale: Float32, step_size: Int): List[Shape] = {        
    val terrain = for {
      x <- 0 until width by step_size
      z <- 0 until depth by step_size
    } yield {      
      perlinNoiseBox(x.toFloat * scale, z.toFloat * scale, step_size)      
    }
    terrain.toList
  }
  
  @main
  def terrain() =
    val lightMaterial = Material(
      color = (1f, 0.3f, 0.3f),
      emissive = vec3(40f)
    )        

    val floorMaterial = Material(
      color = vec3(0.5f),
      emissive = vec3(0f),
      roughness = 0.9f
    )

    val boxMaterial = Material(
      color = (0.3f, 0.3f, 1f),
      emissive = vec3(0f),
      percentSpecular = 0.5f,
      specularColor = (0.3f, 0.3f, 1f) * 0.1f,
      roughness = 0.1f
    )

    val staticShapes: List[Shape] = List(      
      // Light
      Sphere((-140f, -140f, 10f), 50f, lightMaterial),
      // Floor
      Plane((0f, 3.5f, 0f), (0f, 1f, 0f), floorMaterial),
    )

    val staticShapes2: List[Shape] = List(      
      Box((0f, 0f, 0f), (1f, -0.3f, 1f), boxMaterial),
      Box((0f, 0f, 1f), (1f, -1f, 2f), boxMaterial),
      Box((1f, 0f, 0f), (2f, -0.8f, 1f), boxMaterial),
      Box((1f, 0f, 1f), (2f, -0.6f, 2f), boxMaterial),
    )

    val scene = AnimatedScene(
      // shapes = staticShapes ::: generateTerrain(50, 50, 1f, 5),
      shapes = staticShapes ::: staticShapes2,
      camera = Camera(position = (0f, 0f, -10f)),
      duration = 2.seconds
    )

    val parameters = AnimationRtRenderer.Parameters(
      width = 640,
      height = 360,
      superFar = 300f,
      pixelIterations = 500,
      iterations = 1,
      bgColor = hex("#ADD8E6"),
      framesPerSecond = 1
    )
    val renderer = AnimationRtRenderer(parameters)
    renderer.renderFramesToDir(scene, Paths.get("output"))

// Renderable with ffmpeg -framerate 30 -pattern_type sequence -start_number 01 -i frame%02d.png -s:v 1920x1080 -c:v libx264 -crf 17 -pix_fmt yuv420p output.mp4

// ffmpeg -t 3 -i output.mp4 -vf "fps=30,scale=720:-1:flags=lanczos,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" -loop 0 output.gif
