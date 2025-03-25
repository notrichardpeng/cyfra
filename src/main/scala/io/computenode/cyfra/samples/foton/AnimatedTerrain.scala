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
import io.computenode.cyfra.foton.rt.shapes.{Plane, Shape, Sphere, Box, Quad}
import io.computenode.cyfra.foton.rt.{Camera, Material}
import scala.concurrent.duration.DurationInt

import java.nio.file.Paths

object AnimatedTerrain:    
  def distanceGradientDotProduct(grid_x: Int32, grid_y: Int32, x: Float32, y: Float32, random: Random): (Float32, Random) = {    
    val (nextRandom: Random, randGradient: Vec2[Float32]) = random.next[Vec2[Float32]]
    
    val x_dist = x - grid_x.asFloat
    val y_dist = y - grid_y.asFloat

    (x_dist * randGradient.x + y_dist * randGradient.y, nextRandom)
  }

  def cubicInterpolation(a: Float32, b: Float32, weight: Float32): Float32 = {
    (b - a) * (3f - weight * 2f) * weight * weight + a
  }

  def perlinNoise(x: Float32, y: Float32, step_size: Int32, random: Random): (Float32, Float32, Float32, Float32, Float32, Random) = {
    val boxMaterial = Material(
      color = (0.3f, 0.3f, 1f),
      emissive = vec3(0.3f),      
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

    val (top_a, r2) = distanceGradientDotProduct(x0, y0, x, y, random)
    println(top_a)
    val (top_b, r3) = distanceGradientDotProduct(x1, y0, x, y, r2)
    println(top_b)
    val top_interpolate = cubicInterpolation(top_a, top_b, weight_x)

    val (bot_a, r4) = distanceGradientDotProduct(x0, y1, x, y, r3)
    println(bot_a)
    val (bot_b, r5) = distanceGradientDotProduct(x1, y1, x, y, r4)
    println(bot_b)
    val bot_interpolate = cubicInterpolation(bot_a, bot_b, weight_x)

    val noise = cubicInterpolation(top_interpolate, bot_interpolate, weight_y)  

    // val clippedNoise = when(noise > 1f){
    //   1f
    // }.elseWhen(noise < 0f){
    //   0f
    // }.otherwise(noise)

    // println(clippedNoise)

    // Box((x0.asFloat * scale, -1f * scale, y0.asFloat * scale), (x1.asFloat * scale, 0f, y1.asFloat * scale), boxMaterial)
    (noise, x0.asFloat, y0.asFloat, x1.asFloat, y1.asFloat, r5)
  }
  
  def renderBoxWithQuads(min: Vec3[Float32], max: Vec3[Float32]): List[Quad] = {
    val boxMaterial = Material(
      color = (0.3f, 0.3f, 1f),
      emissive = vec3(0f),      
      roughness = 0.1f
    )

    List(
      // Bottom face (touching the plane)
      // Quad((min.x, min.y, min.z), (max.x, min.y, min.z), (max.x, min.y, max.z), (min.x, min.y, max.z), boxMaterial),
      
      // Top face
      Quad((min.x, max.y, min.z), (max.x, max.y, min.z), (max.x, max.y, max.z), (min.x, max.y, max.z), boxMaterial),

      // Front face
      Quad((min.x, min.y, max.z), (max.x, min.y, max.z), (max.x, max.y, max.z), (min.x, max.y, max.z), boxMaterial),

      // Back face
      // Quad((min.x, min.y, min.z), (max.x, min.y, min.z), (max.x, max.y, min.z), (min.x, max.y, min.z), boxMaterial),

      // Left face
      Quad((min.x, min.y, min.z), (min.x, min.y, max.z), (min.x, max.y, max.z), (min.x, max.y, min.z), boxMaterial),

      // Right face
      Quad((max.x, min.y, min.z), (max.x, min.y, max.z), (max.x, max.y, max.z), (max.x, max.y, min.z), boxMaterial)
    )
  }

  def generateTerrain(width: Int, depth: Int, scale: Float32, step_size: Int): List[Shape] = {  
    val x_offset = 1f
    val y_offset = 3.5f
    val z_offset = 8f
    val max_height = 10f // Negative coordinates are higher on y-axis
    var initRandom = Random(999)

    val boxMaterial = Material(
      color = (0.3f, 0.3f, 1f),
      emissive = vec3(0f),      
      roughness = 0.1f
    )

    val terrain = (0 until width by step_size).flatMap { x =>
      val (shapes, _) = (0 until depth by step_size).foldLeft((List.empty[Shape], initRandom)) {
        case ((shapes, random), z) =>
          val (noise, x0, z0, x1, z1, nextRandom) = perlinNoise(x.toFloat, z.toFloat, step_size, random)

          var modifiedNoise = noise * 1.2f
          modifiedNoise = when(noise > 1f) { 1f }
            .elseWhen(noise < -1f) { -1f }
            .otherwise(noise)
          modifiedNoise = (modifiedNoise + 1f) * 0.5f

          val minV = ((x0 + x_offset) * scale, y_offset * scale, (z0 + z_offset) * scale)
          val maxV = ((x1 + x_offset) * scale, (max_height * modifiedNoise + y_offset) * scale, (z1 + z_offset) * scale)

          // (shapes ::: renderBoxWithQuads(minV, maxV), nextRandom)
          (shapes :+ Box(minV, maxV, boxMaterial), nextRandom)
      }

      shapes  // Ensure that `flatMap` receives an Iterable (List[Shape])
    }

    // val terrain = for {
    //   x <- 0 until width by step_size
    //   z <- 0 until depth by step_size
    // } yield {                                    
    //   val (noise, x0, z0, x1, z1, nextRandom) = perlinNoise(x.toFloat, z.toFloat, step_size, random)

    //   // println((noise, x0, z0, x1, z1))

    //   var modifiedNoise = noise * 1.2f // Contrast
    //   modifiedNoise = when(noise > 1f){ // Clipping
    //     1f
    //   }.elseWhen(noise < -1f){
    //     -1f
    //   }.otherwise(noise)
    //   modifiedNoise = (modifiedNoise + 1f) * 0.5f // Normalize

    //   val minV = ((x0 + x_offset) * scale, y_offset * scale, (z0 + z_offset) * scale)
    //   val maxV = ((x1 + x_offset) * scale, (max_height * modifiedNoise + y_offset) * scale, (z1 + z_offset) * scale)

    //   random = nextRandom

    //   renderBoxWithQuads(minV, maxV)
    // }

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

    val staticShapes: List[Shape] = List(      
      // Light
      Sphere((-140f, -140f, 10f), 50f, lightMaterial),
      // Floor
      Plane((0f, 3.5f, 0f), (0f, 1f, 0f), floorMaterial),
    )    

    // val tinyBoxMaterial = Material(vec3(0.8f, 0.2f, 0.2f), vec3(0f)) // Red box

    // // Box position (centered at x = 1, y = 3.55, z = 5)
    // val min = (1f, 3.5f, 10f) // Bottom-left-back corner
    // val max = (2f, 3.0f, 11f) // Top-right-front corner

    // val tinyBox: List[Quad] = List(
    //   // Bottom face (touching the plane)
    //   Quad((min._1, min._2, min._3), (max._1, min._2, min._3), (max._1, min._2, max._3), (min._1, min._2, max._3), tinyBoxMaterial),
      
    //   // Top face
    //   Quad((min._1, max._2, min._3), (max._1, max._2, min._3), (max._1, max._2, max._3), (min._1, max._2, max._3), tinyBoxMaterial),

    //   // Front face
    //   Quad((min._1, min._2, max._3), (max._1, min._2, max._3), (max._1, max._2, max._3), (min._1, max._2, max._3), tinyBoxMaterial),

    //   // Back face
    //   Quad((min._1, min._2, min._3), (max._1, min._2, min._3), (max._1, max._2, min._3), (min._1, max._2, min._3), tinyBoxMaterial),

    //   // Left face
    //   Quad((min._1, min._2, min._3), (min._1, min._2, max._3), (min._1, max._2, max._3), (min._1, max._2, min._3), tinyBoxMaterial),

    //   // Right face
    //   Quad((max._1, min._2, min._3), (max._1, min._2, max._3), (max._1, max._2, max._3), (max._1, max._2, min._3), tinyBoxMaterial)
    // )

    val tinyBox = List()

    val scene = AnimatedScene(
      // shapes = generateTerrain(50, 50, 0.1f, 5),      
      // shapes = staticShapes ::: tinyBox,
      shapes = staticShapes ::: generateTerrain(4, 4, 1f, 1),
      camera = Camera(position = (2f, 0f, smooth(from = -1f, to = 5f, 5.seconds))),
      duration = 5.seconds
    )

    val parameters = AnimationRtRenderer.Parameters(
      width = 640,
      height = 360,
      superFar = 300f,
      pixelIterations = 500,
      iterations = 1,
      bgColor = hex("#000000"),
      framesPerSecond = 1
    )
    val renderer = AnimationRtRenderer(parameters)
    renderer.renderFramesToDir(scene, Paths.get("output"))

// Renderable with ffmpeg -framerate 30 -pattern_type sequence -start_number 01 -i frame%02d.png -s:v 1920x1080 -c:v libx264 -crf 17 -pix_fmt yuv420p output.mp4

// ffmpeg -t 3 -i output.mp4 -vf "fps=30,scale=720:-1:flags=lanczos,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" -loop 0 output.gif
