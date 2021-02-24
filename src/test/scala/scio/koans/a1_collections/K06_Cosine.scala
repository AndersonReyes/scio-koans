package scio.koans.a1_collections

import org.openjdk.jmh.annotations._
import scio.koans.shared._

/**
 * Compute the cosine similarity between 2 vectors.
 */
class K06_Cosine extends JmhKoan {

  val vec1: Array[Double] = (1 to 100).map(_.toDouble / 100).toArray
  val vec2: Array[Double] = (-100 to -1).map(_.toDouble / 100).toArray

  // cosine(v1, v2) = dot_product(v1, v2) / (magnitude(v1) * magnitude(v2))
  // dot_product(v1, v2) = sum(v1[i] * v2[i] for i in [0, |v1|])
  // magnitude(v) = sqrt(sum(v[i]^2 for i in [0, |v|]))
  @Benchmark def baseline: Double = {
    val dotProd = (vec1 zip vec2).map(t => t._1 * t._2).sum
    val mag1 = math.sqrt(vec1.map(math.pow(_, 2)).sum)
    val mag2 = math.sqrt(vec2.map(math.pow(_, 2)).sum)
    dotProd / (mag1 * mag2)
  }

  @Benchmark def v1: Double = {
    var mag1 = 0.0
    var mag2 = 0.0
    var dot = 0.0

    var i = 0
    while (i < vec1.size) {
      dot += vec1(i) * vec2(i)
      mag1 += vec1(i) * vec1(i)
      mag2 += vec2(i) * vec2(i)
      i += 1
    }
    return dot / (math.sqrt(mag1) * math.sqrt(mag2))
  }

  @Benchmark def v2: Double = {
    var mag1 = 0.0
    var mag2 = 0.0
    var dot = 0.0

    (vec1.iterator zip vec2.iterator).foreach( { case(x, y) => {
      dot += x * y
      mag1 += x * x
      mag2 += y * y
    }})
    return dot / (math.sqrt(mag1) * math.sqrt(mag2))
  }

  verifyResults()
  verifySpeedup(Speedup.Times(5))
}
