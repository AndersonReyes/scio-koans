package scio.koans.a3_serde

import scio.koans.shared._

/**
 * Fix the `NotSerializableException`.
 */
class K07_WordCount4 extends PipelineKoan {

  val input: Seq[String] = Seq("a b c", "b c d", "c d e")

  "Snippet" should "work" in {
    runWithContext { sc =>
      val output = sc
        .parallelize(input)
        .flatMap(_.split("[^a-zA-Z']+").filter(_.nonEmpty))
        .countByValue

      // Hint: `_.toSet == expect` is a lambda that pulls in `expected`
      import K07_WordCount4._
      output should satisfy[(String, Long)](_.toSet == expected)
    }
  }
}

object K07_WordCount4 {
  val expected: Set[(String, Long)] = Set(("a", 1), ("b", 2), ("c", 3), ("d", 2), ("e", 1))
}
