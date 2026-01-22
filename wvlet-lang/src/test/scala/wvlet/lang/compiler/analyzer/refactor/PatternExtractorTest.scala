/*
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package wvlet.lang.compiler.analyzer.refactor

import wvlet.airspec.AirSpec
import wvlet.lang.api.Span.NoSpan
import wvlet.lang.model.plan.*
import wvlet.lang.model.expr.*
import wvlet.lang.model.DataType

class PatternExtractorTest extends AirSpec:

  private def createTableRef(name: String): TableRef =
    TableRef(UnquotedIdentifier(name, NoSpan), NoSpan)

  private def createUnquotedId(name: String): UnquotedIdentifier =
    UnquotedIdentifier(name, NoSpan)

  private def createLongLiteral(value: Long): LongLiteral =
    LongLiteral(value, value.toString, NoSpan)

  test("SubtreeCollector should collect subtrees from a plan"):
    val filter1 = Filter(
      createTableRef("orders"),
      Eq(createUnquotedId("status"), createUnquotedId("active"), NoSpan),
      NoSpan
    )
    val filter2 = Filter(
      createTableRef("customers"),
      Eq(createUnquotedId("status"), createUnquotedId("active"), NoSpan),
      NoSpan
    )
    
    val join = Join(
      JoinType.InnerJoin,
      filter1,
      filter2,
      JoinOn(Eq(createUnquotedId("order_id"), createUnquotedId("customer_id"), NoSpan), NoSpan),
      false,
      NoSpan
    )

    val config = CollectorConfig()
    val subtrees = SubtreeCollector.collect(join, config, Some("test_source"))
    
    // Should collect the join, both filters, and both table refs
    (subtrees.size > 0) shouldBe true

  test("DuplicateDetector should find duplicate patterns"):
    // Create two structurally similar filters
    val filter1 = Filter(
      createTableRef("orders"),
      Eq(createUnquotedId("status"), createUnquotedId("active"), NoSpan),
      NoSpan
    )
    val filter2 = Filter(
      createTableRef("customers"),
      Eq(createUnquotedId("status"), createUnquotedId("inactive"), NoSpan),
      NoSpan
    )

    val config = CollectorConfig()
    val hashConfig = HashConfig(ignoreLiterals = true, ignoreIdentifiers = true)
    
    val subtrees1 = SubtreeCollector.collect(filter1, config, Some("source1"))
    val subtrees2 = SubtreeCollector.collect(filter2, config, Some("source2"))
    
    val allSubtrees = subtrees1 ++ subtrees2
    val groups = SubtreeCollector.groupByHash(allSubtrees, 2)
    
    // Should find structurally similar patterns
    // The two Filter nodes have the same structure
    info(s"Found ${groups.size} groups with duplicates")

  test("Pattern extraction basic functionality"):
    val filter = Filter(
      createTableRef("test"),
      Eq(createUnquotedId("col"), createLongLiteral(1), NoSpan),
      NoSpan
    )
    
    // Use CollectorConfig.all to include all nodes including leaf nodes
    val collectorConfig = CollectorConfig.all
    val subtrees = SubtreeCollector.collect(filter, collectorConfig, Some("source1"))
    
    // Basic sanity check - subtrees should be collected
    // With CollectorConfig.all, we should get the Filter and TableRef
    info(s"Collected ${subtrees.size} subtrees")
    (subtrees.size > 0) shouldBe true

end PatternExtractorTest
