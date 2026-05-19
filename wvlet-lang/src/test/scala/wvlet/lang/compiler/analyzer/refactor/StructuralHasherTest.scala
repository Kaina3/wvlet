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

class StructuralHasherTest extends AirSpec:

  private def createTableRef(name: String): TableRef =
    TableRef(UnquotedIdentifier(name, NoSpan), NoSpan)

  private def createUnquotedId(name: String): UnquotedIdentifier =
    UnquotedIdentifier(name, NoSpan)

  private def createLongLiteral(value: Long): LongLiteral =
    LongLiteral(value, value.toString, NoSpan)

  test("hash identical plans should return same hash"):
    val plan1 = Filter(
      createTableRef("test_table"),
      Eq(createUnquotedId("col1"), createLongLiteral(100), NoSpan),
      NoSpan
    )
    val plan2 = Filter(
      createTableRef("test_table"),
      Eq(createUnquotedId("col1"), createLongLiteral(100), NoSpan),
      NoSpan
    )

    val hash1 = StructuralHasher.hash(plan1)
    val hash2 = StructuralHasher.hash(plan2)
    hash1 shouldBe hash2

  test("hash with ignoreLiterals should ignore literal values"):
    val plan1 = Filter(
      createTableRef("test_table"),
      Eq(createUnquotedId("col1"), createLongLiteral(100), NoSpan),
      NoSpan
    )
    val plan2 = Filter(
      createTableRef("test_table"),
      Eq(createUnquotedId("col1"), createLongLiteral(200), NoSpan), // Different literal
      NoSpan
    )

    val config = HashConfig(ignoreLiterals = true)
    val hash1  = StructuralHasher.hash(plan1, config)
    val hash2  = StructuralHasher.hash(plan2, config)
    hash1 shouldBe hash2

  test("hash without ignoreLiterals should detect different literals"):
    val plan1 = Filter(
      createTableRef("test_table"),
      Eq(createUnquotedId("col1"), createLongLiteral(100), NoSpan),
      NoSpan
    )
    val plan2 = Filter(
      createTableRef("test_table"),
      Eq(createUnquotedId("col1"), createLongLiteral(200), NoSpan),
      NoSpan
    )

    val config = HashConfig(ignoreLiterals = false)
    val hash1  = StructuralHasher.hash(plan1, config)
    val hash2  = StructuralHasher.hash(plan2, config)
    hash1 shouldNotBe hash2

  test("hash different node types should return different hashes"):
    val table = createTableRef("test_table")
    val filter = Filter(
      table,
      Eq(createUnquotedId("col1"), createLongLiteral(100), NoSpan),
      NoSpan
    )
    
    val hash1 = StructuralHasher.hash(filter)
    val hash2 = StructuralHasher.hash(table)
    hash1 shouldNotBe hash2

  test("hash commutative operations with normalization"):
    val expr1 = And(
      Eq(createUnquotedId("a"), createLongLiteral(1), NoSpan),
      Eq(createUnquotedId("b"), createLongLiteral(2), NoSpan),
      NoSpan
    )
    val expr2 = And(
      Eq(createUnquotedId("b"), createLongLiteral(2), NoSpan),
      Eq(createUnquotedId("a"), createLongLiteral(1), NoSpan),
      NoSpan
    )

    val config = HashConfig(normalizeCommutative = true, ignoreLiterals = true, ignoreIdentifiers = true)
    val hash1  = StructuralHasher.hashExpr(expr1, config)
    val hash2  = StructuralHasher.hashExpr(expr2, config)
    hash1 shouldBe hash2

  test("hash join with different join types should differ"):
    val left  = createTableRef("t1")
    val right = createTableRef("t2")
    val cond  = JoinOn(Eq(createUnquotedId("a"), createUnquotedId("b"), NoSpan), NoSpan)

    val join1 = Join(JoinType.InnerJoin, left, right, cond, false, NoSpan)
    val join2 = Join(JoinType.LeftOuterJoin, left, right, cond, false, NoSpan)

    val hash1 = StructuralHasher.hash(join1)
    val hash2 = StructuralHasher.hash(join2)
    hash1 shouldNotBe hash2

end StructuralHasherTest
