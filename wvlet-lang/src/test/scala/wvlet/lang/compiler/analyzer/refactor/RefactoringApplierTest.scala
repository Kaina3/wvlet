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
import wvlet.lang.model.expr.*
import wvlet.lang.model.plan.*

class RefactoringApplierTest extends AirSpec:

  test("ExpressionValueEncoder should encode basic types"):
    // String
    val strExpr = ExpressionValueEncoder.encode("hello")
    strExpr.isInstanceOf[SingleQuoteString] shouldBe true

    // Int
    val intExpr = ExpressionValueEncoder.encode(42)
    intExpr.isInstanceOf[LongLiteral] shouldBe true
    intExpr.asInstanceOf[LongLiteral].value shouldBe 42L

    // Double
    val dblExpr = ExpressionValueEncoder.encode(3.14)
    dblExpr.isInstanceOf[DoubleLiteral] shouldBe true

    // Boolean
    val trueExpr = ExpressionValueEncoder.encode(true)
    trueExpr.isInstanceOf[TrueLiteral] shouldBe true

    val falseExpr = ExpressionValueEncoder.encode(false)
    falseExpr.isInstanceOf[FalseLiteral] shouldBe true

    // Null
    val nullExpr = ExpressionValueEncoder.encode(null)
    nullExpr.isInstanceOf[NullLiteral] shouldBe true

    // Pass-through Expression
    val existingExpr = UnquotedIdentifier("test", NoSpan)
    val passThrough  = ExpressionValueEncoder.encode(existingExpr)
    passThrough shouldBe existingExpr

  test("ExpressionValueEncoder should create FunctionArgs"):
    val bindings   = Map("$P1" -> "value1", "$P2" -> 42)
    val paramOrder = List("$P1", "$P2")
    val args       = ExpressionValueEncoder.toFunctionArgs(bindings, paramOrder)

    args.size shouldBe 2
    args(0).value.isInstanceOf[SingleQuoteString] shouldBe true
    args(1).value.isInstanceOf[LongLiteral] shouldBe true

  test("PathRewriter should replace at empty path"):
    val original    = EmptyRelation(NoSpan)
    val replacement = EmptyRelation(NoSpan)
    val result      = PathRewriter.replaceAtPath(original, Nil, replacement)
    result shouldBe replacement

  test("PathRewriter should replace child at path"):
    // Create a simple Filter(child=EmptyRelation)
    val child    = EmptyRelation(NoSpan)
    val original = Filter(child, TrueLiteral(NoSpan), NoSpan)

    val newChild = EmptyRelation(NoSpan)
    val result   = PathRewriter.replaceAtPath(original, List(0), newChild)

    result.isInstanceOf[Filter] shouldBe true
    result.asInstanceOf[Filter].child shouldBe newChild

  test("ApplyConfig should have sensible defaults"):
    val config = ApplyConfig.default
    config.topK shouldBe 1
    config.useOptimalSuggestions shouldBe true
    config.modelNamePrefix shouldBe "auto"

  test("ApplyResult should compute summaries"):
    val applied = List(
      AppliedRefactoring("model1", null, 3, 100),
      AppliedRefactoring("model2", null, 2, 50)
    )
    val skipped = List(
      SkippedRefactoring("model3", "No params")
    )
    val result = ApplyResult(EmptyRelation(NoSpan), applied, skipped)

    result.hasChanges shouldBe true
    result.totalOccurrencesReplaced shouldBe 5
    result.totalNodeReduction shouldBe 150

end RefactoringApplierTest
