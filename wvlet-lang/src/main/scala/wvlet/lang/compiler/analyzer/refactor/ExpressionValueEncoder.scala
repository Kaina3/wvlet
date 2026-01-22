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

import wvlet.lang.api.Span.NoSpan
import wvlet.lang.model.expr.*

/**
  * Utility to convert Any values (from Substitution.bindings) back to Wvlet AST Expressions.
  * 
  * This is used when generating ModelScan arguments from the anti-unification substitutions.
  */
object ExpressionValueEncoder:

  /**
    * Convert a binding value to an Expression.
    * 
    * @param value The value from Substitution.bindings (can be String, Number, Boolean, Expression, etc.)
    * @return The corresponding Wvlet Expression
    */
  def encode(value: Any): Expression =
    value match
      // Already an Expression - return as-is
      case e: Expression =>
        e
      
      // String values -> SingleQuoteString
      case s: String =>
        SingleQuoteString(s, NoSpan)
      
      // Numeric values
      case i: Int =>
        LongLiteral(i.toLong, i.toString, NoSpan)
      case l: Long =>
        LongLiteral(l, l.toString, NoSpan)
      case d: Double =>
        DoubleLiteral(d, d.toString, NoSpan)
      case f: Float =>
        DoubleLiteral(f.toDouble, f.toString, NoSpan)
      case bd: BigDecimal =>
        DecimalLiteral(bd.toString(), bd.toString(), NoSpan)
      case bi: BigInt =>
        LongLiteral(bi.toLong, bi.toString, NoSpan)
      
      // Boolean values
      case true =>
        TrueLiteral(NoSpan)
      case false =>
        FalseLiteral(NoSpan)
      
      // Null
      case null =>
        NullLiteral(NoSpan)
      
      // Fallback: convert to string literal
      case other =>
        SingleQuoteString(other.toString, NoSpan)

  /**
    * Convert multiple values to Expressions
    */
  def encodeAll(values: Seq[Any]): Seq[Expression] =
    values.map(encode)

  /**
    * Create a FunctionArg from a binding value
    */
  def toFunctionArg(value: Any): FunctionArg =
    FunctionArg(
      name = None,
      value = encode(value),
      isDistinct = false,
      orderBy = Nil,
      span = NoSpan
    )

  /**
    * Create FunctionArgs from a map of parameter bindings
    * 
    * @param bindings Map from parameter ID to value
    * @param paramOrder List of parameter IDs in the order they should appear
    * @return List of FunctionArg in the specified order
    */
  def toFunctionArgs(bindings: Map[String, Any], paramOrder: List[String]): List[FunctionArg] =
    paramOrder.flatMap { paramId =>
      bindings.get(paramId).map(toFunctionArg)
    }

end ExpressionValueEncoder
