package name.rayrobdod.collectionRichVarargs

import scala.collection.mutable.Builder
import scala.quoted.*

/**
 * @example
 * ```
 * RichVarargsList("a", if (true) "b", if (false) "c") == List("a", "b")
 * ```
 */
inline def RichVarargsList[A](inline elems: (A | Unit)*): List[A] =
	richVarargs(List.newBuilder, elems*)

private inline def richVarargs[A, To](builder: Builder[A, To], inline elems: (A | Unit)*): To =
	${richVarargsImpl('builder, 'elems)}

def richVarargsImpl[A, To](builder: Expr[Builder[A, To]], elems: Expr[Seq[(A | Unit)]])(using Quotes, Type[A], Type[To]): Expr[To] =
	import quotes.reflect.report.errorAndAbort
	val addends: List[Expr[Builder[A, To]] => Expr[_]] = elems match
		case Varargs(elems2) =>
			elems2.toList.map: elem =>
				elem match
					case '{if ($condition) ($value: A)} =>
						(b: Expr[Builder[A, To]]) => '{if ($condition) {$b.addOne($value)}}
					case '{$value: A} =>
						(b: Expr[Builder[A, To]]) => '{$b.addOne($value)}
					case _ =>
						errorAndAbort(s"Unknown tree", elem)
		case _ =>
			errorAndAbort(s"Expected arguments to be in varargs form", elems)

	import quotes.reflect.*
	val retval =
		ValDef.let(Symbol.spliceOwner, "builder$", builder.asTerm): builderRef =>
			Block(
				addends.map(addend => addend(builderRef.asExprOf[Builder[A, To]]).asTerm),
				Apply(Select.unique(builderRef, "result"), Nil),
			)
	//println(retval.asExpr.show)
	retval.asExprOf[To]
