import scala.quoted.*

object UppercaseMacro:

  inline def toUpperCase(inline str: String): String = 
    ${ toUpperCaseImpl('str) }

  private def toUpperCaseImpl(strExpr: Expr[String])(using Quotes): Expr[String] =
    strExpr.value match
      case Some(value) => Expr(value.toUpperCase)
      case None => 
        quotes.reflect.report.error("Expected a constant string")
        '{ "" }
