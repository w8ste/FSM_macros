package sm

import  scala.quoted.*

object StateMachine:
    inline def state_machine(inline body: Unit): Unit =
      ${ stateMaschineImpl('body) }

    private def stateMaschineImpl(bodyExprs: Expr[Unit])(using Quotes): Expr[Unit] = 
      import quotes.reflect.*

      val tree = bodyExprs.asTerm

      println(tree.show(using Printer.TreeStructure))
    
      val statements = tree match
        case Inlined(_, _, Block(stats, _)) => stats
        case Inlined(_, _, stat)            => List(stat)
        case Block(stats, _)                => stats
        case _                              => List(tree)

      statements.foreach { stmt =>
        println(stmt.show(using Printer.TreeStructure))  // For structural tree

}
    
      val states = scala.collection.mutable.ListBuffer[String]()
      var init_state: Option[String] = None
    
      statements.foreach {
        case Inlined(Some(Apply(Ident("state"), List(Literal(StringConstant(name))))), _, _) =>
          states += name
    
        case Inlined(Some(Apply(Ident("initialState"), List(Literal(StringConstant(name))))), _, _) =>
          init_state = Some(name)
          println("hello")
    
        case Apply(Ident("state"), List(Literal(StringConstant(name)))) =>
          states += name
    
        case Apply(Ident("initialState"), List(Literal(StringConstant(name)))) =>
          init_state = Some(name)
          println("hello")    
        case stmt =>
          report.warning(s"[macro] Unknown statement: ${stmt.show(using Printer.TreeStructure)}")
      }
    
      // Convert results into Exprs
      val stateExprs = states.map(Expr(_)).toList
      val allStatesList = Expr.ofList(stateExprs)
      val initialExpr = Expr(init_state.getOrElse(""))
    
      // Runtime code emitted
      '{
        val allStates = $allStatesList
        val initial = $initialExpr
        println("StateMachine initialized with states: " + allStates.mkString(", "))
        println("Initial state: " + initial)
      }
    


  // These are only used at compile-time parsing
    inline def state(name: String): Unit = ()
    inline def initialState(name: String): Unit = ()
