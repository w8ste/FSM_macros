package sm

import  scala.quoted.*

object StateMachine:
    inline def state_machine(inline body: Unit): Unit =
      ${ stateMaschineImpl('body) }

    private def stateMaschineImpl(bodyExprs: Expr[Unit])(using Quotes): Expr[Unit] = 
      import quotes.reflect.*

      val tree = bodyExprs.asTerm

      val statements = tree match
        case Inlined(_, _, Block(stats, e)) =>  {
          stats :+ e
        }
        case Inlined(_, _, stat)            => List(stat)
        case Block(stats, _)                => stats
        case _                              => List(tree)
    
      val states = scala.collection.mutable.ListBuffer[String]()
      var init_state: Option[String] = None
    
      statements.foreach {
        case Inlined(Some(Apply(Ident("state"), List(Literal(StringConstant(name))))), _, _) =>
          states += name
    
        case Inlined(Some(Apply(Ident("initialState"), List(Literal(StringConstant(name))))), _, _) =>
          if init_state.isDefined then
            report.error(s"Duplicate initial state: $name")
          init_state = Some(name)
    
        case Apply(Ident("state"), List(Literal(StringConstant(name)))) =>
          states += name
    
        case Apply(Ident("initialState"), List(Literal(StringConstant(name)))) =>
          init_state = Some(name)
          if init_state.isDefined then
            report.error(s"Duplicate initial state: $name")

        case stmt =>
          report.warning(s"[macro] Unknown statement: ${stmt.show(using Printer.TreeStructure)}")
      }
    
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
    

    inline def state(name: String): Unit = ()
    inline def initialState(name: String): Unit = ()
