
object Main:
  def main(args: Array[String]): Unit =
    import sm.StateMachine.*
    state_machine {
      state("Red")
      state("Green")
      state("Yellow")
      initialState("Red")
    }
    println("---------------Exit MAIN---------------------")

    
