package runtime
import problem.Problem
import scala.io.Source


object Main extends App{

  // Parse the input file and load the data
  def loadProblems(inputFile: String): List[Problem] = {
    val lines = Source.fromFile(inputFile).getLines().toList
    var firstLine = true
    var problems: List[Problem] = Nil

    var balance = 0
    var days = 0
    var machines: List[Array[Int]] = Nil

    for(l <- lines) {
      val values = l.split(" ").map(i => i.toInt)
      values.length match {
        case 3 =>
          if (firstLine) { // First line, new problem
            firstLine = false
          } else { // Not first line, need to record the previous problem
            problems = new Problem(balance, days, machines) :: problems
          }
          if (values.sum != 0) { // Not Last line: initialize a new problem
            balance = values(1)
            days = values(2)
            machines = Nil
          }
        case 4 => // "Machine description line" -> Add it to the machine store
          machines = values :: machines
        case _ => // w00t case, ignore.
      }
    }
    problems.reverse // List always prepend, need to reverse to keep the file order.
  }


  val problems = loadProblems(if (args.length > 0) args(0) else "../input.txt")
  var idx = 0
  for (p <- problems) {
    idx += 1
    println("Case "+idx+": "+p.solve())
  }
}