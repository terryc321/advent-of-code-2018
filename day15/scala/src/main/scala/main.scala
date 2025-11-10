
@main
def main(): Unit = {
  val matrix = Array.ofDim[Int](2, 3)
//  val matrix = Array.tabulate(2,5){ case (x,y) => x + 10*y}
 matrix(0)(0) <- 1
 println(s" 0:0 = ($matrix(0)(0)) ")

  /*
  for (rowIndex <- 0 to 1; colIndex <- 0 until 5) {
    print(s"$rowIndex, $colIndex")

    // Accessing the elements
    println(s" = ${matrix(rowIndex)(colIndex)}")
  }

  for (i <- 1 to 5) {
    //TIP Press <shortcut actionId="Debug"/> to start debugging your code. We have set one <icon src="AllIcons.Debugger.Db_set_breakpoint"/> breakpoint
    // for you, but you can always add more by pressing <shortcut actionId="ToggleLineBreakpoint"/>.
    println(s"i = $i")
  }
  */

}

