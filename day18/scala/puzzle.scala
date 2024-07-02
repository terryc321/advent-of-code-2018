
import scala.io.Source

object Foo {

  val hgt = 50
  val wid = 50

  def showWithBorder(arr : Array[Array[Char]]) : Unit = {
    // Access and print the elements of the array
    println("Printing elements of 2D array:")
    for (j <- 0 to (hgt + 1)) {
      for (i <- 0 to (wid + 1)) {
        print(arr(i)(j) + " ")
      }
      println("")
    }
  }

  def copyArr (arr : Array[Array[Char]]) : Array[Array[Char]]  = {
    val arr2 : Array[Array[Char]] = create(wid,hgt)
    for (j <- 0 to (hgt + 1)) {
      for (i <- 0 to (wid + 1)) {
        arr2(i)(j) = arr(i)(j)
      }
    }
    return arr2
  }


  def show(arr : Array[Array[Char]]) : Unit = {
    // Access and print the elements of the array
    println("Printing elements of 2D array:")
    for (j <- 1 to hgt) {
      for (i <- 1 to wid) {
        print(arr(i)(j) + " ")
      }
      println("")
    }
  }



  
  def create(w:Int , h:Int) : Array[Array[Char]] = {
    val arr = Array.ofDim[Char](w+2,h+2)
    println(s"( wid = $wid , hgt = $hgt )")
    for (j <- 0 to (hgt + 1)) yield {
      for (i <- 0 to (wid + 1)) yield {
        arr(i)(j) = '?'
      }
    }
    return arr
  }



  def countAcres (arr : Array[Array[Char]] ) : List[Int]  = {
    var nTree = 0;
    var nLumber = 0;
    var nOpen = 0;
    for (j <- 1 to hgt) yield {
      for (i <- 1 to wid) yield {
        val ch = arr(i)(j)
        ch match {
          case '.' => nOpen = nOpen + 1 ;
          case '|' => nTree = nTree + 1 ;
          case '#' => nLumber = nLumber + 1;
        }
      }
    }
    return List (nTree , nLumber , nOpen , nTree * nLumber)
  }


  def inbound (x:Int, y:Int) : Boolean = {
    if (x < 1 || x > wid || y < 1 || y > hgt) return false else true
  }

  def hasTree (arr : Array[Array[Char]]  ,x:Int, y:Int) : Int = {
    if (inbound(x,y) &&  '|' == arr(x)(y) ){
      return 1
    }
    return 0
  }


  def hasLumber (arr : Array[Array[Char]] ,x:Int, y:Int) : Int = {
    if (inbound(x,y) && '#' == arr(x)(y) ){
      return 1
    }
    return 0
  }


  //open acre will become filled with trees
  //if three or more adjacent acres contained trees.
  //Otherwise, nothing happens.
  def transitOpen(arr : Array[Array[Char]] ,
                     arr2 : Array[Array[Char]] ,x : Int, y : Int) : Unit = {
    var nTree : Int = hasTree(arr,x-1,y-1) + hasTree(arr,x,y-1) + hasTree(arr,x+1,y-1)
    nTree += hasTree(arr,x-1,y) +                   + hasTree(arr,x+1,y)
                          //     ^^^^^^^ was BUG 
    nTree += hasTree(arr,x-1,y+1) + hasTree(arr,x,y+1) + hasTree(arr,x+1,y+1)
    if (nTree >= 3){
      // a tree
      arr2(x)(y) = '|'
    }
    else {
      // remains open area
      arr2(x)(y) = '.'
    }
  }

  // An acre containing a lumberyard will remain a lumberyard
  // if it was adjacent to at least one other lumberyard and
  // at least one acre containing trees.
  //   Otherwise, it becomes open.
  def transitLumber(arr : Array[Array[Char]] ,
                arr2 : Array[Array[Char]] , x : Int, y : Int) : Unit = {
    var nTree : Int = hasTree(arr,x-1,y-1) + hasTree(arr,x,y-1) + hasTree(arr,x+1,y-1)
    nTree += hasTree(arr,x-1,y) +                      + hasTree(arr,x+1,y)
                           //////     ^^^^ was bug
    nTree += hasTree(arr,x-1,y+1) + hasTree(arr,x,y+1) + hasTree(arr,x+1,y+1)
    
    var nLumber : Int = hasLumber(arr,x-1,y-1) + hasLumber(arr,x,y-1)
    nLumber += hasLumber(arr,x+1,y-1)
    nLumber += hasLumber(arr,x-1,y) +                   + hasLumber(arr,x+1,y)
    //                                 ^^^^ was bug 
    nLumber += hasLumber(arr,x-1,y+1) + hasLumber(arr,x,y+1) + hasLumber(arr,x+1,y+1)

    if (nLumber >= 1 && nTree >= 1){
      // stay a lumberyard
      arr2(x)(y) = '#'
    }
    else {
      // become open
      arr2(x)(y) = '.'
    }
  }



  // An acre filled with trees will become a lumberyard
  // if three or more adjacent acres were lumberyards.
  //   Otherwise, nothing happens.
  def transitTrees(arr : Array[Array[Char]] ,
                      arr2 : Array[Array[Char]] , x : Int, y : Int) : Unit = {
    var nLumber : Int = hasLumber(arr,x-1,y-1) + hasLumber(arr,x,y-1)
    nLumber += hasLumber(arr,x+1,y-1)
    nLumber += hasLumber(arr,x-1,y) +                    + hasLumber(arr,x+1,y)
    nLumber += hasLumber(arr,x-1,y+1) + hasLumber(arr,x,y+1) + hasLumber(arr,x+1,y+1)
    if (nLumber >= 3){
      // a lumberyard
      arr2(x)(y) = '#'
    }
    else {
      // remain as trees
      arr2(x)(y) = '|'
    }
  }



  // . open area
  // # lumberyard
  // | trees
  def transition (arr : Array[Array[Char]]) : Array[Array[Char]]  = {
    val arr2 = create(wid,hgt);

    for (i <- 1 to hgt) yield {
      for (j <- 1 to wid) yield {
        arr2(i)(j) = arr(i)(j)
      }
    }

    for (i <- 1 to wid) yield {
      for (j <- 1 to hgt) yield {
        // considering arr(i)(j)
        (arr(i)(j)) match {
          case '.' => transitOpen(arr,arr2,i,j)
          case '#' => transitLumber(arr,arr2,i,j)
          case '|' => transitTrees(arr,arr2,i,j)
        }
      }
    }

    return arr2
  }



  @main
  def main(args: String*): Unit = {
    var lines = scala.io.Source.fromFile("input").getLines()
    // determine size of input
    // we already know how big the demo board is 

    // Create a 2D array of integers with 3 rows and 4 columns
    var arr : Array[Array[Char]] = create(wid,hgt)
    
    // Initialize the elements of the array
    var x = 0
    for (y <- 1 to hgt) yield {
      x = 1
      for (ch <- lines.next) yield {
        //println("" + x +"," + y + " ==> " + ch)
        arr(x)(y) = ch
        x = x + 1
      }
    }


    println("Initial State ")
    showWithBorder(arr)
      

    for (i <- 1 to 10) yield {
      var arr2 : Array[Array[Char]] = transition(arr)
      arr = copyArr(arr2)
      println(s"Transition $i  ")
      showWithBorder(arr)
    }

    println("counted acres = " + countAcres(arr))
    println("nTree * nLumber = " + countAcres(arr)(3))
    


    //println(arr(0)(0))
    
  }
}


