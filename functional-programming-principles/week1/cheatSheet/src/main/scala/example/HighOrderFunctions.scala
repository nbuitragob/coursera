package example

object HighOrderFunctions{
  def highOrderFilter (){
	val numSet = (-10 to 10).toSet
	def isEven(i: Int)= i%2 ==0

    println(numSet.filter(i => isEven(i)))
	println(numSet.filter(isEven(_)))
	println(numSet.filter(isEven))

  }

  def highOrderMap () {
    def capitalize(s: String) = s.head.toUpper+ s.tail.toLowerCase

    var list = List("Sanwise", "meriadoc", "peregrin")

    print(list.map(capitalize))
  }

  def main (args: Array[String]) {
    highOrderFilter

    highOrderMap
  }

}
