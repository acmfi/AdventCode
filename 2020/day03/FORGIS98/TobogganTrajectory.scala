import scala.io.Source;

object TobogganTrajectory{
  def main(args: Array[String]): Unit = {

    val myFile = Source.fromFile("input.txt");
    val lines = myFile.getLines().toArray;

    var bigN: BigInt = BigInt(1);

    bigN *= countTrees(lines, 1, 1);
    bigN *= countTrees(lines, 3, 1);
    bigN *= countTrees(lines, 5, 1);
    bigN *= countTrees(lines, 7, 1); 
    bigN *= countTrees(lines, 1, 2);
    print(bigN);

    myFile.close();
  }

  def countTrees(forest: Array[String], right: Int, down: Int): Int = {

    var positionRight: Int = right;
    var positionDown: Int = 0;
    var totalTrees: Int = 0;

    for(positionDown <- down until forest.size by down){
      if(forest(positionDown)(positionRight) == '#')
        totalTrees += 1;

      positionRight += right;
      positionRight %= forest(0).length();
    }

    return totalTrees;
  }
}
