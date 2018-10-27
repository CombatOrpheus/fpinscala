class Trees {

  abstract class huffmanTree{
    def occurrence: Int
    def symbol: Char
  }

  case class occurrenceNode(n: Int, char: Char) extends huffmanTree{
    override def occurrence: Int = n
    override def symbol: Char = char
  }

  case class accumulationNode(leftChild: huffmanTree,
                              rightChild: huffmanTree
                             ) extends huffmanTree {
    override def occurrence: Int = leftChild.occurrence + rightChild.occurrence
    override def symbol: Char = '*'
    def left: huffmanTree = leftChild
    def right: huffmanTree = rightChild
  }

  def buildOccurrenceNode(n: Int, char: Char): occurrenceNode = new occurrenceNode(n, char)

  def buildAccumulationNode(left: huffmanTree,
                            right: huffmanTree
                           ): accumulationNode = new accumulationNode(left, right)

  def buildOccurrenceList(list: List[(Int, Char)]): List[occurrenceNode] = {
    def apply[A, B, C](f: (A, B) => C,  pair: (A, B)): C = f(pair._1, pair._2)
    list.map(x => apply(buildOccurrenceNode, x))
  }

  def buildHuffTree(list: List[huffmanTree]): List[huffmanTree] = {
    def compareNodes(node1: huffmanTree,
                     node2: huffmanTree
                    ): Boolean = node1.occurrence < node2.occurrence

    val sortedList = list.sortWith(compareNodes)
    sortedList match {
      case head :: next :: _ => buildHuffTree(buildAccumulationNode(head, next) :: sortedList.drop(2))
      case head :: _ => List(head)
      case Nil => Nil
      }
    }

  def decodeHuffTree(list: List[Char], tree: huffmanTree): List[Char] = {
    def aux_decode(list: List[Char], tree: huffmanTree): (Char, List[Char]) ={
      (list, tree) match{
        case (Nil, _) => (' ', Nil)
        case (head :: tail, tree: accumulationNode) =>{
          if (head == '0') aux_decode(tail, tree.left)
          else aux_decode(tail, tree.right)
        }
        case (_ :: _, tree: occurrenceNode) => (tree.symbol, list)
      }
    }
    val decodedChar = aux_decode(list, tree)
    if (decodedChar._2 == Nil) decodedChar._1 :: Nil
    else decodedChar._1 :: decodeHuffTree(decodedChar._2, tree)
  }

  def compactar[A](list: List[A]): List[(Int, A)] ={
    def compat[B](n: Int, list: List[B]): (Int, B) = {
      list match {
        case head :: next :: tail => {
          if (head == next) compat(n+1, tail)
          else (n+1, head)
        }
        case head :: _ => (n+1, head)
      }
    }

    list match {
      case Nil => Nil
      case list => {
        val pair = compat(0, list)
        pair :: compactar(list.drop(pair._1))
      }
    }
  }
}