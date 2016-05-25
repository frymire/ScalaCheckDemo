
// Apparently need this to use the '{...} check' syntax 
import language.postfixOps

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.{forAll, classify, collect}


sealed abstract class Tree
case class Node(left: Tree, right: Tree, v: Int) extends Tree
case object Leaf extends Tree


object Generators extends App {
  
  // Use arbitrary[T]() to generate arbitrary values of a given type.  (Note that
  // it is possible to define arbitrary for custom types.)  
  val ints = arbitrary[Int]
  
  // By using a for-comprehension, you don't have to explicitly generate and then get the optional values.  
  val squares = for ( x <- Gen.choose(0, 12) ) yield { x * x }  

  // Use listOf to make a sample with a specific number of outcomes of a given generator.
  println( Gen.listOfN(10, ints).sample )
  println( Gen.listOfN(10, squares).sample )

    
  // Here's a specialized generator of integer pairs
  val myGen = for ( m <- Gen.choose(10, 20); n <- Gen.choose(2 * m, 500) ) yield (m, n)
  println( Gen.listOfN(10, myGen).sample )
  
  // More generally, you can generate containers with values populated by another generator.
  // By default, ScalaCheck supports List, Stream, Set, Array, and java.util.ArrayList.
  println( Gen.containerOf[List,Int](Gen.oneOf(1, 3, 5)).sample ) 
  println( Gen.containerOfN[List,Int](10, Gen.oneOf(1, 3, 5)).sample )
  println( Gen.containerOf[Stream,String](Gen.alphaStr).sample )
  println( Gen.containerOf[Array,Boolean](true).sample )
  
  // Use suchThat() to generate values conditionally. 
  val smallEvenInteger = Gen.choose(0, 200) suchThat { _ % 2 == 0 }
  println
  for (i <- 0 to 10; v <- smallEvenInteger.sample) print(v + " ")
  println
  
  
  val vowel = Gen.oneOf('A', 'E', 'I', 'O', 'U', 'Y')  
  println
  println( Gen.listOfN(50, vowel).sample )
  
  val weightedVowel = Gen.frequency( (30, 'A'), (40, 'E'), (2, 'I'), (3, 'O'), (1, 'U'), (1, 'Y') )
  println
  println( Gen.listOfN(50, weightedVowel).sample )
  
  
  // Now we'll generate case classes... 
  
//  // Define Leaf and Node generators.  It's not obvious why Scala thinks genNode is a Gen[Node] as   
//  // opposed to a Node.  Presumably ScalaCheck overrides the for structure to automatically do that  
//  // when the values of the for-comprehension are generated?
//  val genLeaf = Gen.value(Leaf) 
//  val genNode = for (v <- arbitrary[Int]; left <- genTree; right <- genTree) yield Node(left, right, v)
//
//  // Define a function to generate a tree.  If genNode is selected, genNode and genTree form a recursion.
//  def genTree: Gen[Tree] = Gen.oneOf(genLeaf, genNode)
//
//  // Use genTree as a generator to generate Trees
//  println
//  println(genTree.sample)
  
  
  // Define a function to determine whether a list is ordered.  
  def ordered(l: List[Int]) = { l == ( l sortBy { x: Int => x } ) }
  
  // Check a bunch of randomly generated lists, and accumulate statistics about their size, 
  // and whether they are ordered.  Here, we probably won't get any large ordered lists, because
  // they're hard to generate randomly.  This might tell us that we need a special generator
  // to build these cases.
  forAll { l: List[Int] =>   
    
    classify(ordered(l), "ordered" /*, "unordered"*/) {    
      classify(l.length > 5, "large", "small") { l.reverse.reverse == l }      
    }
    
  } check
  
  
  // Use collect to summarize the distribution of values generated.
  forAll( Gen.choose(1,10) ) { n => collect(n) { n == n } } check  
    
  
} // Generators
