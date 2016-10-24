

import scala.language.postfixOps

import org.scalacheck.Prop.{forAll, BooleanOperators}
import org.scalacheck.Gen

object SimpleTest extends App {
  
  // You can just check properties without naming them like this.
  print("List size...")
  forAll { (l1: List[Int], l2: List[Int]) => l1.size + l2.size == (l1 ::: l2).size } check
  
  // Or, you can assign them to a variable like this.
  print("\nSquare root...")
  val sqrtProp = forAll { (n: Int) => scala.math.sqrt(n*n) == n };  
  sqrtProp.check 
 
  // Define and use your own generator
  print("\nSmall integers...")
  val smallInteger = Gen.choose(0,100)
  val smallProp = forAll(smallInteger) { n => (n >= 0) && (n <= 100) }
  smallProp.check
  
  // Be careful with implications, because they might be hard to fulfill.  Here, ScalaCheck will generate
  // a lot of integers, but only zeros satisfy the condition to be checked anyway.
  print("\nImplications...")
  forAll { (n: Int) => (n == 0) ==> (n == 0) } check
  
  // You can also combine properties like this...
  print("\nProperty combinations...")
  print("\n--Square root and small integers...")
  (sqrtProp && smallProp).check // or all(sqrtProp, smallProp)
  print("\n--Square root or small integers...")
  (sqrtProp || smallProp).check // or atLeastOne(sqrtProp, smallProp)
  print("\n--Square root equals small integers...")
  (sqrtProp == smallProp).check
  
}