
import org.scalacheck._
import Prop.forAll
    
// Use the Properties trait to group several related properties.  You can run it directly.    
object StringSpecification extends Properties("String") { 
  
  // I'm not sure how this works, but property({name}) lets you name each property, although you don't seem to 
  // get a Scala variable to represent it.  Each property will be called String.{name}.    
  property("startsWith") = forAll { (a: String, b: String) => (a+b).startsWith(a) }
  
  property("endsWith") = forAll { (a: String, b: String) => (a+b).endsWith(b) }  
  
  property("substring") = forAll { (a: String, b: String) => (a+b).substring(a.length) == b }
  
  property("substring") = forAll { (a: String, b: String, c: String) => 
    (a+b+c).substring(a.length, a.length+b.length) == b    
  }
  
} // StringSpecification


// You can group separate Properties sets into one using include().
object MyAppSpecification extends Properties("MyApp") { include(StringSpecification); include(StringUtilsProps) }


// We can also run the tests from a separate program.
object TestEverything extends App {
  
  println("In TestEverything...")
 
  // The Properties trait extends Prop, so we can treat it as a single property.
  println("\n\nTesting StringSpecification...")
  StringSpecification.check  
  println("\n\nTesting MyAppSpecification...")
  MyAppSpecification.check
  
} // TestEverything
