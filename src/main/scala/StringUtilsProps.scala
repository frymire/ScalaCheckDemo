
import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, throws, BooleanOperators, AnyOperators}
import org.scalacheck.Gen.{listOf, alphaStr, numChar}

// NOTE: The Properties trait extends App (presumably), so this runs normally.
object StringUtilsProps extends Properties("StringUtils") {
  
  // This property will fail for negative values of n
  property("truncate") = forAll { (s: String, n: Int) =>    
    val t = StringUtils.truncate(s, n)
    (s.length <= n && t == s) || (s.length > n && t == s.take(n) + "...")        
  }  

  // This property will succeed, because it detects that the correct error is thrown for negative values of n
  property("truncateWithError") = forAll { (s: String, n: Int) =>
    
    // Declare this lazily so that an exception is not thrown during assignment.
    lazy val t = StringUtils.truncate(s, n)
    
    // Check that it throw an error is we pass in an illegal negative value for n.  The classOf operator is 
    // built into Scala and used for retrieving the java.lang.Class instance for a particular type.
    if (n < 0) throws(classOf[StringIndexOutOfBoundsException]) { t }
    else (s.length <= n && t == s) || (s.length > n && t == s.take(n) + "...")
    
  } // truncateWithError
  

  // This property will succeed, because it detects that the correct error is thrown for negative values of n
  property("truncateWithPrecondition") = forAll { (s: String, n: Int) =>
    
    // Use an implication operator to keep ScalaCheck from testing values that don't fulfill a condition.
    // Note that this approach will allow runtime errors of the class for illegal values, since they are
    // simply ignored here.
    (n >= 0) ==> {
      val t = StringUtils.truncate(s, n)    
      (s.length <= n && t == s) || (s.length > n && t == s.take(n) + "...") 
    }
    
  } // truncateWithPrecondition
  
  
  // This one assumes we correct the original truncate method
  property("truncateCorrected") = forAll { (s: String, n: Int) => 
  
    // We expect that this shouldn't throw an error, so we leave it "in the open" 
    // and let it fail if it does throw an error for some reason.      
    val t = StringUtils.truncateCorrected(s, n)    
    
    // Now we have to check that the new version works correctly when a negative n is passed.
    if (n < 0) t == ""
    else (s.length <= n && t == s) || (s.length > n && t == s.take(n) + "...") 
    
  } // truncateCorrected
  

  // Explicitly specify the value generators.  Also, we use ?= from Prop.AnyOperators 
  // to get labels when this property fails.
  property("tokenize") = forAll(listOf(alphaStr), numChar) { (ts, d) =>  
    val str = ts.mkString(d.toString)
    StringUtils.tokenize(str, d).toList ?= ts    
  }
  
  
  // This one is fine.
  property("contains") = forAll { (s1: String, s2: String, s3: String) => StringUtils.contains(s1 + s2 + s3, s2) }
  
  
} // StringUtilsProps
