
import org.scalacheck.Prop.{forAll, BooleanOperators, all}
import org.scalacheck.Properties

object Labels extends Properties("LabelTest") {
  
  // Define a function
  def f(m: Int, n: Int) = m + n  
  
  property("ComplexCondition") = forAll { (m: Int, n: Int) =>
  
    // Test a function that doesn't satisfy the conditions we're testing
    val result = f(m, n)
  
    // Give labels to each condition that must be satisfied with ":|".
    // You can also prepend the label with "|:", but you have to wrap it in parentheses.
    (result >= m) :| "Result must be greater than m." &&
    (result >= n) :| "Result must be greater than n." &&    
    ( "Result must be less than the argument sum." |: (result < m + n) ) 
    
  } // "ComplexCondition"
  
  
  // Instead of using && for each condition, we can use a comma-separated list within all(). 
  property("Multiplication") = forAll { (m: Int, n: Int) =>

    val result = m * n
    
    s"result = $result" |: all(       
        "div1" |: m != 0 ==> (result / m == n),       
        "div2" |: n != 0 ==> (result / n == m),
        "lt1"  |: result > m,
        "lt2"  |: result > n
      )
      
  } // "Multiplication"
  
} // ComplexProperty