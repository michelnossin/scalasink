
//Package object is a package with including methods/variables that can be used package wide. Used for aliases for example.
package object michelAlias {
  def geefNaam = println("Ik ben Michel")

}

package com.nossin.Rational {

//Using the Ordered Trait icw the compare method all the < > <= >= operators etc do not have to be defined. They are already in that trait
// Lets build the Rational class, with a number n, and denominator d.
class Rational(n: Int, d: Int) extends Ordered[Rational] {
  //Denominator (passed parameter d) may not be 0 because dividing by 0 is not allowed)
  require(d != 0)

  //private variable means only use this value within Rational class methods, not instances of Rational. So val x = someratobj.prive will not work.
  // Note companion objects can see this value also.
  private val prive = 0 //You don't see types even though Scala is statically typed. This is because of type inference. It knows this is an Int type.
  // At compile time it checks code to see if this value is used properly

  //This class and subclasses van use this value
  protected val prot = 0

  //public
  val pub = 0 //val x = someobj.pub this will work

  //Qualified access modifiers
  private[Rational] val prive2 = 0 //Means it is private in class Rational
  //class's , objects, methods, values etc can be protected by giving their names in [x]

  //The only properties we will use are the number and denominator given in the constructor:
  val numer = n
  val denom = d

  //Auxiliary constructor which allows our objects to be made without denominator (e.g. 2/1 is the same as just 2)
  def this(n: Int) = this(n, 1)

  //Lets define the methods on Rational objects
  def add(that: Rational): Rational =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def addInt(i: Int): Rational = new Rational(numer + i * denom, denom)

  def multiply(that: Rational): Rational = new Rational(numer * that.numer, denom * that.denom)

  def isSmaller(i: Int): Boolean = if ((numer / denom) < i) true else false

  def isGreater(i: Int): Boolean = if ((numer / denom) > i) true else false

  def isEqual(i: Int): Boolean = if ((numer / denom) == i) true else false

  def isEqual(i: Rational): Boolean = if ((numer / denom) == (i.numer / i.denom)) true else false

  //Has to be implemented because we use the Ordered trait. This also means the is... methods mentioned are now not needed anymore
  def compare(that: Rational) = {
    (this.numer * that.denom) - (that.numer * this.denom)
  }

  def toDouble: Double = {
    //The requires statement of Rational prevents denom to be 0 .
    //To check something within this object use assert
    assert(denom > 0) //Will throw exception if not the case

    numer / denom
  }

  //You could also use ensuring keyword (at the bottom) to replace the assert keyword a complete section/method
  def toLong: Long = {
    (numer/denom).toLong
  }
  //ensuring(true)
//Alternatively of assert use at end of function after } :  ensuring (<some bool expression>)

  //Operator methods, just redirect to the earlier defined methods.
  def +(that: Rational): Rational = add(that)

  def +(that: Int): Rational = addInt(that)

  //overloading
  def *(that: Rational): Rational = multiply(that)

  def <(i: Int): Boolean = isSmaller(i)

  def >(i: Int): Boolean = isGreater(i)

  def ==(i: Int): Boolean = isEqual(i)

  def ==(i: Rational): Boolean = isEqual(i)

  def unary_- : Rational = new Rational(numer * -1, denom) //Unary operator allows methods to be in front of an object (so -x will make 3/4 to -3/4)

  override def toString = numer + "/" + denom //Must have to change toString to see the value in interpreter


}

//Companion object. When a companion object  is defined with the same name as the class (with apply implemented method) , you can create an instance of the class without using new keyword .
object Rational {
  //def unapply(that : Rational) : Boolean = {
  //  if (that.denom != 0) Some(that.numer / that.denom) else None
  //}
  //This make sure you do not need to NEW Rational objects
  def apply(n: Int, d: Int) = {
    new Rational(n, d)
  }

  def apply(n: Int) = {
    new Rational(n)
  }
}

}
