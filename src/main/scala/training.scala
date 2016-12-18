import java.awt.Button
import java.awt.event.{ActionListener, ActionEvent}
import java.io.{PrintWriter, FileNotFoundException, FileReader}
import java.net.ServerSocket
import java.util.Date
import javax.swing.JButton

import akka.actor.Actor

import scala.swing.event.ButtonClicked
import scala.swing.{Label, FlowPanel, MainFrame, SimpleSwingApplication}
import scala.util.matching.Regex


// blabla.{x ==> _,_}  means to exclude x but include the rest
import com.nossin.Rational.{Rational => pietje}
import com.nossin.Rational.Rational
import michelAlias.geefNaam //Using package object you can import methods/values etc per object. USed for package wide variables

import scala.collection.mutable.ArrayBuffer


/**
 * Created by Gebruiker on 17-11-2015.
 */

object training {

  def main (args: Array[String]) {

    val x2 = geefNaam //Call to method in package object

    //implicit conversion, allowed other types to work with Rational even if these classes can not be changed!
    // In this case when finding a Int object it will be converted to a Rational object using the Integer is parameter
    implicit def intToRational(i: Int): Rational = Rational(i)

    //Creating objects bij calling the apply method in the companion object named Rational. These are factory methods, creating instance of the Rational class using the main
    //or auxiliary constructors. The main constructor uses the class parameters only.
    val piet = pietje(3,4) //See import statement, pietje is an alias for Rational class
    val x = Rational(3, 4)
    val y = Rational(5, 7)
    val r = Rational(2)

    //This comparison uses the Rational class because the Ordered trait it implements (icw compare method) all kinds of comparison operators are given.
    val xBiggerEqualThenY = if (x >= y) true else false

     2 + r //This will requires implicit conversion functions because 2 is typed Int, the implicit conversion function (intToRational)will convert it to our Rational object
    r + 2  //This will just use the + method in Rational, which uses the add method.
    val z = -(x * y + 2 + y)
    val isLargerThenMin10 = if (z > -10) true else false //This if statement gives a result, always catch the result. Working with functional style programming tt prevents the use of variables.

    //Using for loop. This example uses filter (iterator guards, variables within to store intermediate results, and nested loops.
    //Yield keyword will make sure the result is saved, Always svae the result, it's good functional best practise.
    def ratList = for {i <- 1 to 10
         if Rational(5) > i
         if i != 3
         z <- 1 to 10
      x = Rational(i,z)
    } yield x

    //Using first class functions: A first class function takes and or returns another function!
    ratList.foreach(x=> println(x.numer + "/" + x.denom)) //x can be written as (x: Rational) but Scala knows it  so can be excluded
    val fRatList = ratList.filter(_ > 1) //Because Scala knows all objects are Rational you can just use _ placeholder wildcard. Note, > is using the > method within Rational class
    fRatList.foreach(x=> println(x.numer + " is the numer")) //x can be written as (x: Rational) but target typing within Scala allready knows that so can be excluded

    //Some sum function on an array of Rational objects
    def sumRatList(x: IndexedSeq[Rational]): Double = {
      var sum : Double = 0
       x.foreach(y => sum += y.toDouble)
      sum
    }
    val som = sumRatList(fRatList)

    //Recursive functions: While loops and mutable variables (var) are not recommended because they are mutable: use recursive methods whenever possible to make this a more functional function
    def sumRatListRecur(x: IndexedSeq[Rational]) : Double = {
      def sumRatRational(lst : IndexedSeq[Rational], sum : Double): Double = {
        if (lst.size == 0) //Return if its the last elem.
          sum
        else
          sumRatRational(lst.tail,lst(0).toDouble + sum) //now go to the next value and pass the temp sum
      }
      sumRatRational(x, 0.0) //test it
    }
    val ratje = IndexedSeq[Rational](Rational(3,1),Rational(4,2))
    val somRecur = sumRatListRecur(ratje) //returns 5.0

    //partially applied function, let's explain them.
    //But first some normal function will be defined.
    def sumRatDouble(r1 : Rational , r2: Rational) : Double = {
      r1.toDouble + r2.toDouble
    }
    def sumRatRational(r1 : Rational , r2: Rational) : Rational = {
      r1 + r2
    }
    val sumRat3 = sumRatRational(Rational(3,1),(_ : Rational)) //This is now a partially applied function . Notice the type after _ is necessary otherwise it expects the complete parameter list
    val sumRat4 = sumRatRational(Rational(4,1),(_ : Rational))
    val resultSum3 = sumRat3(Rational(5,2)) //You only need the 2nd parameter
    var resultSum4 = sumRat4(Rational(5,2)) //You only need the 2nd parameter

    //Named parameters in function calls
    val someSum = sumRatRational(r2 = Rational(2,6),r1 = Rational(7,2)) //You can now switch parameters around because you use the names.

    //default parameters can be used, so these will not be required anymore when using a method.
    def sumDefaultDouble(r1: Rational = Rational(1,1),r2: Rational) : Double = {
      r1.toDouble + r2.toDouble
    }
    val sumDef = sumDefaultDouble(r2 = Rational(2,1)) //r1 not needed. The default valie 1 is added
    val sumDef2 = sumDefaultDouble(Rational(1,2), Rational(2,1)) //this also works, however 0.5 + 2 should not be 2.0!!

    //closure objects
    var sumRat5 = sumRatRational(resultSum4,Rational(5,3)) //We are using resultSum4 which is a free variable and not a bound variable.
    // The function will close the value so sumRat5's object is the only closure

    //Lets show what we mean:
    resultSum4 //Stil the same value
    //sumRat5 will see when resultSum4 changes
    resultSum4 += 1
    sumRat5
    sumRat5 = sumRatRational(resultSum4,Rational(5,3)) //sumRat 5 is different because resultSum4 changed.

    //repeated parameters use <type>* with * notation, meaning any number of paramaters can get passed
    def repParam(str: String*) = {
      str.foreach(println)
    }
    repParam("hallo")
    repParam("Hallo","Michel")
    val arrStr = Array("hallo","michel")
    //repParam(arrStr) //NOT ALLOWED
    repParam(arrStr : _*) //This tells scala to send each entry of the array seperately

    //or doing this using our own function.
    //Function can have local functions in it (so you do not have to create a private def on the same level
    def masterFunction (x: IndexedSeq[Rational]) = {
      def localFunction(r: Rational) = {
        println(r.numer + " is de numer")
      }
      x.foreach(x=> localFunction(x))

    }
    masterFunction(ratList)

    //Function values. You can assign a function to an object. In this case a function literal (anonymous function)
    val funcVal = (x : Int) => x + 1
    //And call it like any other function
    funcVal(10) //gives 11

    //Lets define a higher order function, it can return and or take functions
    def isOdd (str : String) = {
      val isOdd = if ( (str.length & 1) == 0 ) true else false
      isOdd
    }
    def high (str : String, func : String => Boolean): Boolean = {
        val result = func(str)
        result
    }
    val result = high("michelly",isOdd) //michel gives true, michell gives false etc.
      println("result is " + result)
    //Why? So high can be reused even if isOdd will be replaced by a complete other kind of function

    //Even beter is to use existing methods, which is a higher order function, so you dont have to loop through a list.
    //This will do exactly whet we did before.
    def containsOdd (i : List[Int]) = i.exists(_ % 2 == 1)
    val lijst = List(2,3)
    val contOdd = containsOdd(lijst)
    println ("lijst hass odd? : " + contOdd)

    //Function currying. This means a function will use multiple parameter groups
    def sumInts (x1 : Int)(x2 : Int) : Int = {x1 + x2}
    val summy = sumInts{1}{2} //or sumInts(1)(2) . But { are allowed because it expects 1 parameter each. This allowes a structure which makes it feels like plain Scala

    //So this can be used to create your own control structure :
    //Using higher order functions icw currying and named parameters makes perfect control structure like while, for etc: The : has to be after the name
    //Note: Do not create you own while loop like below, as it's not a good functional programming style
    def whilst(condition: => Boolean)(code: => Unit) = {
      while (condition) {
        code
      }
    }
    //Use our whilst loop:
    var x3 = 0
    whilst(x3 < 10) {
      println(x3)
      x3 += 1
    }

    //How for works, iterating a container object
    val lst = Array[String]("michel","rok","diederik")
    for (x <- lst) {
      //Even Match statement can save results of matched statements (and should be when using functional programming.). This is better then doing a println for each case statement!
      val check = x match {
        case "michel" => "hallo michel"
        case "rok" => "hallo rok"
        case "diederik" => "hallo diederik"
        case _ => "wie is daar?" //Because there is no break (like java has that in a switch statement).  You have to have a other (_) case
      }
      println(check)

    }

    //Abstract classes . Classes without implementation, so declare but do not define.
    //If an class has 1 or more methods not implemented (abstract members instead of concrete members) the class has to be definied abstract. It does also mean it can not be initiated.
    abstract class Elem {
      def contents : Array[String] //No input, array as output. Also no = means no implementation.
      def height : Int = contents.length //parameterless method (with () is the same but called empty-paren method. Use () when doing stuff with side-effects/not pure functions. Like I/O or change var's etc.
      def width : Int = if (height == 0) 0 else contents(0).length
      //val width can not be made because we just declared a method with that name, would be allowed in Java
    }

    //If we want to use abstract classes, we have to use this: Extending classes / inheritance
    //Without the extend keyword the class would extend scala.AnyRef (java's java.lang.object equivalent), the superclass
    class ArrayElement (conts: Array[String]) extends Elem {
      val contents: Array[String] = conts //Notice you can override methods and values , and also mix them up. COntents is now changed to a value instead of a method
      def firstStr : String = contents(0)
    }
    //Hold on parametric field is better because we have now conts and contents, looks like code melting: notice the val in the class parameter list instead of in the body
    class ArrayElement2 (val contents: Array[String]) extends Elem {
      def firstStr : String = contents(0)
      final val theend = 0 //The final keyword prevents inheritance in derived classes
    }
    val klas = new ArrayElement(Array[String]("michel","was here"))
    klas.height    //2
    val sup : Elem = new ArrayElement(Array[String]("een","twee en drie"))//subtyping ,meaning it's compatible . Also called polymorphism
    sup.width //3 works
    //sup.firstStr is not existing  because it was subtyped to Elem.

    //Lets make classes derived from our already derived classes
    class LineElement(s: String) extends ArrayElement(Array(s)) {
      override def width = s.length
      override def height = 1
      //override val theend = 3 : not allowed because its final declared in the superclass
    }

    //What are traits: Traits can be sed to mix classes, like java's interface keyword ,with concrete (not abstract) methods.
    //so you only need to implement it once, on java each class has to implement all methods making it lot of work. so most of the times java provide thin interfaces because of it.
    //Note a trait does not allow class parameters unlike a normal class
    trait trede {
      def eenFunc = {
        println("Ik ben een functie")
      }
    }
    trait trede2 {
      def eenFunc2={
        println("ik ben een andere functie")
      }
    }
    class Frog extends trede {
      override def toString ="Green"
    }
    val frog = new Frog
    frog.eenFunc
    val frog2: Frog = frog
    class Animal {
      def isAnimal = true
    }
    //It allows to extend classes and mix traits to them:
    class Horse extends Animal with trede with trede2 {
      override def toString = "paard"
      override def eenFunc = println("Ik overschrijf trait 1") //So function from trait 1 is overwritten but not eenfunc2 from trait2
    }
    val horse = new Horse
    horse.eenFunc //From trait
    horse.isAnimal //From super class
    horse.eenFunc2 //From 2ncd trait

    //Stackable modifications using traits
    abstract class intQueue {
      def get : Int
      def put (x: Int)
    }
    import scala.collection.mutable.ArrayBuffer
    class BasicIntQueue extends intQueue {
      private val arr = new ArrayBuffer[Int]
      def get = arr.remove(0)
      def put(x: Int) = arr += x
    }
    val basicInt = new BasicIntQueue()
    basicInt.put(10)
    basicInt.put(20)
    basicInt.get //10
    basicInt.get //20
    trait Doubling extends intQueue {
      abstract override def put(x: Int) = { super.put(2 * x)} //super call is allowed when a super class has a concrete method of Put .
      // And Scala has to know its intentionally so abstract override keyword is needed. In class this would not be allowed
    }
    class queue extends BasicIntQueue with Doubling
    val q2 = new queue
    //Or just val q = new BasicIntQueue with Doubling because queue does not implement anything, it just mixes traits.
    q2.put(10)
    q2.get //20
    trait Incrementing extends intQueue {
      abstract override def put(x: Int) = { super.put(x + 1) }
    }
    trait Filtering extends intQueue {
      abstract override def put(x: Int) = { if (x >= 0) super.put(x)}
    }
    //So we want to have some class with queue with filtering and incrementing? With traits we can!
    //Notice: Doing this with inheritance of classes only the super.put of class Filtering would be executed and incrementing would be done! So it is not possbile with classes.
    //Even if you can hard code the class of the super method you want to call it, but this will be useless because the base class of super will be called twice!
    val q= new BasicIntQueue with Incrementing with Filtering //Most right will be called first
    q.put(10)
    q.get //10
    q.put(-10) //not done
    q.get //error

    //Use Trait or not? Just ese concrete class if you don't make reusable code. Use traits if others will be depending code on it. Use abstract class if you want to use it in java.
    //Distribute code in compiled form use abstract class. Is performance important? Use class. Do know yet what to do? Use trait, can always change it later on.

    //Packages features, just see the Rational class, it's in the other file within this project

    //case classes. What are these? Lets show an example.
    sealed abstract class expr //Sealed means in pattern matching scenarios (like match case) the compiler will give an error if no case is matched.
    case class Num(num: Double) extends expr
    case class Val(str:String) extends expr
    case class UnOp(str: String, arg: expr) extends expr
    case class BinOp(str: String, arg1: expr,arg2: expr) extends expr
    case class Oper(oper : String,arg: expr) extends expr //By extending all case classes from expr we can exchange these classes within each other : example UnOp("+",Num(2.0)


    //For all case classes of type expr (it will check the current file only)
    //So only works when doing a <expr> case match <all cases classes>. Abstract means it may have methods not concrete / implemended
    //You could use case _ to get no error but the compiler will then just throw exception. Or use (e: @unchecked) match .... does the same.

    //We now can do all kinds of stufF:
    val x9 = Num(4.0) //Companion object was created automatic in case class. No new keyword is needed
    x9.num //4.0 in case clases parameters of the class can be seen as field/values. In normal classes class parameters can not be seen
    val x8 = Oper("+",Num(3.0)) //No type glutter
    x8.arg == Num(3.0) //true, Case classes implement toString , equals and hashCode so you can automatic compare objects
    val x88 = x8.copy(oper = "-") //x88: Oper = Oper(-,Num(3.0)) , Provides copy statement in which named parameters can be used to change some field.s

    //Pattern matching using match and case classes work out of box in case classes
    //Suppose you want to get the Num(3.0) from this expression: Oper("+",Num(3.0)
    def giveArgExp(exp : expr) : expr = {
      val result = exp match {
        case (Oper("+",e)) => e //Constructor patterns. With case classes deep matching is supported, it checks the methods and its parameter so very powerful
        case (Oper(_,_)) => {  //Wildcards as parameters, let you check certain parameters. Another example List(0,_,_) check for list with length 3 starting with value 0. List(0,_*) fore any list length
                              // Tuple matching (a,b,c) => do something with a b or c to get key value pairs
          println("Yep it was an Oper but no valid operator in it i guess")
          null
        }
        case (Num(e)) => {
          println("The object " + exp + " has no expression in it but the value is " + e + " of type double")
          null
        }
        // Get alle other stuff and use it in your code
        case yourstuff => {
          println("well " + yourstuff + " can not be used by me")
          null
        }
          //This will catch all other stuff without doing anything with it. called wildcard. But this _ can not be reach due to yourstuff above
        case _ => {
          println("Give some expr value in my list .")
          null
        }
      }
      result
    }

    //We can now use it like this:
    val sub = giveArgExp(Oper("+",Num(3.0))) //Num(3.0)
    val sub2 = giveArgExp(Num(5.0)) //null with some print text
    val sub3 = giveArgExp(Oper("-",Num(2.0))) //some text with null
    val sub4 = giveArgExp(Val("pietje")) //SOme text


    //typed patterns: can be used to check types: example use case str: String => println("its a string) to check if some expr is of type String (for example if some functions get an Any object in it.
    def checkType(x: Any) = {
      x match {
        case str : String => println("string type " + str)
        case in : Int => println("Int type " + in)
      }
    }
    val typ = checkType("piet")
    val typ2 = checkType(4)
    "piet".isInstanceOf[String] //Does the same
    // Use combi of <expr></expr>.isInstanceOf[Int] and <expr>.asInstanceOf[type] to cnvert stuff . do not use


    //Variable bounding: Can be used to store expression within a match expression
    val fun6 = UnOp("abs",UnOp("abs",Num(4.0)))
    val res = fun6 match {
      case (UnOp("abs", e @ UnOp("abs",_))) => e //e will be  UnOp("abs",Num(4.0) , it store the whole expression
    }

    //Pattern guard, supposed x + x has to be rewritten to 2 * x : then case UnOp("+", x,x) => ... would fail , x is not allowed twice You could place an if statement within the match expression
    val fun77 = BinOp("+",Num(3.0),Num(3.0))
    val res77 = fun77 match {
      case (BinOp("+",x,y)) if x == y => UnOp("*",Num(2.0))
      case _ => null
    }


    //Options : These will prevents null values . Its clearer to instead of using String us optional String : Option[String]
    val lijstje = Map("Paris" -> "France","Amsterdam" -> "Holland", "Berlin" -> "Germany")
    val op1 = lijstje get "Paris" //Some(France)
    val op2 = lijstje get "Rome" //None
    def showOpt(opt : Option[String]) : String = {
      val res = opt match {
        case Some(x) => x
        case _ => "NOTHING"
      }
      res
    }
    println(showOpt(op1)) //France
    println(showOpt(op2)) //NOTHING

    //patterns are not only used in match case statements. They can be used in many places:
    //1) In values
    val tup = (1, "abc", "def") //SOme Tuple3
    val (a,b,c) = tup //Decontruct the Tuple in 3 variables in 1 statement!
    val exp = BinOp("abs",Num(3.0),Num(2.0)) //Some case class
    val BinOp(cmd,left,right) = exp //Get the parameters of some Expression! (you could also see the class parameters and use theese field like expr.str expr.arg1 and expr.arg2
    //2) Case sequence as partial functions (so without match)
    val funcy : Option[Int] => Int = {
      case Some(x) => x
      case None => 0
    }
    val x99 = funcy(Some(10)) //10
    val x100 = funcy(None) //0
    //3) patterns in for expression
    for((country,city) <- lijstje) print(country + " is in " + city) //paris is in france, amsterdam un holland etc

    //List . A list is like array but inmutable. Also homogeneous so 1 type at a time
    val list1 = List(1,2,3) //List[Int]
    val list2 = List(List(1,2),List(8,9)) //Lis[List[Int]]
    list2(1)(1) //9
    //covariant: List[String] is subtype of List[Object] . So List[String] = List() is allowed. List() is same as List[Nothing] because Nothing is subtype of List[T] (any other object)
    //List can be build using Nil or :: (pron. cons). : is an infix operator were the first operator is attached at the front of a list. So x :: y is treaded as ::(x,y) and :: is the class, x the element, y the list.
    val list3 = list1 :: list2 //list3: List[List[Int]] = List(List(1, 2, 3), List(1, 2), List(8, 9))
    val list4 = Nil //empty List
    val list6 = list4 :: list3 //list6: List[List[Int]] = List(List(), List(1, 2, 3), List(1, 2), List(8, 9))
    list3.head //res6: List[Int] = List(1, 2, 3)
    list3.tail //res3: List[List[Int]] = List(List(1, 2), List(8, 9))
    list3.isEmpty //false
    //list pattern matching
    val List(a6,b6,c6) = list3
    //a: List[Int] = List(1, 2, 3)
    //b: List[Int] = List(1, 2)
    //c: List[Int] = List(8, 9)
    //or if you don't know length use:
    val a9 :: b9 :: rest = list3
    //First order function does not take functions as param, higher order functions do.
    val list7 = list3 ::: list6 //Concat both lists into 1 List: list7: List[List[Int]] = List(List(1, 2, 3), List(1, 2), List(8, 9), List(), List(1, 2, 3), List(1, 2), List(8, 9))
    list3.length //3 (do not replace with
    //init and last is exact opposite of head and tail
    list3.last //List(8,9)
    list3.init //res10: List[List[Int]] = List(List(1, 2, 3), List(1, 2))
    list3.reverse //res11: List[List[Int]] = List(List(8, 9), List(1, 2), List(1, 2, 3))
    list3.take(2) //take first 2 elems: res12: List[List[Int]] = List(List(1, 2, 3), List(1, 2))
    list3.drop(2) //drop first 2 elems : res13: List[List[Int]] = List(List(8, 9))
    list3 splitAt 2 //Make 2 List from pos 2 : res14: (List[List[Int]], List[List[Int]]) = (List(List(1, 2, 3), List(1, 2)),List(List(8, 9)))
    list3 apply 2 //give 2nd param (same as list3(2) : res15: List[Int] = List(8, 9)   ,
    list3.indices //There are 3 elements in it (happening to be lists) : res16: scala.collection.immutable.Range = Range(0, 1, 2)
    list3.flatten //If contents are Lists itself get contents of these Lists and put in 1 list: res17: List[Int] = List(1, 2, 3, 1, 2, 8, 9)
    List(6,7,3) zip List(6,7) // concat 2 list, left list is used as index . Different in lenght data is lost: res19: List[(Int, Int)] = List((6,6), (7,7))
    val zipje = List(6,7,3) zipWithIndex //Zip list with location in lists: res20: List[(Int, Int)] = List((6,0), (7,1), (3,2))
    val unzipje = zipje unzip  //and undo: unzipje: (List[Int], List[Int]) = (List(6, 7, 3),List(0, 1, 2))
    val str = list3.toString //String format for printing: res21: String = List(List(1, 2, 3), List(1, 2), List(8, 9))
    list3 mkString("LIJST(","|",")") //use your own string format : res23: String = LIJST(List(1, 2, 3)|List(1, 2)|List(8, 9))
    val arr = list3.toArray //make it array (flat no recursion), toList within Array to get back arr: Array[List[Int]] = Array(List(1, 2, 3), List(1, 2), List(8, 9))
    list3 //res27: List[List[Int]] = List(List(1, 2, 3), List(1, 2), List(8, 9))
    list3.copyToArray(arr,2) //copy list to array in pos 2:  It has to be large enough, as you can see not all elements could be copied:
    arr //res26: Array[List[Int]] = Array(List(1, 2, 3), List(1, 2), List(1, 2, 3))
    //mergesort is way more iffecient then copyToArray, but gonna skip this for now.
    //Hiher order functions List prevents loops
    List(1,2,3) map (_ + 1) //    List(2, 3, 4)
    val lijst999 = List("Michel","was","here") map (_.reverse) // List(lehciM, saw, ereh)
    lijst999 map (_.toList.reverse.mkString) //List(Michel, was, here)
    lijst999 flatMap (_.toList.reverse.mkString) //List(M, i, c, h, e, l, w, a, s, h, e, r, e) flatmap makes sure no sub collection exists
    List.range(1,5) map (_ + 1) //List(2, 3, 4, 5) , Range creates elements 1 tm 5
    var sum=0
    val sommetje = List.range(1,5) foreach (sum += _) //Foreach can be done to do somethin foreach element
    val sommetje2 = List.range(1,5) map (sum += _) //Foreach can be done to do somethin foreach element , like map
    sum //20 because 10 and 10 from 2 prev sum actions
    val filt = List.range(1,6) filter (_ > 3) //filter can be used to use a function returning a Boolean to make a selection criteria for elements in a List. Result: List(4,5)
    //partition is like filter but it creates 2 list 1 for result true and one for false
    List.range(1,6) partition(_ % 2 == 0) // (List[Int], List[Int]) = (List(2, 4),List(1, 3, 5))   , so 1 has even nums and other odd
    //Find is also like filter but only gives first result, if any, so its a Option value which could be None instead of Some
    List.range(1,6) find (_ % 2 == 0) //Some(2)
    //takewhile continues scrolling thourhg a list until the expression is not satisfied and returns all values until that moment
    List(1,2,3,-4,5) takeWhile( _ > 0) //List(1, 2, 3)
    //dropWhile drops elements until all element satisfy expre. ??? Not working like i thought
    List("michel","was","here","helelangestring") dropWhile (_.length > 4) //List[String] = List(was, here, helelangestring)
    //span combines takeWhile and dropWhile in 1 action and return 2 lists, prevents going through list twice
    List(1,2,3,-4,5,6) span (_ > 0) //(List[Int], List[Int]) = (List(1, 2, 3),List(-4, 5, 6))
    //forall gives result if ALL elements satisfy some condition
    List(1,2,3,4) forall (_  > 2) //false
    List(1,2,3,4) forall (_  > 0) //true
    //exists if 1 or more satisfy
    List(1,2,3,4) exists  (_  > 2) //true
    List(1,2,3,4) exists  (_  < 0) //false
    // /: is a fold lect operation on List which requires a start value , a list and a operation .
    def sumx(x: List[Int]) : Int = {
      (10 /: x) (_ + _)
    }
    sumx(List(1,2,3)) //16 , something like op(3,op(2,op(10,1)) were op is _ + _  starting with left values first
    // :\ does the same but starting from the right elements. For product + this make now difference.
    // sorWith sort the list based on the function
    List(1,2,-3,-6,5) sortWith(_ < _) // List[Int] = List(-6, -3, 1, 2, 5)
    //Some methods are outside the List class, eg in the companion objects and are mostly factory methods:
    List.apply(1,2,3) //Create List(1,2,3)
    List.range(1,5) //create list with 1 tm 5
    List.fill(3)("hello") //Makes list with 3 times some entry like : List(hello, hello, hello)
    List.fill(2,3)("x") // More parameters  makes multidimensional collection: List(List(x, x, x), List(x, x, x))
    List.tabulate(2,3)(_ + _) //Like fill but uses your function to fill : List(List(0, 1, 2), List(1, 2, 3))
    List.concat(List(1,2),List(3,4)) //make 1 lst of 2 lists : List(1, 2, 3, 4)
    (List(10,20),List(40,60)).zipped.map(_ * _) // processing multiple lists together. zipped uses tuples to split up lists so you can use map to do some function on each : List(400, 1200)

    //Collections.
    //Sequence , groups of data lined up in order. List is a Sequence for example allready discussed
    //Array, fixed nr of elements and which can be accesed efficiently withoug going through the list (using index).
    val arr1 = new Array[Int](5)
    val arr2 = Array(0,0,0,0,0) //Is the same as above! But now we use the companion object factory method
    arr1(4) //Get value at pos 4 quickly, with List this can be done but goes through whole List.
    //When using List you can efficiently use the head but if you need the end, you  have to build the list backwards and calling reverse is awkward.
    //Use ListBuffer instead, you can prepand (+=: method or append (+= method) ), its mutable
    import scala.collection.mutable.{ListBuffer,ArrayBuffer}
    val buffy = new ListBuffer[Int]
    buffy += 1 //ListBuffer(1)
    buffy += 2 //ListBuffer (1,2)
    3 +=: buffy //ListBuffer (3,1,2) , uses implicit conversion i guess
    //Arraybuffers is like Array but you can also prepand and append elements like ListBuffer. Unlike ListBuffer Arraybuffer can quickly get element x with index.
    val arry = new ArrayBuffer[String](5)
    arry += "truffel"
    arry += "gone"
    arry(0) //truffel
    arry.length //2
    //Strings can be used with StringOps , its imported by default using Predef package. Using implicit conversions String is transformed to StringOps making it a Sequence.
    "michel".length //6
    "michel" (3) //h
    "michel" exists(_.equals('m')) //true , exists is not in String but StringOps!
    //sets and maps. With set you can make sure only 1 uniq elements exists per object. Bu default inmutable, but import will make mutable one usable
    import scala.collection.mutable
    val muta = mutable.Set(1,2,3,4,3) //Set(1,2,3,4)
    val empty = mutable.Set.empty[String]
    val words = "These are some words, yes these are indeed words to know"
    val wordArray = words.split("[ ,]+") //Split words in Araay[String] elements
    for (word <- wordArray) empty += word.toLowerCase //Iterate through them and add them to mutable list
    empty //Set(indeed, these, to, words, know, are, yes, some)
    //Maps is like array, but instead of some index nr use can use any type of key
    import scala.collection.mutable
    val mapy = mutable.Map.empty[String,String]
    mapy("michel") = "nossin"
    mapy += "truus" -> "nossin" //res32: mapy.type = Map(michel -> nossin, emile -> nossin)
    mapy.contains("truus") //true , look for key
    mapy.keys //Set(michel, truus)
    mapy.values //HashMap(nossin, nossin)
    mapy.size //2
    mapy ++= List("saskia" -> "achternaam", "kees" -> "wasnothere") // Add multiple entries in mutable map: Map(saskia -> achternaam, michel -> nossin, emile -> nossin, kees -> wasnothere). ordered and uniq
    val inmutamap = Map("parijs" -> "frankrijk", "berlijn" -> "duitsland")
    val newmapy = inmutamap + ("amsterdam" -> "holland") //Add entry , scala.collection.immutable.Map[String,String] = Map(parijs -> frankrijk, berlijn -> duitsland, amsterdam -> holland)
    val newmapy2 = newmapy ++ List("madrid" -> "spain","rome" -> "italie") //Add multiple antries: Map(amsterdam -> holland, madrid -> spain, berlijn -> duitsland, parijs -> frankrijk, rome -> italie)
    //map and set use emptyset, set1, set<x> and hashset when more then 5 entries are in it. map is same thing.
    //sortedset and sortedmap traits, implemented by treemap and treesort an be use if you want some special kind of ordering., determined by ORdered trait.
    //Tuples can be used to group stuff without created a class.
    val x999 = (1,"michel","nossin")
    x999._2 //michel
    val (a99,b99,c99) = x999 //inits a,b,c with _1,_2 and _3 .Use () otherwise all vals get the complete tuple.

    //stateful class (see bottom of this file to see definition)
    val account = new bankAccount
    account deposit 30.0 //No need to use () or =
    account deposit 50.0
    val getMoney = account withdraw 30.0
    val cury = account.balance //50
    //account current = 3.0 //give some feature warning but is posible when var bal is public instead of private
    account.resetAccount
    account.balance //0


    //Type parameterization, allowes to create generic classes like set[T] where T can be any class like Set[String]
    class Queue[T] (lijst : List[T]) { //T means any Class
      def head = lijst.head
    }
    object Queue { //COmpanion object so calls apply method and new method is not required (factory method apply makes the object)
      def apply[T](lijst : T*) : Queue[T] = { //T is the type like String, so T* means 1 or more Strings as parameter , returns a created Queue containing T (so Strings)
        new Queue(lijst.toList) //The T's , so multiple Strings for example are put in  a list and is just to create the Queue
      }
    }
    val obj = Queue("Michel","truffel") //Test it
    obj.head //Michel
    //Information hiding, for example you need 2 lists but only want 1 to be seen by user:
    trait Queue2[+T] { //T = any type , +T means enable covariance, so Queue2[String] for example is a subtype of Queue2[AnyRef]
    //If Queue2[-T] was used then its contravariant , the otherway arround T would be a subtype of Queue[S]
      def head : T
      //def someFun[ U >: T](x:U) : Queue2[T] //Lower bounds. this allows for example to Add Apple classes to Banana Classes and get Fruit class back (assuming to be the super class of apple and banana)
    }
    object Queue2 {
      def apply[T](lijst : T* ) : Queue2[T] = {
        new QueueImpl[T](lijst.toList,lijst.reverse.toList)
      }
      private class QueueImpl[T] (private val lijst1 : List[T], private val lijst2 : List[T]) extends Queue2[T] { //note the val 's because it's private you have to add them
        def head : T = lijst1.head
      }
    }
    val obj2 = Queue2("Jan","Truus") //No multiple lists given. Just multiple parameters as 1 list.
    obj2.head //Jan
    //Queue2 is a trait no a type (you can not make a val or var with it.
    //but its a type constructor
    def mijnFunctie (lijstie : Queue2[AnyRef] ) = { //this is allowed
      lijstie.head
    }

    //Is Queue2[String] a subtype from Queue2[AnyRef] ?
    //No not be default but you can enable Covariance by using +T (which is enabled in our trait , see above.
    mijnFunctie(obj2) //Jan , because +T is defined our Srings are allowed in AnyRef ask within mijnFunctie
    //mijnFunctie(Queue2(1,2,3)) //not allowd Int is no anyref but anyval
    mijnFunctie(Queue2(Option("hallo"))) //This works: Some(Hallo)


    //Lower bounds:
    //see Queue2 example

    //Abstract members
    //Scala has abstract methods like Java, but also field and types. Abstract components requires you to make them concrete and implement them
    trait Abstract { //Abtract is by definition abstract because a trait it is allowed to have 1 or more abstract members. so abstract keyword not needed. A class does need Abstract declared expl.
      val x : Int
      var y: Int //var get by default always a getter and setter method so in the trait in this case.
      def func : Int
      type T
    }
    //Implement it
    class Implement extends Abstract {
      lazy val x = 10 //Lazy means its only init'ed when used. Ideal for usage were order of init multiple params does not matter (good for performance)
      var y = 20
      def func : Int = { 10 }
      type T = String //Call this a Type alias. Everywere T is mentioned you put in String
    }
    //or use a anonym class (note the {} instead of ()! )
    val hallo = new Abstract {
      val x = 10
      var y = 20
      def func : Int = { 10 }
      type T = String
    }
    //abstract ype
    class Food
    abstract class Dier {
      type SuitableFood <: Food //This will allow animals/dieren to eat foods, but only the once suitable for that animal . Called path dependent type. like java inner class but then looks at objects outside its class
      def eats (food : SuitableFood)
      type AnimalsThatEatGrass = Grass //Defining a structural type
    }
    class Grass extends Food
    class Fish extends Food
    class Cow extends Dier {
      type SuitableFood = Grass //path dependent type
      override def eats (food: Grass) {}  //Would not compile without suitablefood type. Without the suitablefood type (and just using Food) it would mean we could feed a fish to a cow for example. So it wont compile
      //Not allowed food: Fish

    }
    val fish1 = new Fish
    val cow1 = new Cow
    val cow2 = new Cow
    cow2 eats (new cow1.SuitableFood) //path dependent type
    class Outer {
      class Inner //Like java support of inner classes.
    }
    val out1 = new Outer
    val out2 = new Outer
    val in1 = new out1.Inner //Path dependent type (refers to object)
    val in2 = new out2.Inner //Also path dependent type, does not equal latter .
    // Both math the general type Outer#Inner. Note the # instead of .
    //Extending a class means named/nomimal subtypes . To have structural subtypes use "refinement types" , more flexible but less convient (no names)
    //Use it when a type only has members and no further meaning. You could use a trait AnimalsThatEatGrass and extend it to animals that eat grass
    //Better is refinement types (Defined in the Dier class:
    abstract class Pasture {
      val Animals : List [ Dier { type SuitableFood = Grass } ] //Makes sure that Animals defined do eat Grass ,so when a Fish has type SuitableFood = Weed is is not allowed
    }
    //Another user is grouping of classes
    def using2[T,S](obj : T) (oper : T => S) = {
      val result = oper(obj)
      //obj.close NOT ALLOWED because T , any object, does not always have a clode() method
      result
    }
    //Solution structural subtype:
    def using[T <: { def close() : Unit } ,S](obj : T) (oper : T => S) = {
      val result = oper(obj)
      obj.close //ALLOWED , because user is forced to pass object with a close method
      result
    }
    //example:
    //This works (has close
    using(new PrintWriter("michel.txt")) {
      writer =>  writer.println(new Date)
    }
    //Enumerations Also used to create type (subtype)
    object Color extends Enumeration {
      val Blue = List(0,0,1)
      val Green = List (0,1,0)
      val Red = Value //Value is part of Enumeration class, calling Vlaue method gives the instance of all members within the object Color
    }
    val Blue2 = Color.Blue
    import Color._
    val Blue3 = Blue //From now on COlor. is not needed to call
    val Red2 = Red //Value containing instance of Color
    object Directions extends Enumeration {
      val North = Value("North") //Now Value can be used to create an instance of Color with a specific value
      val South = Value("South")
      val East = Value("East")
      val West = Value("West")

    }
    import Directions._
    val wind = North
    //Iterate thourh Enumeration, only when values is used:
    for (d <- Directions.values) println (d)
    Directions.East.id //2 , each value has a key
    Directions(3) //West , using values the other way around
    for (d <- Color.values) println (d) //Only Red because not all use Values in Color class

    //Implicit conversions and parameters
    //Conversion:
    //EXAMPLE 1
    //Swing is a library used to convert OS events into event platform independ events that code can use.
    //In java you would create a button and add a OBJECT to it (the listener) button.addActionListener(new ActionListener : def ActionPErformed(event: ActionEvent) { println"pressed"}}}}
    //In scala : button.AddActionListener( (_: ActionListener) => print("pressed"))
    //However Swing expects object not a function.
    //Conversion can be used to change Swing!
    val button = new JButton()
    implicit def function2ActionListener(f: ActionEvent => Unit) = {
      new ActionListener {
        override def actionPerformed(e: ActionEvent): Unit = f(e)
      }
    }
    //Now a function can be used
    button.addActionListener(
      function2ActionListener(
        ((_: ActionEvent) => println("pressed"))
      )
    )
    //Remove the boilerplate is even better:
    button.addActionListener(
      (_:ActionEvent) => println("pressed")
    )
    //EXAMPLE 2
    //implicit def intToString(i : Int) : String = i.toString
    3 + "2" //Not really needed is included by default Scala packages
    //EXAMPLE 3
    case class Dollar (amount: Double)
    case class Euro (amount: Double) {
      def + (e : Euro) : Euro = Euro(this.amount + e.amount) //Functional style: Always return an object
    }
    val eur = Euro(3.00)
    val dol = Dollar(2.00)
    // NOT ALLOWED: val total = eur + dol
    implicit def eurToDol(e : Euro) : Dollar =  Dollar(e.amount * 0.7)
    implicit def DolToEur(e : Dollar) : Euro =  Euro(e.amount * 1.3)
    val total = eur + dol //5.6 Euro it works!
    val total2 = dol + eur // Also works... Not sure why we did not define + method in class Dollar. Guess it can see the + in the Euro class
    //implicit parameters
    class greet  {

      def hello (name : String)(implicit greet : String) = {
        println("Hello " + name + ", this is my greeting " + greet)
      }
    }
    val grt = new greet
    grt.hello("michel")("Doe de groeten")
    object greet2 {
      implicit val grtz = "Joehoe"
    }
    import greet2._
    grt.hello("jan") //joehoe... No 2nd parameter given , it knows to just use the implicit imported class of type String because we used the Implicit keywordon both classes

    //for loop
    case class Person (name: String, male: Boolean, children : Person* )
    //Lets define a family
    val p1 = Person("piet",true)
    val p2 = Person("truus", false)
    val p3 = Person("pappie",true,p1,p2)
    val plist : List[Person]= List(p1,p2,p3)
    //get all (<daddie>,<child>) combinations
    plist filter (p => p.male) flatMap (males =>  males.children map (child => (males.name,child.name))) //List((pappie,piet), (pappie,truus)) , or use withFilter that does not make intermediate object
    //Not so easy to read, for can do it better
    for (p <- plist ; if p.male ; c <- p.children) yield (p.name,c.name) //Generator and filter within for loop. Also definition is allowed like n = p.name . All elements seperated by ;
    //several generators are allowed
    for (p <- 1 to 10 ; y <- 1 to 10) println(p + "," + y)
    //to query
    val queryList = for (a <- plist ; if a.name startsWith "pi" )  yield a.name //List(piet)

    //collections in scala , easy to use, concise , safe, fast, universal
    //scala.collection.[inmutable|mutabable] is the source. Inmutable never change, they simulate change by returning new objects. and it's the defaul.t The mutable has to be imported.
    //scala collection itself is as supertrait of the inmutable and mutable packages which can be both mutable or inmutable.
    //Traversable -> iterable -> Seq | Set | Map are the main classes with each having subclasses like sortedset, hashmap etc etc.
    //Traversable only has abstract foreach method. The is concrete like additions (++) to<type> and map stuff (map, flatmap etc), copy<xx> , size stuff (size, isempty), retrieval (head last etc)
    //and many more. books describes them
    //ITerable , all methods are using an abstract method iterator . methods like next and zip, or zippedwith are definined. Foreach is implemented by mostly overwritten by subclasses

    //Skipped some chapters about collections , and how to build your own collections.

    //extractors
    //is an object with unapply method used for complex pattern matching.
    //so eg 2 emails addresses in a list, check if these are the same.
    object Email {
      def apply(user: String, domain : String) = user + "@" + domain
      def unapply (str: String) : Option[(String,String)] = {
        val parts = str split "@"
        if (parts.length == 2) Some(parts(0),parts(1)) else None

      }
    }
    object Twice {
      def unapply(str: String) : Option[String] = {
        val len = str.length / 2
        val half = str.substring(0,len)
        if (half == str.substring(len)) Some(half) else None
      }
    }
    object UpperCase {
      def unapply (str : String) : Boolean = str.toUpperCase == str
    }
    //Normal unapply example by matching emails
    val e1 = Email("michel","gmail.com")
    val e2 = Email("saskia","piet.com")
    val valu : Any = "michel@gmail.com"
    val res67 = valu match {
      case Email(user, domain) => user + " at " + domain
      case None => "not an email"
      case _ => "Something went wrong"
    }
    def userTwice (str: String) = str match {
      case Email(Twice(x @ UpperCase()),domain) => {
        x + " in domain " + domain + " twice"
      }
      case _ => "No match"
    }
    userTwice ("DIDI@gmail.com") //yes DI in domain gmail.com twice
    userTwice ("didi@hotmail.com") //no
    userTwice ("YOYO@shell.cl") //yes , YO in domain shell.cl twice
    //Variable length extractors
    object Domain {
      def apply (parts : String*) : String = {
        parts.reverse.mkString(("."))
      }
      def unapplySeq(whole : String) : Option[Seq[String]] = {
        Some(whole.split("\\.").reverse) //reverse to easily use Sequence
      }
    }
    def isTomInDotCom(s: String) : Boolean =
      s match {
        case Email("tom",Domain("com",_*)) => true
        case _ => false
    }
    isTomInDotCom("tom@schiphol.nl") //false
    isTomInDotCom("tom@schiphol.com") //true
    isTomInDotCom("jan@schiphol.com") //false
    val dom = "truus.google.nl"
    val res90 = dom match {
      case Domain ("nl","schiphol","piet") => println("is ook piet domein onder schiphol.nl")
      case Domain("nl","schiphol",_*) => println("Is schiphol.nl")
      case Domain("nl",_*) => println("is ook  nl")
      case _ => println("no match")

    }
    //Case classes can be used within match case C(..) . But it only tells you it's a Class C.
    //Client code using it would depend on the class.. You can not rename or change it. So for libraries thats bad.
    //However case classes are better in performance and to set up. (less code, compiler optimilisation)
    //Extractor use representation independence , if the type changes it still works.
    //Extractors are great for working with regular expressions:
    val decimal = new Regex("(-)?(\\d+)(\\.\\d*)?") //optional - , 1 or more number , optionally with a . and 0 or more numbers
    //or
    val decimal2 = "(-)?(\\d+)(\\.\\d*)?".r
    val input = "for -1.0 to 99  by 3"
    for (s <- decimal findAllIn input) { println(s) } //-1.0 99 3  , or just findFirstIn or findPrefixOf
    //You get extrators for free in RegEx!
    val decimal(sign,integerpart,decimalpart) = "-2.3"
    //sign: String = -
    //  integerpart: String = 2
    //decimalpart: String = .3
    //You could used any name, it's looks for groups with () and there should be 3 groups in this case


    //Annotations
    //USed to show that methods are depracated or volatile etc



    //Modular systems, like Spring and Guice can be used in Java to seperate implementations of modules as classes and interfaces of modules as interfaces and configure them within xml configs,
    //Scala has build in features. Modules can be used for example if you have a database and a message service, you would first only use 2 versions, later on test data on only the database, and
    //later for both. This can be done with Modules. also using module teams van better seperate their work.
    //use abstract classes to define the interface, and objects for the implementation.
    case class Passenger (val name : String, val male : Boolean) {
      override def toString() = name
    }
    abstract class Plane {
      val name : String
      val seats : Int
      val maxSpeed : Int

      override def toString() = name

    }
    abstract class Flight {
      val flight : String
      val plane : Plane
      val passengers : List[Passenger]
      def getFemalePassengers(): List[Passenger] = passengers.filter(_.male == false)
      override def toString() = flight
    }
    object B747 extends Plane {
      val name = "Boeing 747"
      val seats = 800
      val maxSpeed = 950

    }
    object B737 extends Plane {
      val name = "Boeing 737"
      val seats = 200
      val maxSpeed = 850

    }
    object KL123 extends Flight {
      val flight = "KL123"
      val plane = B747
      val passengers = List(Passenger("Michel",true),Passenger("Truus",false),Passenger("Diederik",true),Passenger("Rok",true))

    }
    object MP876 extends Flight {
      val flight = "MP876"
      val plane = B747
      val passengers = List(Passenger("Jan",true),Passenger("Miep",false),Passenger("Tom",true),Passenger("Casper",true))

    }
    object JK856 extends Flight {
      val flight = "JK856"
      val plane = B737
      val passengers = List(Passenger("Jannetje",true),Passenger("Miepie",false),Passenger("Tomy",true),Passenger("Caspertje",true))

    }
    object JO846 extends Flight {
      val flight = "JO846"
      val plane = B747
      val passengers = List(Passenger("Obama",true),Passenger("Wawel",false),Passenger("Michel",true),Passenger("Emile",true))

    }
    abstract class FlightPlanning {
      val name : String
      val flights : List[Flight]
      def getFemalePassengers() : List[Passenger] = flights.flatMap(fl => fl.getFemalePassengers)
      def getWideBodyPlaneFlights : List[Flight] = flights.filter(flight => flight.plane.seats > 500)
      def getUniqPassengers : Set[Passenger] = flights.flatMap(fl => fl.passengers).toSet //set is uniq
      def getPassengersMultipleFlights : Set[Passenger] = {
        flights.flatMap( fl => fl.passengers) groupBy(pas => pas) mapValues(_.size) filter (_._2 > 1) keySet
      }

      override def toString() = name
    }
    object EuropePlanning extends FlightPlanning {
      val name = "Simple flight planning"
      val flights = List(KL123,MP876)

    }
    object WorldPlanning extends FlightPlanning {
      val name = "World flight planning"
      val flights = List(KL123,MP876,JK856,JO846)

    }
    object applic {
      val flightplanning : FlightPlanning = WorldPlanning //Change this accordingly !!!. or use : FlightPlanning.type (singleton type) to not check type type but if the object is the same.
      //that could be usefull if you use this object in another place with another object that extends another class but is actually the same.

      def getWideBodyFlight : List[Flight] = flightplanning.getWideBodyPlaneFlights
    }

    //Lets test our application:
    KL123.plane.maxSpeed //950
    KL123.getFemalePassengers() //List(Truus)
    applic.getWideBodyFlight //Europe : List(KL123, MP876) , World : List(KL123, MP876, JO846) , in world as you can see JK856 is a B737 and is not in list, not widebody.
    //in world mode:
    applic.flightplanning.getFemalePassengers() //List(Truus, Miep, Miepie, Natjat) in world mode
    applic.flightplanning.getUniqPassengers //Set[Passenger] = Set(Tomy, Truus, Tom, Rok, Diederik, Jawel, Jan, Miep, Emile, Miepie, Michel, Obama, Caspertje, Jannetje, Casper)
    applic.flightplanning.getPassengersMultipleFlights //Set[Passenger] = Set(Michel)
    //I'll skip the part of equiality icm parameterized type
    //I'll skip the part of combining java and scala

    //concurrentcy and actors.
    //in java multi threading is difficult. In scala it's  easier with actors (altough we will use spark). Only uses message, no locks or memory
    import akka.actor.ActorSystem
    import akka.actor.Props
    import akka.actor.ActorRef
    import akka.actor.Actor

    import akka.actor.ActorLogging
    import akka.actor.Terminated

    object Greeter {
      case object Greet
      case object Done
    }

    class Greeter extends Actor {

      def receive = {
        case Greeter.Greet =>
          println("Hello World!, sending to sender done message")
          sender() ! Greeter.Done
      }
    }

    class Acteur extends Actor {

      override def preStart(): Unit = {
        // create the greeter actor
        val greeter = context.actorOf(Props[Greeter], "greeter")
        // tell it to perform the greeting
        greeter ! Greeter.Greet
      }

      def receive = {
        // when the greeter is done, stop this actor and with it the application
        case Greeter.Done => {
          println ("Acteur stopped")
          context.stop(self)
        }
      }
    }

    val system = ActorSystem("Hello")
    val accy = system.actorOf(Props[Acteur], "helloWorld") //Hello World!, sending to sender done message + Acteur stopped
    //To debug actors use self, of if you want each thread to be an actor on its own
    //self ! "hallo" //use within shell only?
    //self receiveWithin(1000)  case { x => x } //receive itself can make you wait forever, receive will block the shell itself

    //Use react to turbo boost, receive is slow. React does not return anythin so it has to handle everything. But it can release its stack.
    //example 1
    object turbo extends scala.actors.Actor {
      def act() = {
        react {
          case str : String => {
            println("it was a string")
            act()
          }
          case _ => {
            println("It was NOT a string")
            act()
          }
        }
      }
    }
    turbo.start()
    turbo ! "een string" //it was a string
    turbo ! 123 //it was not a string

    //example 2
    //should use this, scala.actors is depracted : akka.actor.Actor
    class SleepyReactor extends scala.actors.Actor {

      def act() {
        loop {
          react {
            case x => {
              println("reacting to %s on thread %s".format(x, Thread.currentThread.getName))
              Thread.sleep(1000)
              println("done with " + x)
            }
          }
        }
      }
    }
    val sleepyOne = new SleepyReactor
    sleepyOne.start
    sleepyOne ! "first" // runs on thread-5
    // wait until completion
    sleepyOne ! "second" // runs on thread-3
    //Skipped a lot of actor stuff, but these are the basics.


    //combinator parsing.
    //To create your own language, eg read own config file
    //create your own parser and lexical analyzer is hard or just use parser generators in C or Java like ANTLR, and scanner generator like Lef,Flex en Jflex
    //or use internal domain specific language DSL

    //example 1
    import scala.util.parsing.combinator._

    class Compiler extends JavaTokenParsers {
        def expr: Parser[Any] = term~rep("+"~term | "/"~term)
        def term : Parser[Any] = factor~rep("*"~factor | "/"~factor)
        def factor : Parser[Any] = floatingPointNumber | "("~expr~")"
    }
    val compi = new Compiler
    compi.parseAll(compi.expr,"2 + 2") //parsed
    compi.parseAll(compi.expr,"-4 + (-4 / 4)") //parsed
    compi.parseAll(compi.expr,"-4 + (-4 / 4 ))") //failure

    //example 2 Lets create some language
    class Language extends JavaTokenParsers {
      def expr: Parser[Any] = "{"~cmd~rep("\""~tekst~"\"")~"}"
      def tekst : Parser[Any] = "[a-z0-9A-Z]*".r
      def cmd : Parser[Any] = "print" | "message"
    }
    val lan = new Language
    lan.parseAll(lan.expr,"2 + 2") //failed
    lan.parseAll(lan.expr,"{printje\"hallo\"}") //failed
    lan.parseAll(lan.expr,"{print \"hallo\" }") //parsed
    lan.parseAll(lan.expr,"{message \"hallo\" }") //parsed
    lan.parseAll(lan.expr,"message \"hallo\" }") //failed
    lan.parseAll(lan.expr,"{echo \"hallo\" }") //failed
    lan.parseAll(lan.expr,"{message \"ha#llo\" }") //failed

    //example 3 json parser
    class JSON extends JavaTokenParsers {

      def value : Parser[Any] = obj | arr |
        stringLiteral |
        floatingPointNumber |
        "null" | "true" | "false"

      def obj   : Parser[Any] = "{"~repsep(member, ",")~"}"

      def arr   : Parser[Any] = "["~repsep(value, ",")~"]"

      def member: Parser[Any] = stringLiteral~":"~value
    }

    val js = "{\n    \"address book\": {\n      \"name\": \"John Smith\",\n      \"address\": {\n        \"street\": \"10 Market Street\",\n        \"city\"  : \"San Francisco, CA\",\n        \"zip\"   : 94111\n      },\n      \"phone numbers\": [\n        \"408 338-4238\",\n        \"408 111-6892\"\n      ]\n    }\n  }"
    val jsp = new JSON
    jsp.parseAll(jsp.value,js) //parsed
    //repsep is like rep but repsep(exp,seperator) can be used to match a list with seperator. opt(exp) can be used as a option
    //example 4 real usage json
    //var mapje : Map[String,Any] = _

    class JSON1 extends JavaTokenParsers {
      def obj : Parser[Map[String,Any]] = "{"~> repsep(member,",") <~"}" ^^ (x => { x.foreach(println) ; Map() ++ x} ) //(Map() ++ _)
      def arr : Parser[List[Any]] = "["~> repsep(value,",") <~"]"
      def member: Parser[(String,Any)] =  stringLiteral~":"~value ^^ { case name~":"~value => (name,value)}
      def value: Parser[Any] = ( obj | arr | stringLiteral | floatingPointNumber ^^ (_.toDouble) | "null" ^^ (x => null) | "true" ^^ (x=>true)| "false" ^^ (x=>false) )
    }
    val js1 = "{\n    \"address book\": {\n      \"name\": \"John Smith\",\n      \"address\": {\n        \"street\": \"10 Market Street\",\n        \"city\"  : \"San Francisco, CA\",\n        \"zip\"   : 94111\n      },\n      \"phone numbers\": [\n        \"408 338-4238\",\n        \"408 111-6892\"\n      ]\n    }\n  }"
    val jsp1 = new JSON1
    val output55 = jsp1.parseAll(jsp1.value,js1) //parsed
    /*
    ("street","10 Market Street")
    ("city","San Francisco, CA")
    ("zip",94111.0)
    ("name","John Smith")
    ("address",Map("street" -> "10 Market Street", "city" -> "San Francisco, CA", "zip" -> 94111.0))
    ("phone numbers",List("408 338-4238", "408 111-6892"))
    ("address book",Map("name" -> "John Smith", "address" -> Map("street" -> "10 Market Street", "city" -> "San Francisco, CA", "zip" -> 94111.0), "phone numbers" -> List("408 338-4238", "408 111-6892")))

    output55: jsp1.ParseResult[Any] = [14.4] parsed: Map("address book" -> Map("name" -> "John Smith", "address" -> Map("street" -> "10 Market Street", "city" -> "San Francisco, CA", "zip" -> 94111.0), "phone numbers" -> List("408 338-4238", "408 111-6892")))
*/
    //The boosk describes in more details what the parsers does ,skipped it,

    //Gui programming
    import scala.swing.TextField
    import scala.swing._

    object michelGUI extends SimpleSwingApplication {
      def newField = new TextField {
        text = "<Fill in>"
        columns = 20
      }

      val field1 = newField
      val field2 = newField

      def top = new MainFrame {
        title = "Michel rocks"
        val button = new scala.swing.Button("Clicks: 0")
        val clicksLabel = new Label("Clicks")
        contents = new FlowPanel(new Label("Michel:"), field1, new Label("Pietje:"), field2,clicksLabel,button)
        listenTo(button)
        var nClicks = 0
        reactions += {
          case scala.swing.event.ButtonClicked(b) => {
            nClicks = nClicks + 1
            clicksLabel.text = "Clicks: " + nClicks

          }
        }
      }

    }
    michelGUI.startup(Array("michel"))

    //If a specific object code gets to large for one file, move stuff to a new trait and extend original object with the trait.

    //object equality
    //== is not the same as equals (in java), in scala (eq) .. In scala == check value equality, but can also be used as type equality .
    //You can create you own equals method (by default it uses the one from Any).
    //1) Use the correct signature, others containers using contains will fail (so with Any type as parameter
    //2) Change hashcode, otherwise strange stuff will happen sometimes. These are buckets used by Scala.
    //3 make sure hascode and equals do not depend on mutable var's (so class Point (var x: Int, var Y: Int) will not work because the point point.x += 1 can make it fall outside the bucket.
    //4 ) make sure is symmetric/ transidient etc (so x = p , p = y , but x != y). This happens with extended classes (like a colored point). So you
    class Point(val x : Int,val y : Int)    {

      /*wrong: def equals(other : Point): Boolean = {
        if (this.x == other.x && this.y == other.y)
          true
        else
          false
      }*/
      override def hashCode = 41 * (41 + x) + y
      def canEqual (other: Any) = other.isInstanceOf[Point]
      override def equals(other : Any) = other match {
        case that: Point => (that canEqual this) && (this.x == that.x) && (this.y == that.y)
        case _ => false
      }
      override def toString()="Point " + x + " and " + y
    }
    class ColorPoint(override val x : Int,override val y : Int,val color: Color.Value) extends Point(x,y) {
      override def hashCode = 41 * super.hashCode + color.hashCode()
      override def canEqual (other: Any) = other.isInstanceOf[ColorPoint]
      override def equals(other : Any) = other match {
        case that : ColorPoint => (that canEqual this) && (super.equals(that)) && this.color == that.color
        case _ => false
      }
      override def toString()="Color Point" + x + "," + y
  }



    //Error handling
    //error handling. create when using function currying because you can make sure all code in a block has its resourced closed or deleted afterwards. But not used in this example
    try {
      val f = new FileReader("koekoek")
    } catch {
      case ex: FileNotFoundException => println("file not found")
    } finally {
      println("Do here some really necessary steps like closing files etc")
    }
  }

}


//statefull class
class bankAccount {
  //private[this] bal is the same
  private var bal : Double = 0.00 //bal has gotten implicitely the getter method "x" and setter method bal_= , in Java you have to create getters and setters yourself

  //Instead of using the default bal getter and setter you can create your own:
  def current: Double = bal //Create getter named current can only be used within class
  def current_= (x : Double) { bal = x} //setter for current

  def balance : Double = bal

  def deposit (amount : Double) = {
    require(amount > 0.0)
    bal += amount
  }

  def withdraw (amount : Double) : Boolean= {
    if (amount > bal) false
    else {
      bal -= amount
      true
    }
  }
  def resetAccount = current = 0.0
}
