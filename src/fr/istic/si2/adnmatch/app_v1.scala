package fr.istic.si2.adnmatch

import fr.istic.si2.scribble._
import fr.istic.si2.adnmatch._
import fr.istic.si2.adnmatch.FonctionsRExp._
import fr.istic.si2.adnmatchlib._

/**
 * Application ADNMatch version 1.
 */
object ADNMatchV1 extends App {
  val exp: String = scala.io.StdIn.readLine("Enter regular expression>")
  val obj = litRExp(exp)

  obj match {
    case None      => println("Error litRExp()")
    case Some(obj) =>
      println("Expression : " + rExpToString(obj))
      val listExp = deroule(obj);

      listExp match {
        case None          => println("Error deroule()")
        case Some(listExp) =>
          println("Example : " + listeBasesToString(listExp))
      }

  }

}