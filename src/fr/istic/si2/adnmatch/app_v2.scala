package fr.istic.si2.adnmatch

import fr.istic.si2.scribble._
import fr.istic.si2.adnmatch._
import fr.istic.si2.adnmatch.FonctionsRExp._
import fr.istic.si2.adnmatch.RExpMatcher._
import fr.istic.si2.adnmatchlib._

/**
 * Application ADNMatch version 2
 */
object ADNMatchV2 extends App {

  println("ADNMatch Version 2")
  val str: String = scala.io.StdIn.readLine("Enter regular expression>")
  val base = litRExp(str);
  val seq = lireSequence()


  (base, seq) match {
    case (Some(e), Some(s)) =>

      println("Expression : " + rExpToString(e))
      println("Sequence   : " + listeBasesToString(s))

      if (matchComplet(e, s))
        println("The sequence is valid the expression "+ str)
      else
        println("This sequence isn't valid for the expression")

      println(messageResultat(tousLesMatchs(e, s)))

    case (None, _) => println("Wrong expression")
    case (_, None) => println("Wrong sequence")
    case _         => println("ERROR")
  }

}