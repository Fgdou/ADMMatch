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

  val exp: String = scala.io.StdIn.readLine("Enter regular expression>")
  val obj = litRExp(exp);
  val seq = lireSequence()


  (obj, seq) match {
    case (Some(e), Some(s)) =>
      println("Expression : " + rExpToString(e))
      println("Sequence   : " + listeBasesToString(s))

      if (matchComplet(e, s))
        println("La séquence correspond à l'expression")
      else
        println("La séquence ne correspond pas à l'expression")

      println(messageResultat(tousLesMatchs(e, s)))

    case (None, _) => println("Erreur expression")
    case (_, None) => println("Erreur de sequence")
    case _         => println("ERROR")
  }

}