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

  val e = litRExp("C(.*)A")
  val s = lireSequence()

  (e, s) match{
    case (Some(e), Some(s)) =>
      println(tousLesMatchs(e, s))
    case _ => ()
  }




  val exp: String = scala.io.StdIn.readLine("Enter regular expression>")
  val obj = litRExp(exp);

  obj match {
    case None    => println("Error litRExp")
    case Some(e) =>
      println("Expression : " + rExpToString(e))

      val seq = lireSequence()

      seq match{
        case None => "Error sequence"
        case Some(s) =>
          if(matchComplet(e, s))
            println("La séquence correspond à l'expression")
          else
            println("La séquence ne correspond pas à l'expression")
      }
  }

}