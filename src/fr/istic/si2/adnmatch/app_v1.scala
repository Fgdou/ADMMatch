package fr.istic.si2.adnmatch

import fr.istic.si2.scribble._
import fr.istic.si2.adnmatch._
import fr.istic.si2.adnmatch.FonctionsRExp._
import fr.istic.si2.adnmatchlib._

/**
 * Application ADNMatch version 1.
 */
object ADNMatchV1 extends App {

  val a: String = scala.io.StdIn.readLine(">")
  val exp: Option[RExp] = litRExp(a)

  exp match{
    case None => println("Error LitRExp")
    case Some(seq) => {
      println(rExpToString(seq))
      val seqlist: Option[List[Base]] = deroule(seq)
      seqlist match{
        case None => println("Error deroule")
        case Some(seqbase) => println(listeBasesToString(seqbase))
      }

    }
  }
}