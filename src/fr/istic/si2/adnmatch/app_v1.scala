package fr.istic.si2.adnmatch

import fr.istic.si2.scribble._
import fr.istic.si2.adnmatch._
import fr.istic.si2.adnmatch.FonctionsRExp._
import fr.istic.si2.adnmatchlib._

/**
 * Application ADNMatch version 1.
 */
object ADNMatchV1 extends App {

  println("ADNMatch Version 1")
  // A(T|A), T∗(·∗)G, et T∗(·{3})G.
  val seq1 = List(UneBase(A), Choix(UneBase(T), UneBase(A)))
  val seq2 = List(Repete(UneBase(T)), Repete(Nqb), UneBase(G))
  val seq3 = List(Repete(UneBase(T)), NFois(Nqb, 3), UneBase(G))

  //Exo 4
  /** @param lb une liste de bases azotées
   *  @return une chaîne de caractères représentant les bases de lb, dans l'ordre */
  def listeBasesToString(lb: List[Base]): String = {
    lb match{
      case Nil => ""
      case base::remlist => base++listeBasesToString(remlist)
    }
  }
}


//Exo 5