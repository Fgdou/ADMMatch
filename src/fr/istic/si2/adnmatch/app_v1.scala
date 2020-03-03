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

  println(rExpToString(Choix(UneBase(T), UneBase(A))))
  println(rExpToString(Repete(UneBase(T))))
  println(rExpToString(NFois(Nqb, 3)))
}