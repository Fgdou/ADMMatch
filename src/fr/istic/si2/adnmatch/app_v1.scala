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
  val val2 = List(A,A,C,T,T)
  val val3 = List(G,G,G,C,T)
  val seq1 = List(UneBase(A), Choix(UneBase(T), UneBase(A)))

  // A(T|A), T∗(·∗)G, et T∗(·{3})G.
  val val1 = List(A,C,G,T)
  val seq2 = List(Repete(UneBase(T)), Repete(Nqb), UneBase(G))
  val seq3 = List(Repete(UneBase(T)), NFois(Nqb, 3), UneBase(G))
}