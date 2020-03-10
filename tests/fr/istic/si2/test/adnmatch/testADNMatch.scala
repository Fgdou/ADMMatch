package fr.istic.si2.test.adnmatch

import org.junit.Test

import org.junit.Assert._
import util.Random
import fr.istic.si2.testerApp._
import fr.istic.si2.moreAssertions._
import fr.istic.si2.math._

import fr.istic.si2.adnmatch._
import fr.istic.si2.adnmatch.FonctionsRExp._
import fr.istic.si2.adnmatch.RExpMatcher._

class ADNMatchTest {

  // TODO V1 V2 V3 - A compléter

  // Vous aurez besoin de réaliser les imports adéquats.
  // Si besoin, demandez de l'aide à votre encadrant.

  /**
   * A compléter
   */
  @Test
  def testListeBasesToString: Unit = {
    assertEquals(listeBasesToString(A::Nil), "A")
    assertEquals(listeBasesToString(Nil), "")
    assertEquals(listeBasesToString(G::Nil), "G")
    assertEquals(listeBasesToString(T::Nil), "T")
    assertEquals(listeBasesToString(C::Nil), "C")
    assertEquals(listeBasesToString(A::T::G::C::Nil), "ATGC")
  }

  def testrExpToString: Unit ={
    assertEquals(rExpToString(Impossible),"@")
    assertEquals(rExpToString(Vide),"%")
    assertEquals(rExpToString(Nqb),".")
    assertEquals(rExpToString(NFois(UneBase(A), 6)),"(A){6}")
    assertEquals(rExpToString(Concat(UneBase(A),Choix(UneBase(A), UneBase(C)))), )
    assertEquals(rExpToString(Impossible),"@")
    assertEquals(rExpToString(Impossible),"@")
    assertEquals(rExpToString(Impossible),"@")
    assertEquals(rExpToString(Impossible),"@")
    assertEquals(rExpToString(Repete(Choix(UneBase(A), UneBase(C)))),"((A|C))*")
  }
}