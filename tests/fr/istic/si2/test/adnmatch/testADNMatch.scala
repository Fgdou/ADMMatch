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
    assertEquals(rExpToString(Concat(UneBase(A),Choix(UneBase(A), UneBase(C)))), "A(A|C)")
    assertEquals(rExpToString(Repete(UneBase(A))),"(A)*")
    assertEquals(rExpToString(Repete(Choix(UneBase(A), UneBase(C)))),"((A|C))*")
  }

  def testDeroule: Unit ={
    assertEquals(deroule(Impossible), None)
    assertEquals(deroule(Repete(Choix(UneBase(A), UneBase(C)))), Some(A::Nil))
    assertEquals(deroule(Nqb), Some(List(A)))
    assertEquals(deroule(Concat(NFois(UneBase(T), 3), Choix(Nqb, Vide))), Some(T::T::T::A::Nil))
    assertEquals(deroule(Vide), Some(Nil))
    assertEquals(deroule(NFois(Concat(UneBase(A), UneBase(C)), 5)), Some(A::C::A::C::A::C::A::C::A::C::Nil))

  }
}