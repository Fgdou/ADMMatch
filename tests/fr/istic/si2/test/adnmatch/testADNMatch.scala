package fr.istic.si2.test.adnmatch

import org.junit.Test

import org.junit.Assert._
import util.Random
import fr.istic.si2.testerApp._
import fr.istic.si2.moreAssertions._
import fr.istic.si2.math._

import fr.istic.si2.adnmatch._
import fr.istic.si2.adnmatchlib._
import fr.istic.si2.adnmatch.FonctionsRExp._
import fr.istic.si2.adnmatch.RExpMatcher._

class ADNMatchTest {

  // TODO V1 V2 V3 - A compléter

  // Vous aurez besoin de réaliser les imports adéquats.
  // Si besoin, demandez de l'aide à votre encadrant.

  @Test
  def testEstVide(): Unit = {
    assertEquals(estVide(Impossible), false)
    assertEquals(estVide(Vide), true)
    assertEquals(estVide(Repete(Nqb)), true)
    assertEquals(estVide(NFois(Nqb, 1)), false)
    assertEquals(estVide(NFois(Nqb, 0)), true)
    assertEquals(estVide(Nqb), false)
    assertEquals(estVide(UneBase(A)), false)
    assertEquals(estVide(Concat(Nqb, Vide)), false)
    assertEquals(estVide(Choix(Nqb, Vide)), true)
  }

  @Test
  def testSimplifier(): Unit = {
    assertEquals(simplifier(Vide), Vide)
    assertEquals(simplifier(UneBase(A)), UneBase(A))
    assertEquals(simplifier(NFois(Vide, 8)), Vide)
    assertEquals(simplifier(Repete(Vide)), Vide)
    assertEquals(simplifier(Repete(Impossible)), Vide)
    assertEquals(simplifier(NFois(Impossible, 8)), Impossible)
    assertEquals(simplifier(Concat(Impossible, Vide)), Impossible)
    assertEquals(simplifier(Choix(Impossible, Vide)), Vide)
    assertEquals(simplifier(Concat(Nqb, Vide)), Nqb)
    assertEquals(simplifier(Choix(Nqb, Vide)), Vide)
  }

  @Test
  def testDerivee(): Unit = {
    assertEquals(simplifier(derivee(Vide, A)), Impossible)
    assertEquals(simplifier(derivee(Nqb, A)), Vide)
    assertEquals(simplifier(derivee(Repete(Nqb), A)), Repete(Nqb))
    assertEquals(simplifier(derivee(NFois(UneBase(A), 2), A)), Vide)
    assertEquals(simplifier(derivee(UneBase(A), A)), Vide)
    assertEquals(simplifier(derivee(UneBase(G), A)), Impossible)
    assertEquals(simplifier(derivee(Choix(UneBase(A), UneBase(G)), A)), Vide)
    assertEquals(simplifier(derivee(Choix(UneBase(T), UneBase(G)), A)), Impossible)
  }

}