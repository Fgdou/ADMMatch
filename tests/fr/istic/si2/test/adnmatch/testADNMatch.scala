package fr.istic.si2.test.adnmatch

import org.junit.Test
import org.junit.Assert._

import util.Random
import fr.istic.si2.testerApp._
import fr.istic.si2.moreAssertions._
import fr.istic.si2.math._
import fr.istic.si2.adnmatch.{Marqueur, _}
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
    assertEquals("A", listeBasesToString(A::Nil))
    assertEquals("", listeBasesToString(Nil))
    assertEquals("G", listeBasesToString(G::Nil))
    assertEquals("T", listeBasesToString(T::Nil))
    assertEquals("C", listeBasesToString(C::Nil))
    assertEquals("ATGC", listeBasesToString(A::T::G::C::Nil))
  }

  @Test
  def testrExpToString: Unit ={
    assertEquals("@", rExpToString(Impossible))
    assertEquals("%", rExpToString(Vide))
    assertEquals(".", rExpToString(Nqb))
    assertEquals("(A){6}", rExpToString(NFois(UneBase(A), 6)))
    assertEquals("A(A|C)", rExpToString(Concat(UneBase(A),Choix(UneBase(A), UneBase(C)))))
    assertEquals("(A)*", rExpToString(Repete(UneBase(A))))
    assertEquals("((A|C))*", rExpToString(Repete(Choix(UneBase(A), UneBase(C)))))
  }

  @Test
  def testDeroule: Unit ={
    assertEquals(deroule(Impossible), None)
    assertEquals(Some(A::Nil), deroule(Repete(Choix(UneBase(A), UneBase(C)))))
    assertEquals(Some(List(A)), deroule(Nqb))
    assertEquals(Some(T::T::T::A::Nil), deroule(Concat(NFois(UneBase(T), 3), Choix(Nqb, Vide))))
    assertEquals(Some(Nil), deroule(Vide))
    assertEquals(Some(A::C::A::C::A::C::A::C::A::C::Nil), deroule(NFois(Concat(Choix(Nqb, UneBase(A)), UneBase(C)), 5)))
    assertEquals(Some(A::A::A::T::Nil), deroule(Choix(Concat(Choix(NFois(Nqb, 3), Impossible), UneBase(T)), Choix(Repete(Nqb), UneBase(G)))))
  }

  @Test
  def testlignes: Unit = {
    assertEquals(testreturn8, ligne(testList, 8))
    assertEquals(testreturn4, ligne(testList, 4))
    assertEquals(testreturn2, ligne(testList, 2))
  }

  @Test
  def testreturnLigne: Unit = {
    assertEquals(cutList8, returnLigne(testList, 8))
    assertEquals(cutList4, returnLigne(testList, 4))
    assertEquals(cutList2, returnLigne(testList, 2))
  }

  @Test
  def testcutLigne: Unit = {
    assertEquals(restList8, cutLigne(testList, 8))
    assertEquals(restList4, cutLigne(testList, 4))
    assertEquals(restList2, cutLigne(testList, 2))
  }

  val testList:List[(Marqueur, Base)] = List(
    (In, A),(Out, G),(In, T), (Out, C),
    (In, C),(Out, A),(In, G), (Out, T),
    (In, T),(Out, C),(In, A), (Out, G),
    (In, G),(Out, T),(In, C), (Out, A)
  )

  val cutList2:List[(Marqueur, Base)] = List(
    (In, A),
    (Out, G)
  )

  val restList2:List[(Marqueur, Base)] = List(
    (In, T), (Out, C),
    (In, C),(Out, A),(In, G), (Out, T),
    (In, T),(Out, C),(In, A), (Out, G),
    (In, G),(Out, T),(In, C), (Out, A)
  )

  val testreturn2:List[List[(Marqueur, Base)]] =
    ((In, T)::(Out, C)::Nil)::((In, A)::(Out, G)::Nil)::
    ((In, C)::(Out, A)::Nil)::((In, G)::(Out, T)::Nil)::
    ((In, T)::(Out, C)::Nil)::((In, A)::(Out, G)::Nil)::
    ((In, G)::(Out, T)::Nil)::((In, C)::(Out, A)::Nil)::Nil

  val cutList4:List[(Marqueur, Base)] = List(
    (In, A),(Out, G),(In, T), (Out, C)
  )

  val restList4:List[(Marqueur, Base)] = List(
    (In, C),(Out, A),(In, G), (Out, T),
    (In, T),(Out, C),(In, A), (Out, G),
    (In, G),(Out, T),(In, C), (Out, A)
  )

  val testreturn4:List[List[(Marqueur, Base)]] =
    ((In, A)::(Out, G)::(In, T)::(Out, C)::Nil)::
    ((In, C)::(Out, A)::(In, G)::(Out, T)::Nil)::
    ((In, T)::(Out, C)::(In, A)::(Out, G)::Nil)::
    ((In, G)::(Out, T)::(In, C)::(Out, A)::Nil)::Nil

  val cutList8:List[(Marqueur, Base)] = List(
    (In, A),(Out, G),(In, T), (Out, C),
    (In, C),(Out, A),(In, G), (Out, T)
  )

  val restList8:List[(Marqueur, Base)] = List(
    (In, T),(Out, C),(In, A), (Out, G),
    (In, G),(Out, T),(In, C), (Out, A)
  )

  val testreturn8:List[List[(Marqueur, Base)]] =
    ((In, A)::(Out, G)::(In, T)::(Out, C)::
    (In, C)::(Out, A)::(In, G)::(Out, T)::Nil)::
    ((In, T)::(Out, C)::(In, A)::(Out, G)::
    (In, G)::(Out, T)::(In, C)::(Out, A)::Nil)::Nil

}