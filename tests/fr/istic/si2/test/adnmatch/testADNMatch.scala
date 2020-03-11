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

  /* ***************
   * FILE rexp_match
   */

  @Test
  def testEstVide(): Unit = {
    // v(@) = false
    assertEquals(false, estVide(Impossible))
    // v(%) = true
    assertEquals(true, estVide(Vide))

    // v(.*) = true
    assertEquals(true, estVide(Repete(Nqb)))

    // v(.{1}) = false
    assertEquals(false, estVide(NFois(Nqb, 1)))
    // v(.{0}) = true
    assertEquals(true, estVide(NFois(Nqb, 0)))

    // v(.) = false
    assertEquals(false, estVide(Nqb))
    // v(A) = false
    assertEquals(false, estVide(UneBase(A)))
    // v(.%) = false
    assertEquals(false, estVide(Concat(Nqb, Vide)))
    // v(.|%) = true
    assertEquals(true, estVide(Choix(Nqb, Vide)))
  }

  @Test
  def testSimplifier(): Unit = {
    // s(%) = %
    assertEquals(Vide, simplifier(Vide))
    // s(A) = A
    assertEquals(UneBase(A), simplifier(UneBase(A)))
    // s(%{8}) = %
    assertEquals(Vide, simplifier(NFois(Vide, 8)))
    // s(%*) = %
    assertEquals(Vide, simplifier(Repete(Vide)))
    // s(@*) = @
    assertEquals(Vide, simplifier(Repete(Impossible)))
    // s(@{8}) = @
    assertEquals(Impossible, simplifier(NFois(Impossible, 8)))
    // s(%@) = @
    assertEquals(Impossible, simplifier(Concat(Impossible, Vide)))
    // s(@|%) = %
    assertEquals(Vide, simplifier(Choix(Impossible, Vide)))
    // s(.%) = .
    assertEquals(Nqb, simplifier(Concat(Nqb, Vide)))
    // s(.|%) = %
    assertEquals(Vide, simplifier(Choix(Nqb, Vide)))
  }

  @Test
  def testDerivee(): Unit = {
    // d_A(A) = %
    assertEquals(Vide, simplifier(derivee(UneBase(A), A)))
    // d_A(T) = @
    assertEquals(Impossible, simplifier(derivee(UneBase(T), A)))

    // d(@) = @
    assertEquals(Impossible, simplifier(derivee(Impossible, A)))
    // d(%) = @
    assertEquals(Impossible, simplifier(derivee(Vide, A)))

    // d_A(A|G) = %
    assertEquals(Vide, simplifier(derivee(Choix(UneBase(A), UneBase(G)), A)))
    // d_A(T|G) = @
    assertEquals(Impossible, simplifier(derivee(Choix(UneBase(T), UneBase(G)), A)))

    // d_A(A*) = A*
    assertEquals(Repete(UneBase(A)), simplifier(derivee(Repete(UneBase(A)), A)))
    // d_A(T*) = @
    assertEquals(Impossible, simplifier(derivee(Repete(UneBase(T)), A)))
  }

  @Test
  def testMatchComplet(): Unit = {
    // m(A, A) = true
    assertEquals(true, matchComplet(UneBase(A), List(A)))
    // m(A, G) = false
    assertEquals(false, matchComplet(UneBase(A), List(G)))
    // m(@, A) = false
    assertEquals(false, matchComplet(Impossible, List(A)))
    // m(@, ATGC) = false
    assertEquals(false, matchComplet(Impossible, List(A, T, G, C)))
    // m(%, A) = false
    assertEquals(false, matchComplet(Vide, List(A, T, G, C)))
    // m(%, ATGC) = false
    assertEquals(false, matchComplet(Vide, List(A)))
    // m(%,  ) = true
    assertEquals(true, matchComplet(Vide, List()))
    // m(., A) = true
    assertEquals(true, matchComplet(Nqb, List(A)))
    // m(.,  ) = false
    assertEquals(false, matchComplet(Nqb, List()))
    // m(.., GTA) = false
    assertEquals(false, matchComplet(Concat(Nqb, Nqb), List(G, T, A)))
    // m(A*, ) = true
    assertEquals(true, matchComplet(Repete(UneBase(A)), List()))
    // m(A*, AA) = true
    assertEquals(true, matchComplet(Repete(UneBase(A)), List(A,A)))
    // m(A*, AT) = false
    assertEquals(true, matchComplet(Repete(UneBase(A)), List(A,A)))
    // m(A*, T) = false
    assertEquals(false, matchComplet(Repete(UneBase(A)), List(T)))
    // m(A|G, A) = true
    assertEquals(true, matchComplet(Choix(UneBase(A), UneBase(G)), List(A)))
    // m(AG, A) = false
    assertEquals(false, matchComplet(Concat(UneBase(A), UneBase(G)), List(A)))
    // m(G*A, A) = true
    assertEquals(true, matchComplet(Concat(Repete(UneBase(G)), UneBase(A)), List(A)))
    // m(GTA, GTA) = true
    assertEquals(true, matchComplet(Concat(Concat(UneBase(G), UneBase(T)), UneBase(A)), List(G, T, A)))
    // m(A*CT, CT) = true
    assertEquals(true, matchComplet(Concat(Concat(Repete(UneBase(A)), UneBase(C)), UneBase(T)), List(C, T)))
  }

  @Test
  def testSequenceDecrite(): Unit = {
    // s() = ()
    assertEquals(List(), sequenceDecrite(List()))
    // s(A) = (In, A)
    assertEquals(List((In, A)), sequenceDecrite(List(A)))
    // s(A, B) = (In, A),(In, G)
    assertEquals(List((In, A), (In, G)), sequenceDecrite(List(A, G)))
  }

  @Test
  def testSequenceNonDecrite(): Unit = {
    // s() = ()
    assertEquals(List(), sequenceNonDecrite(List()))
    // s(A) = (In, A)
    assertEquals(List((In, A)), sequenceNonDecrite(List(A)))
    // s(A, B) = (In, A),(In, G)
    assertEquals(List((In, A), (In, G)), sequenceNonDecrite(List(A, G)))
  }

  @Test
  def testPrefixMatch(): Unit = {
    // p(@,  ) = None
    assertEquals(None, prefixeMatch(Impossible, List()))
    // p(@, A) = None
    assertEquals(None, prefixeMatch(Impossible, List(A)))
    // p(%,  ) = ()
    assertEquals(Some(List()), prefixeMatch(Vide, List()))
    // p(%, A) = ()
    assertEquals(Some(List()), prefixeMatch(Vide, List(A)))
    // p(A, A) = A
    assertEquals(Some(List(A)), prefixeMatch(UneBase(A), List(A)))
    // p(A, AG) = A
    assertEquals(Some(List(A)), prefixeMatch(UneBase(A), List(A, G)))
    // p(AG, AGC) = AG
    assertEquals(Some(List(A, G)), prefixeMatch(Concat(UneBase(A), UneBase(G)), List(A, G, C)))
    // p(A*T, AAATG) = AAAT
    assertEquals(Some(List(A,A,A,T)), prefixeMatch(Concat(Repete(UneBase(A)),UneBase(T)), List(A,A,A,T,G)))
  }

  @Test
  def testSuppPrefixe(): Unit = {
    // s( , ) = ()
    assertEquals(List(), suppPrefixe(List(), List()))
    // s( , A) = A
    assertEquals(List(A), suppPrefixe(List(), List(A)))
    // s(A, A) = ()
    assertEquals(List(), suppPrefixe(List(A), List(A)))
    // s(A, AT) = T
    assertEquals(List(T), suppPrefixe(List(A), List(A, T)))
  }

  @Test
  def testTousLesMatch(): Unit = {
    // m(%,  ) = ()
    assertEquals(List(), tousLesMatchs(Vide, List()))
    // m(%, A) = (Out, A)
    assertEquals(List((Out, A)), tousLesMatchs(Vide, List(A)))
    // m(@,  ) = ()
    assertEquals(List(), tousLesMatchs(Impossible, List()))
    // m(@, A) = (Out, A)
    assertEquals(List((Out, A)), tousLesMatchs(Impossible, List(A)))
    // m(., A) = (In, A)
    assertEquals(List((In, A)), tousLesMatchs(Nqb, List(A)))
    // m(A, ATAT) = (In, A)(Out, T)(In, A)(Out, T)
    assertEquals(List((In, A),(Out, T),(In, A),(Out, T)), tousLesMatchs(UneBase(A), List(A,T,A,T)))
  }

  @Test
  def testMessageResultat(): Unit = {
    val ncontient: String = "Cette liste ne contient pas de sous séquence"
    val contient: String = "Cette liste contient une ou plusieurs sous séquence"

    // m() = ne contient pas
    assertEquals(ncontient, messageResultat(List()))
    // m((Out, A)) = ne contient pas
    assertEquals(ncontient, messageResultat(List((Out, A))))
    // m((Int, A)) = contient
    assertEquals(contient, messageResultat(List((In, A))))
    // m((Out, A), (Out, A)) = ne contient pas
    assertEquals(ncontient, messageResultat(List((Out, A), (Out, A))))
    // m((Out, A), (In, A)) = contient
    assertEquals(contient, messageResultat(List((Out, A), (In, A))))
    // m((Int, A), (Int, A)) = contient
    assertEquals(contient, messageResultat(List((In, A), (In, A))))
  }

  @Test
  def testAnnulerResultat(): Unit = {
    // a() = ()
    assertEquals(List(), annulerResultat(List()))
    // a((In, A)) = (Out, A)
    assertEquals(List((Out, A)), annulerResultat(List((In, A))))
    // a((Out, A)) = (Out, A)
    assertEquals(List((Out, A)), annulerResultat(List((Out, A))))
    // a((Out, A)(In, A) = (Out, A)(Out, A)
    assertEquals(List((Out, A), (Out, A)), annulerResultat(List((Out, A), (In, A))))
  }

  @Test
  def TestSansMarqueurs(): Unit = {
    // s() = ()
    assertEquals(List(), sansMarqueurs(List()))
    // s((Out, A)) = A
    assertEquals(List(A), sansMarqueurs(List((Out, A))))
    // s((In, A)) = A
    assertEquals(List(A), sansMarqueurs(List((In, A))))
    // s((In, A)(Out, A)) = AA
    assertEquals(List(A, A), sansMarqueurs(List((In, A), (Out, A))))
  }
  /*
   * END FILE rexp_match
   **********************
   */

}