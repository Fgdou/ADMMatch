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
import fr.istic.si2.adnmatch.SequencesImages._
import fr.istic.si2.scribble.{ABeside, Below, Beside, Color, Empty, Image, LineColor, Text, fillColor}

class ADNMatchTest {


  // Vous aurez besoin de réaliser les imports adéquats.
  // Si besoin, demandez de l'aide à votre encadrant.

  @Test
  def testListeBasesToString: Unit = {
    assertEquals("A", listeBasesToString(A :: Nil))
    assertEquals("", listeBasesToString(Nil))
    assertEquals("G", listeBasesToString(G :: Nil))
    assertEquals("T", listeBasesToString(T :: Nil))
    assertEquals("C", listeBasesToString(C :: Nil))
    assertEquals("ATGC", listeBasesToString(A :: T :: G :: C :: Nil))
  }

  @Test
  def testrExpToString: Unit = {
    assertEquals("@", rExpToString(Impossible))
    assertEquals("%", rExpToString(Vide))
    assertEquals(".", rExpToString(Nqb))
    assertEquals("(A){6}", rExpToString(NFois(UneBase(A), 6)))
    assertEquals("A(A|C)", rExpToString(Concat(UneBase(A), Choix(UneBase(A), UneBase(C)))))
    assertEquals("(A)*", rExpToString(Repete(UneBase(A))))
    assertEquals("((A|C))*", rExpToString(Repete(Choix(UneBase(A), UneBase(C)))))
  }

  @Test
  def testDeroule: Unit = {
    assertEquals(deroule(Impossible), None)
    assertEquals(Some(A :: Nil), deroule(Repete(Choix(UneBase(A), UneBase(C)))))
    assertEquals(Some(List(A)), deroule(Nqb))
    assertEquals(Some(T :: T :: T :: A :: Nil), deroule(Concat(NFois(UneBase(T), 3), Choix(Nqb, Vide))))
    assertEquals(Some(Nil), deroule(Vide))
    assertEquals(Some(A :: C :: A :: C :: A :: C :: A :: C :: A :: C :: Nil), deroule(NFois(Concat(Choix(Nqb, UneBase(A)), UneBase(C)), 5)))
    assertEquals(Some(A :: A :: A :: T :: Nil), deroule(Choix(Concat(Choix(NFois(Nqb, 3), Impossible), UneBase(T)), Choix(Repete(Nqb), UneBase(G)))))
  }

  val testList: List[(Marqueur, Base)] = List(
    (In, A), (Out, G), (In, T), (Out, C),
    (In, C), (Out, A), (In, G), (Out, T),
    (In, T), (Out, C), (In, A), (Out, G),
    (In, G), (Out, T), (In, C), (Out, A)
  )

  val cutList2: List[(Marqueur, Base)] = List(
    (In, A),(Out, G)
  )

  val restList2: List[(Marqueur, Base)] = List(
    (In, T), (Out, C),
    (In, C), (Out, A), (In, G), (Out, T),
    (In, T), (Out, C), (In, A), (Out, G),
    (In, G), (Out, T), (In, C), (Out, A)
  )

  val testreturn2: List[List[(Marqueur, Base)]] =
    ((In, A) :: (Out, G) :: Nil) :: ((In, T) :: (Out, C) :: Nil) ::
    ((In, C) :: (Out, A) :: Nil) :: ((In, G) :: (Out, T) :: Nil) ::
    ((In, T) :: (Out, C) :: Nil) :: ((In, A) :: (Out, G) :: Nil) ::
    ((In, G) :: (Out, T) :: Nil) :: ((In, C) :: (Out, A) :: Nil) :: Nil

  val cutList4: List[(Marqueur, Base)] = List(
    (In, A), (Out, G), (In, T), (Out, C)
  )

  val restList4: List[(Marqueur, Base)] = List(
    (In, C), (Out, A), (In, G), (Out, T),
    (In, T), (Out, C), (In, A), (Out, G),
    (In, G), (Out, T), (In, C), (Out, A)
  )

  val testreturn4: List[List[(Marqueur, Base)]] =
    ((In, A) :: (Out, G) :: (In, T) :: (Out, C) :: Nil) ::
    ((In, C) :: (Out, A) :: (In, G) :: (Out, T) :: Nil) ::
    ((In, T) :: (Out, C) :: (In, A) :: (Out, G) :: Nil) ::
    ((In, G) :: (Out, T) :: (In, C) :: (Out, A) :: Nil) :: Nil

  val cutList8: List[(Marqueur, Base)] = List(
    (In, A), (Out, G), (In, T), (Out, C),
    (In, C), (Out, A), (In, G), (Out, T)
  )

  val restList8: List[(Marqueur, Base)] = List(
    (In, T), (Out, C), (In, A), (Out, G),
    (In, G), (Out, T), (In, C), (Out, A)
  )

  val testreturn8: List[List[(Marqueur, Base)]] =
    ((In, A) :: (Out, G) :: (In, T) :: (Out, C) ::
    (In, C) :: (Out, A) :: (In, G) :: (Out, T) :: Nil) ::
    ((In, T) :: (Out, C) :: (In, A) :: (Out, G) ::
    (In, G) :: (Out, T) :: (In, C) :: (Out, A) :: Nil) :: Nil

  @Test
  def testlignes: Unit = {
    assertEquals(testreturn8, lignes(testList, 8))
    assertEquals(testreturn4, lignes(testList, 4))
    assertEquals(testreturn2, lignes(testList, 2))
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

  val outG: Image = LineColor(fillColor(Text("G", 14), Color(100, 0, 0, 255)), Color(100, 0, 0, 255))
  val outT: Image = LineColor(fillColor(Text("T", 14), Color(100, 0, 0, 255)), Color(100, 0, 0, 255))
  val outC: Image = LineColor(fillColor(Text("C", 14), Color(100, 0, 0, 255)), Color(100, 0, 0, 255))
  val outA: Image = LineColor(fillColor(Text("A", 14), Color(100, 0, 0, 255)), Color(100, 0, 0, 255))

  val inG: Image = LineColor(fillColor(Text("G", 14), Color(0, 100, 0, 255)), Color(0, 100, 0, 255))
  val inT: Image = LineColor(fillColor(Text("T", 14), Color(0, 100, 0, 255)), Color(0, 100, 0, 255))
  val inC: Image = LineColor(fillColor(Text("C", 14), Color(0, 100, 0, 255)), Color(0, 100, 0, 255))
  val inA: Image = LineColor(fillColor(Text("A", 14), Color(0, 100, 0, 255)), Color(0, 100, 0, 255))

  val empty: Image = Empty


  @Test
  def testmarqueurBaseToImage: Unit = {
    assertEquals(inG, marqueurBaseToImage((In, G)))
    assertEquals(outG, marqueurBaseToImage((Out,G)))
    assertEquals(inA, marqueurBaseToImage((In, A)))
    assertEquals(outA, marqueurBaseToImage((Out, A)))
    assertEquals(inT, marqueurBaseToImage((In, T)))
    assertEquals(outT, marqueurBaseToImage((Out, T)))
    assertEquals(inC, marqueurBaseToImage((In, C)))
    assertEquals(outC, marqueurBaseToImage((Out, C)))
  }

  val imglist1: List[(Marqueur, Base)] = List(
    (In, A),(Out, G),(In, T),(Out, C)
  )

  val imgLs1: Image = {
    Beside(inA, Beside(outG, Beside(inT, Beside(outC, empty))))
  }

  val imglist2: List[(Marqueur, Base)] = List(
    (In, A),(In, G),(In, T),(In, C)
  )

  val imgLs2: Image = {
    Beside(inA, Beside(inG, Beside(inT, Beside(inC, empty))))
  }

  val imglist3: List[(Marqueur, Base)] = List(
    (Out, A),(Out, G),(Out, T),(Out, C)
  )

  val imgLs3: Image = {
    Beside(outA, Beside(outG, Beside(outT, Beside(outC, empty))))
  }

  val imglist4: List[(Marqueur, Base)] = List(
    (In, T),(Out, C),(In, A),(Out, G)
  )

  val imgLs4: Image = {
    Beside(inT, Beside(outC, Beside(inA, Beside(outG, empty))))
  }

  val imglist5: List[(Marqueur, Base)] = List(
    (Out, A),(In, G),(Out, T),(In, C)
  )

  val imgLs5: Image = {
    Beside(outA, Beside(inG, Beside(outT, Beside(inC, empty))))
  }

  val imglist6: List[(Marqueur, Base)] = List(
    (In, A),(Out, G),(In, A),(Out, G)
  )

  val imgLs6: Image = {
    Beside(inA, Beside(outG, Beside(inA, Beside(outG, empty))))
  }

  @Test
  def testimageUneLigne: Unit = {
    assertEquals(imgLs1, imageUneLigne(imglist1))
    assertEquals(imgLs2, imageUneLigne(imglist2))
    assertEquals(imgLs3, imageUneLigne(imglist3))
    assertEquals(imgLs4, imageUneLigne(imglist4))
    assertEquals(imgLs5, imageUneLigne(imglist5))
    assertEquals(imgLs6, imageUneLigne(imglist6))
  }

  val imgplist1: List[List[(Marqueur, Base)]]= List(
    List((In, A),(Out, G),(In, A),(Out, G)), List((Out, A),(In, G),(Out, T),(In, C)), List((In, T),(Out, C),(In, A),(Out, G))
  )

  val imgpLs1: Image = {
    Below(imgLs6, Below(imgLs5, Below(imgLs4, empty)))
  }

  val imgplist2: List[List[(Marqueur, Base)]]= List(
    imglist1,imglist2,imglist3
  )

  val imgpLs2: Image = {
    Below(imgLs1, Below(imgLs2, Below(imgLs3, empty)))
  }

  val imgplist3: List[List[(Marqueur, Base)]]= List(
    imglist2,imglist3,imglist4
  )

  val imgpLs3: Image = {
    Below(imgLs2, Below(imgLs3, Below(imgLs4, empty)))
  }

  val imgplist4: List[List[(Marqueur, Base)]]= List(
    imglist4,imglist5,imglist6
  )

  val imgpLs4: Image = {
    Below(imgLs4, Below(imgLs5, Below(imgLs6, empty)))
  }

@Test
  def testimagePlusieursLignes: Unit = {
    assertEquals(imgpLs1,imagePlusieursLignes(imgplist1))
    assertEquals(imgpLs2,imagePlusieursLignes(imgplist2))
    assertEquals(imgpLs3,imagePlusieursLignes(imgplist3))
    assertEquals(imgpLs4,imagePlusieursLignes(imgplist4))
  }


}