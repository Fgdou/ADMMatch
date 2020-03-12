package fr.istic.si2.test.adnmatch

import org.junit.Test
import org.junit.Assert._

import util.Random
import fr.istic.si2.testerApp._
import fr.istic.si2.moreAssertions._
import fr.istic.si2.math._
import fr.istic.si2.adnmatch._
import fr.istic.si2.adnmatchlib._
import fr.istic.si2.adnmatch.{Marqueur, _}
import fr.istic.si2.adnmatch.FonctionsRExp._
import fr.istic.si2.adnmatch.RExpMatcher._
import fr.istic.si2.adnmatch.SequencesImages._
import fr.istic.si2.scribble.{ABeside, Below, Beside, Color, Empty, Image, LineColor, Text, fillColor}

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

  /** ---------------------------------------------------------------------------------------------
   * Les tests JUnit et les tests que nous avons effectués dans la V2 marchent tous pour le matchComplet.
   * Or la v3 affiche toute la sequence en vert peu importe l'expression.
   * Nous ne comprenons pas.
   * ------------------------------------------------------------------------------------------------
   */
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
    assertEquals(List((Out, A)), sequenceNonDecrite(List(A)))
    // s(A, B) = (In, A),(In, G)
    assertEquals(List((Out, A), (Out, G)), sequenceNonDecrite(List(A, G)))
  }


  /*
   * END FILE rexp_match
   **********************
   */

  @Test
  def testListeBasesToString()= {
    assertEquals("A", listeBasesToString(A :: Nil))
    assertEquals("", listeBasesToString(Nil))
    assertEquals("G", listeBasesToString(G :: Nil))
    assertEquals("T", listeBasesToString(T :: Nil))
    assertEquals("C", listeBasesToString(C :: Nil))
    assertEquals("ATGC", listeBasesToString(A :: T :: G :: C :: Nil))
  }

  @Test
  def testrExpToString()= {
    //Impossible =  @
    assertEquals("@", rExpToString(Impossible))

    //Vide = %
    assertEquals("%", rExpToString(Vide))

    //Nqb = .
    assertEquals(".", rExpToString(Nqb))

    //NFois(e1, n)) = (e1){n}
    assertEquals("(A){6}", rExpToString(NFois(UneBase(A), 6)))

    // Concat(UneBase(A), Choix(UneBase(A), UneBase(C)))) = A(A|C)/
    assertEquals("A(A|C)", rExpToString(Concat(UneBase(A), Choix(UneBase(A), UneBase(C)))))

    //Repete(e1) = (e1)*
    assertEquals("(A)*", rExpToString(Repete(UneBase(A))))

    //Repete(Choix(UneBase(A), UneBase(C)))) = ((A|C))*
    assertEquals("((A|C))*", rExpToString(Repete(Choix(UneBase(A), UneBase(C)))))
  }

  @Test
  def testDeroule()= {
    //deroule(@) = None
    assertEquals(deroule(Impossible), None)

    //deroule(Choix(e1, e2)) = Some(e1 :: Nil);
    //deroule(Repete(e1)) = Some(e1 :: Nil);
    assertEquals(Some(A :: Nil), deroule(Repete(Choix(UneBase(A), UneBase(C)))))

    //deroule(.) = Some(List(A))
    assertEquals(Some(List(A)), deroule(Nqb))
    assertEquals(Some(T :: T :: T :: A :: Nil), deroule(Concat(NFois(UneBase(T), 3), Choix(Nqb, Vide))))

    //deroule(%) = Some(Nil)
    assertEquals(Some(Nil), deroule(Vide))
    assertEquals(Some(A :: C :: A :: C :: A :: C :: A :: C :: A :: C :: Nil), deroule(NFois(Concat(Choix(Nqb, UneBase(A)), UneBase(C)), 5)))
    assertEquals(Some(A :: A :: A :: T :: Nil), deroule(Choix(Concat(Choix(NFois(Nqb, 3), Impossible), UneBase(T)), Choix(Repete(Nqb), UneBase(G)))))
  }

  //.....................................Values used for the lignes function............................
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
  //......................................................................................................

  @Test
  def testlignes(){
    assertEquals(testreturn8, lignes(testList, 8))
    assertEquals(testreturn4, lignes(testList, 4))
    assertEquals(testreturn2, lignes(testList, 2))
  }

  @Test
  def testreturnLigne(){
    assertEquals(cutList8, returnLigne(testList, 8))
    assertEquals(cutList4, returnLigne(testList, 4))
    assertEquals(cutList2, returnLigne(testList, 2))
  }

  @Test
  def testcutLigne(){
    assertEquals(restList8, cutLigne(testList, 8))
    assertEquals(restList4, cutLigne(testList, 4))
    assertEquals(restList2, cutLigne(testList, 2))
  }

  //..........................Values Used for the image tets.........................................
  val outG: Image = LineColor(fillColor(Text("G", 14), Color(100, 0, 0, 255)), Color(100, 0, 0, 255))
  val outT: Image = LineColor(fillColor(Text("T", 14), Color(100, 0, 0, 255)), Color(100, 0, 0, 255))
  val outC: Image = LineColor(fillColor(Text("C", 14), Color(100, 0, 0, 255)), Color(100, 0, 0, 255))
  val outA: Image = LineColor(fillColor(Text("A", 14), Color(100, 0, 0, 255)), Color(100, 0, 0, 255))

  val inG: Image = LineColor(fillColor(Text("G", 14), Color(0, 100, 0, 255)), Color(0, 100, 0, 255))
  val inT: Image = LineColor(fillColor(Text("T", 14), Color(0, 100, 0, 255)), Color(0, 100, 0, 255))
  val inC: Image = LineColor(fillColor(Text("C", 14), Color(0, 100, 0, 255)), Color(0, 100, 0, 255))
  val inA: Image = LineColor(fillColor(Text("A", 14), Color(0, 100, 0, 255)), Color(0, 100, 0, 255))

  val empty: Image = Empty
  //......................................................................................................


  @Test
  def testmarqueurBaseToImage(){
    assertEquals(inG, marqueurBaseToImage((In, G)))
    assertEquals(outG, marqueurBaseToImage((Out,G)))
    assertEquals(inA, marqueurBaseToImage((In, A)))
    assertEquals(outA, marqueurBaseToImage((Out, A)))
    assertEquals(inT, marqueurBaseToImage((In, T)))
    assertEquals(outT, marqueurBaseToImage((Out, T)))
    assertEquals(inC, marqueurBaseToImage((In, C)))
    assertEquals(outC, marqueurBaseToImage((Out, C)))
  }

  //.................................Values used for my imagesurligne........................................
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
  //......................................................................................................

  @Test
  def testimageUneLigne(){
    assertEquals(imgLs1, imageUneLigne(imglist1))
    assertEquals(imgLs2, imageUneLigne(imglist2))
    assertEquals(imgLs3, imageUneLigne(imglist3))
    assertEquals(imgLs4, imageUneLigne(imglist4))
    assertEquals(imgLs5, imageUneLigne(imglist5))
    assertEquals(imgLs6, imageUneLigne(imglist6))
  }

  //.......................................values used for imageplsligne...................................
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
  //......................................................................................................

  @Test
  def testimagePlusieursLignes(){
    assertEquals(imgpLs1,imagePlusieursLignes(imgplist1))
    assertEquals(imgpLs2,imagePlusieursLignes(imgplist2))
    assertEquals(imgpLs3,imagePlusieursLignes(imgplist3))
    assertEquals(imgpLs4,imagePlusieursLignes(imgplist4))
  }


  @Test
  def testPrefixMatch(){
    // p(@,  ) = None
    assertEquals(None, prefixeMatch(Impossible, List()))
    // p(@, A) = None
    assertEquals(None, prefixeMatch(Impossible, List(A)))
    // p(%,  ) = None
    assertEquals(None, prefixeMatch(Vide, List()))
    // p(%, A) = None
    assertEquals(None, prefixeMatch(Vide, List(A)))
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
  def testSuppPrefixe(){
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
  def testTousLesMatch(){
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
    // m(A*, A) = (In, A)
    assertEquals(List((In, A)), tousLesMatchs(Repete(UneBase(A)), List(A)))
    // m(A*, G) = (Out, G)
    assertEquals(List((Out, G)), tousLesMatchs(Repete(UneBase(A)), List(G)))
    // m(A*, AG) = (In, A)(Out, G)
    assertEquals(List((In, A), (Out, G)), tousLesMatchs(Repete(UneBase(A)), List(A, G)))
    // m(A, ATAT) = (In, A)(Out, T)(In, A)(Out, T)
    assertEquals(List((In, A),(Out, T),(In, A),(Out, T)), tousLesMatchs(UneBase(A), List(A,T,A,T)))
  }

  @Test
  def testMessageResultat(){
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
  def testAnnulerResultat(){
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
  def TestSansMarqueurs(){
    // s() = ()
    assertEquals(List(), sansMarqueurs(List()))
    // s((Out, A)) = A
    assertEquals(List(A), sansMarqueurs(List((Out, A))))
    // s((In, A)) = A
    assertEquals(List(A), sansMarqueurs(List((In, A))))
    // s((In, A)(Out, A)) = AA
    assertEquals(List(A, A), sansMarqueurs(List((In, A), (Out, A))))
  }

}