package fr.istic.si2.adnmatch

import fr.istic.si2.scribble._
import fr.istic.si2.adnmatch._

/**
 * Type algébrique modélisant
 * les bases azotées composant les séquences ADN.
 */
sealed trait Base

case object A extends Base

case object T extends Base

case object G extends Base

case object C extends Base

/**
 * Type algébrique récursif modélisant les
 * expressions régulières sur les bases azotées.
 */
sealed trait RExp

case object Impossible extends RExp

case object Vide extends RExp

case object Nqb extends RExp

case class UneBase(b: Base) extends RExp

case class Choix(e1: RExp, e2: RExp) extends RExp

case class Concat(e1: RExp, e2: RExp) extends RExp

case class Repete(e: RExp) extends RExp

case class NFois(e: RExp, n: Int) extends RExp

object FonctionsRExp {

  /**
   * @param lb une liste de bases azotées
   * @return une chaîne de caractères représentant les bases de lb, dans l'ordre
   */
  def listeBasesToString(lb: List[Base]): String = {
    lb match {
      case Nil    => ""
      case A :: l => "A" + listeBasesToString(l)
      case T :: l => "T" + listeBasesToString(l)
      case G :: l => "G" + listeBasesToString(l)
      case C :: l => "C" + listeBasesToString(l)
    }
  }
̊
  /**
   * @param e une expression régulière
   * @return la représentation textuelle de e, avec toutes les parenthèses nécessaires
   */
  def rExpToString(e: RExp): String = {
    e match {
      case UneBase(A)     => "A"
      case UneBase(T)     => "T"
      case UneBase(G)     => "G"
      case UneBase(C)     => "C"
      case Vide           => "%"
      case Nqb            => "."
      case Impossible     => "@"
      case NFois(e, n)    => "(" + rExpToString(e) + "){" + n + "}"
      case Concat(e1, e2) => rExpToString(e1) + rExpToString(e2)
      case Choix(e1, e2)  => "(" + rExpToString(e1) + "|" + rExpToString(e2) + ")"
      case Repete(e)      => "(" + rExpToString(e) + ")*"
    }
  }

  /**
   * @param e une expression régulière
   * @return une liste de bases obtenue en déroulant e tout le temps de la même manière.
   * @note Nqb        : A
   *       Repetition : 1 fois
   */
  def deroule(e: RExp): Option[List[Base]] = {
    e match {
      case Impossible     => None
      case Nqb            => Some(List(A))
      case Vide           => Some(Nil)
      case UneBase(a)     => Some(List(a))
      case Concat(e1, e2) =>
        (deroule(e1), deroule(e2)) match {
          case (Some(e1), Some(e2)) => Some(e1 ++ e2)
          case _                    => None
        }
      case Choix(e, _)    =>
        deroule(e) match {
          case Some(e) => Some(e)
          case _       => None
        }
      case Repete(e)      =>
        deroule(e) match {
          case Some(e) => Some(e)
          case _       => None
        }
      case NFois(e, 0)    =>
        deroule(e) match {
          case Some(e) => Some(Nil)
          case _       => None
        }
      case NFois(e, n)    =>
        (deroule(NFois(e, n - 1)), deroule(e)) match {
          case (Some(e1), Some(e2)) => Some(e1 ++ e2)
          case (_, _)               => None
        }
    }
  }

}