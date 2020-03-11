package fr.istic.si2.adnmatch

import fr.istic.si2.scribble._
import fr.istic.si2.adnmatch._
import fr.istic.si2.adnmatch.FonctionsRExp._

/**
 * Type algébrique décrivant les différents marqueurs
 * indiquant les résultats de recherche.
 */
sealed trait Marqueur

case object In extends Marqueur

case object Out extends Marqueur

object RExpMatcher {
  /**
   * @param e une expression régulière
   * @return si l'expression renvoie vide
   */
  def estVide(e: RExp): Boolean = {
    e match {
      case Repete(_)      => true
      case UneBase(_)     => false
      case Impossible     => false
      case Vide           => true
      case Concat(e1, e2) => estVide(e1) && estVide(e2)
      case Choix(e1, e2)  => estVide(e1) || estVide(e2)
      case NFois(_, 0)    => true
      case NFois(e, _)    => estVide(e)
      case _              => false
    }
  }

  /**
   * @param e une expression régulière
   * @return une expression régulière simplifiée, sans % ni @
   * @note %T|@ => T
   */
  def simplifier(e: RExp): RExp = {
    e match {
      case Choix(e1, e2)  =>
        (simplifier(e1), simplifier(e2)) match {
          case (Vide, _)       => Vide
          case (_, Vide)       => Vide
          case (Impossible, e) => e
          case (e, Impossible) => e
          case (e1, e2)        => Concat(e1, e2)
        }
      case Concat(e1, e2) =>
        (simplifier(e1), simplifier(e2)) match {
          case (Impossible, _) => Impossible
          case (_, Impossible) => Impossible
          case (Vide, Vide)    => Vide
          case (Vide, e)       => e
          case (e, Vide)       => e
          case (e1, e2)        => Concat(e1, e2)
        }
      case NFois(e, n)    =>
        simplifier(e) match {
          case Vide       => Vide
          case Impossible => Impossible
          case e          => NFois(e, n)
        }
      case Repete(e)      =>
        simplifier(e) match {
          case Vide       => Vide
          case Impossible => Vide
          case e          => Repete(e)
        }
      case _              => e
    }
  }

  /**
   * @param e une expression régulière
   * @param b une base azotée
   * @return la dérivée de Brzozowski de e par rapport à b
   */
  def derivee(e: RExp, b: Base): RExp = {
    e match {
      case UneBase(e)     =>
        if (e == b)
          Vide
        else
          Impossible
      case Choix(e1, e2)  => Choix(derivee(e1, b), derivee(e2, b))
      case Concat(e1, e2) =>
        if (estVide(e1))
          Choix(Concat(derivee(e1, b), e2), derivee(e2, b))
        else
          Concat(derivee(e1, b), e2)
      case Nqb            => Vide
      case NFois(e, n)    => Concat(derivee(e, b), NFois(e, n - 1))
      case NFois(e, 1)    => derivee(e, b)
      case Repete(e)      => Concat(derivee(e, b), Repete(e))
      case Vide           => Impossible
      case Impossible     => Impossible
    }
  }

  /**
   * @param e  une expression régulière
   * @param lb une liste de bases azotées
   * @return vrai ssi la liste lb entière est décrite par e
   */
  def matchComplet(e: RExp, lb: List[Base]): Boolean = {
    lb match {
      case l :: lb => matchComplet(derivee(e, l), lb)
      case Nil     => (simplifier(e) == Vide)
    }
  }

  /**
   * @param lb une liste de bases azotées
   * @return la liste des bases de lb, dans l'ordre, marquées pour indiquer
   *         que la totalité de lb est décrite
   */
  def sequenceDecrite(lb: List[Base]): List[(Marqueur, Base)] = {
    lb match {
      case Nil    => Nil
      case n :: l => (In, n) :: sequenceDecrite(l)
    }
  }

  /**
   * @param lb une liste de bases azotées
   * @return la liste des bases de lb, dans l'ordre, marquées pour indiquer
   *         que la totalité de lb n'est pas décrite
   */
  def sequenceNonDecrite(lb: List[Base]): List[(Marqueur, Base)] = {
    lb match {
      case Nil    => Nil
      case n :: l => (In, n) :: sequenceNonDecrite(l)
    }
  }

  /**
   * @param e  une expression régulière
   * @param lb une liste de bases azotées
   * @return s'il existe, le plus petit prefixe de lb qui est décrit par e
   */
  def prefixeMatch(e: RExp, lb: List[Base]): Option[List[Base]] = {
    if(e == Vide)
      Some(Nil)
    else
      lb match {
        case Nil          => None
        case base :: list =>
          prefixeMatch(simplifier(derivee(e, base)), list) match {
            case None       => None
            case Some(list) => Some(base :: list)
          }
      }
  }

  /**
   * @param pref une liste de bases azotées *préfixe* de lb
   * @param lb   une liste de bases azotées
   * @return la sous-liste de lb située après le préfixe pref
   */
  def suppPrefixe(pref: List[Base], lb: List[Base]): List[Base] = {
    (pref, lb) match {
      case (_ :: pref, _ :: lb) => suppPrefixe(pref, lb)
      case (Nil, lb)            => lb
      case (Nil, Nil)           => Nil
    }
  }

  /**
   * @param e  une expression régulière
   * @param lb une liste de bases
   * @return une liste  (m1, base1)::...::(mN,baseN)::Nil, qui marque,
   *         base après base, les sous-listes de lb décrites par e.
   *         Les basei sont les bases de lb dans l'ordre.
   */
  def tousLesMatchs(e: RExp, lb: List[Base]): List[(Marqueur, Base)] = {
    lb match {
      case Nil          => Nil
      case base :: list =>
        prefixeMatch(e, lb) match {
          case None       => (Out, base) :: tousLesMatchs(e, list)
          case Some(pref) => sequenceDecrite(pref) ++ tousLesMatchs(e, suppPrefixe(pref, lb))
        }
    }
  }

  /**
   * @param lbm une liste de bases marquées selon un résultat de recherche
   * @return une description textuelle du résultat pour l'utilisateur
   */
  def messageResultat(lbm: List[(Marqueur, Base)]): String = {
    lbm match {
      case Nil              =>
        "Cette liste ne contient pas de sous séquence"
      case (In, _) :: list  =>
        "Cette liste contient une ou plusieurs sous séquence"
      case (Out, _) :: list =>
        messageResultat(list)
    }
  }

  /**
   * @param lb une liste de bases azotées marquées
   * @return liste des mêmes bases que lb, mais où tous les marqueurs indiquent
   *         une non-correspondance
   */
  def annulerResultat(lb: List[(Marqueur, Base)]): List[(Marqueur, Base)] = {
    lb match {
      case Nil               => Nil
      case (_, base) :: list => (Out, base) :: annulerResultat(list)
    }
  }

  /**
   * @param lbm une liste de bases azotées marquées
   * @return la liste des bases de lbm dont on a oublié les marqueurs, en conservant l'ordre
   */
  def sansMarqueurs(lbm: List[(Marqueur, Base)]): List[Base] = {
    lbm match {
      case Nil               => Nil
      case (_, base) :: list => base :: sansMarqueurs(list)
    }
  }

}