package fr.istic.si2.adnmatch

import fr.istic.si2.scribble._
import fr.istic.si2.adnmatch._
import fr.istic.si2.adnmatch.FonctionsRExp._
import fr.istic.si2.adnmatch.RExpMatcher._

object SequencesImages {

  /**
   * @param list une liste de base
   * @param n la taille d'une ligne
   * @return une liste des n premiers éléments
   * @note liste = ABCDEF et n = 3
   *       return ABC
   */
  def getLine(list: List[(Marqueur, Base)], n: Int): List[(Marqueur, Base)] = {
    list match {
      case b :: list =>
        if (n == 0)
          Nil
        else
          b :: getLine(list, n - 1)
      case Nil       => Nil
    }
  }

  /**
   * @param list une liste de base
   * @param n une taille de ligne
   * @return la liste sans les n premiers éléments
   */
  def suppLine(list: List[(Marqueur, Base)], n: Int): List[(Marqueur, Base)] = {
    list match {
      case b :: list =>
        if (n == 0)
          list
        else
          suppLine(list, n - 1)
      case Nil       => Nil
    }
  }

  /**
   * @param lmb    une liste de bases marquées
   * @param tligne entier strictement positif, représentant la taille d'une ligne en nombre de bases marquées
   * @return une liste contenant des sous-listes de lmb, toutes de taille tligne, sauf la dernière qui
   *         peut être de taille inférieure.
   */
  def lignes(lmb: List[(Marqueur, Base)], tligne: Int): List[List[(Marqueur, Base)]] = {
    lmb match {
      case Nil => Nil
      case _   => getLine(lmb, tligne) :: lignes(suppLine(lmb, tligne), tligne)
    }
  }

  /**
   * Taille du texte à utiliser pour représenter
   * graphiquement les bases azotées.
   */
  val fontSizeBase: Int = 14

  /**
   * @param mb une base azotée marquée
   * @return une image représentant la base avec son marqueur visuel
   */
  // TODO V3
  def marqueurBaseToImage(mb: (Marqueur, Base)): Image = ???

  /**
   * @param ligne une liste de bases azotées marquées
   * @return une image représentant les bases marquées de ligne, dans l'ordre, toutes sur la même ligne
   */
  // TODO V3
  def imageUneLigne(ligne: List[(Marqueur, Base)]): Image = ???

  /**
   * @param llignes une liste de listes de bases azotées marquées
   * @return une image représentant les bases marquées de llignes, dans l'ordre.
   *         Chaque élément de llignes est sur une ligne distincte.
   *         Les lignes sont visualisées les unes en dessous des autres.
   */
  // TODO V3
  def imagePlusieursLignes(llignes: List[List[(Marqueur, Base)]]): Image = ???

}