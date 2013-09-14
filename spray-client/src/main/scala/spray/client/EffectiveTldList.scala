package spray.client

import scala.io.Source

object EffectiveTldList {
  val lines = Source.fromFile("effectivetlds.lst", "UTF-8").getLines.filterNot(_.startsWith("//")).filterNot(_.isEmpty)
  val list = build(lines)
  def contains(domain: List[String]) = list.contains(domain)
  def contains(domain: String) = list.contains(domain.split('.').reverse.toList)

  sealed abstract class Trie {
    def contains(list: List[String]): Boolean = {
      list match {
        case Nil ⇒ true
        case head :: tail ⇒ this match {
          case Leaf                ⇒ false
          case Wildcard(negations) ⇒ tail == Nil && !negations.contains(head)
          case Node(m) ⇒ m.get(head) match {
            case None           ⇒ false
            case Some(trietail) ⇒ trietail.contains(tail)
          }
        }
      }
    }
    def merge(that: Trie): Trie
  }
  case class Node(map: Map[String, Trie]) extends Trie {
    def merge(that: Trie): Trie = {
      that match {
        case Node(thatmap) ⇒ Node(mapmerge(map, thatmap, (_: Trie).merge(_: Trie)))
        case Leaf          ⇒ this
        case x: Wildcard   ⇒ throw new Exception(s"tries $x and $this not mergable")
      }
    }
  }

  case object Leaf extends Trie {
    def merge(that: Trie) = that
  }

  case class Wildcard(whitelist: Set[String]) extends Trie {
    def merge(that: Trie) = that match {
      case Leaf                    ⇒ this
      case Wildcard(thatwhitelist) ⇒ Wildcard(whitelist ++ thatwhitelist)
      case x: Node                 ⇒ throw new Exception(s"tries $x and $this not mergable")
    }
  }

  def mapmerge[T, U](left: Map[T, U], right: Map[T, U], merger: (U, U) ⇒ U) = {
    left.foldLeft(right)((nm, kvp) ⇒ {
      val present = nm.get(kvp._1)
      present match {
        case None       ⇒ nm + kvp
        case Some(val2) ⇒ nm + (kvp._1 -> merger(val2, kvp._2))
      }
    })
  }

  def totrie(elems: List[String]): Trie = {
    elems match {
      case head :: tail ⇒ {
        if (head == "*") Wildcard(Set.empty)
        else if (head.startsWith("!")) Wildcard(Set(head.substring(1)))
        else Node(Map(head -> totrie(tail)))
      }
      case Nil ⇒ Leaf
    }
  }

  def build(domains: Iterator[String]) = {
    domains.foldLeft(Leaf: Trie)((r, domain) ⇒ {
      val elems = domain.split('.').toList.reverse
      r.merge(totrie(elems))
    })
  }
}
