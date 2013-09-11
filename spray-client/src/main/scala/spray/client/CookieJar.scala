package spray.client

import spray.http.HttpHeaders.Host
import spray.http.Uri
import scala.annotation.tailrec
import spray.http.DateTime
import spray.http.HttpCookie

/**
 * This is a *basic* CookieJar implementation. It will happily accept cookies with invalid characters
 * accept cookies without domain (which will happily be served to *any* site), accept cookies for
 * top level domains, and cookies for invalid domains. As such, it is not suitable for general use
 * where privacy matters are important, and not suitable for use where you may expect "bad" cookies.
 *
 *  Also, cookie paths and ports are completely ignored.
 *  Emptor caveat.
 */
class CookieJar {

  var jar: CookieJar_ = CookieJar_("", Map.empty, Set.empty)
  def cookiesfor(domain: String) = jar.cookiesfor(domain)
  def setCookie(cookie: HttpCookie, domain: String) = {
    println("setting cookie " + cookie)
    jar = jar.setCookie(cookie, domain)
  }

  case class CookieJar_(domainElement: String, subdomains: Map[String, CookieJar_], cookies: Set[HttpCookie]) {
    def cookiesfor(domain: String) = {
      val domainelements = domain.split('.').toList.reverse
      _getCookies(domainelements, Set.empty)
    }

    @tailrec
    private def _getCookies(domain: List[String], accum: Set[HttpCookie]): Set[HttpCookie] = {
      val now = DateTime.now
      val totalCookies = accum ++ removeStale(cookies, now)
      domain match {
        case Nil ⇒ totalCookies
        case head :: tail ⇒ {
          subdomains.get(head) match {
            case None      ⇒ totalCookies
            case Some(jar) ⇒ jar._getCookies(tail, totalCookies)
          }
        }
      }
    }

    def cookieString(domain: String) = {
      cookiesfor(domain).mkString("; ")
    }

    def setCookie(cookie: HttpCookie, domain: String) = {
      println("setting cookie")
      val trimmed = if (domain.indexOf('.') == 0) domain.substring(1) else domain
      val domainelements = trimmed.split('.').toList.reverse
      _setCookie(domainelements, cookie)
    }

    private def _setCookie(domain: List[String], cookie: HttpCookie): CookieJar_ = {
      val now = DateTime.now
      domain match {
        case Nil ⇒ {
          val newcookies = removeStale(cookies, now).filterNot(c ⇒ c.name == cookie.name) + cookie
          this.copy(cookies = newcookies)
        }
        case head :: tail ⇒ {
          lazy val newsubjar = CookieJar_(head, Map.empty, Set.empty)
          val subjar = subdomains.getOrElse(head, newsubjar)
          this.copy(subdomains = subdomains + (head -> subjar._setCookie(tail, cookie)))
        }
      }

    }

    def removeStale(cookies: Set[HttpCookie], cutoff: DateTime) = {
      cookies.filter(c ⇒ {
        c.expires match {
          case None                                ⇒ true
          case Some(datetime) if datetime > cutoff ⇒ true
          case _                                   ⇒ false
        }
      })
    }
  }

}