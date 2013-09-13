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
 *  Emptor caveat.
 */
class CookieJar {

  var jar: CookieJar_ = CookieJar_("", Map.empty, Map.empty)
  def cookiesfor(uri: Uri) = jar.cookiesfor(uri)
  def setCookie(cookie: HttpCookie, domain: String) = {
    jar = jar.setCookie(cookie, domain)
  }

  case class CookieJar_(domainElement: String, subdomains: Map[String, CookieJar_], cookies: Map[String, HttpCookie]) {
    def cookiesfor(uri: Uri) = {
      val domain = uri.authority.host.address
      val domainelements = domain.split('.').toList.reverse
      _getCookies(domainelements, uri, Map.empty).values
    }

    @tailrec
    private def _getCookies(domain: List[String], uri: Uri, accum: Map[String, HttpCookie]): Map[String, HttpCookie] = {
      val now = DateTime.now
      val newcookies = removeStale(cookies, now)
        .filter(c ⇒ uri.scheme == "https" || !c._2.secure)
        .filter(c ⇒ c._2.path match {
          case Some(path) ⇒ uri.path.startsWith(Uri.Path(path))
          case None       ⇒ true
        })
      val totalCookies = accum ++ newcookies
      domain match {
        case Nil ⇒ totalCookies
        case head :: tail ⇒ {
          subdomains.get(head) match {
            case None      ⇒ totalCookies
            case Some(jar) ⇒ jar._getCookies(tail, uri, totalCookies)
          }
        }
      }
    }

    def setCookie(cookie: HttpCookie, domain: String) = {
      val trimmed = if (domain.indexOf('.') == 0) domain.substring(1) else domain
      val domainelements = trimmed.split('.').toList.reverse
      _setCookie(domainelements, cookie)
    }

    private def _setCookie(domain: List[String], cookie: HttpCookie): CookieJar_ = {
      val now = DateTime.now
      domain match {
        case Nil ⇒ {
          val newcookies = removeStale(cookies, now) + (cookie.name -> cookie)
          this.copy(cookies = newcookies)
        }
        case head :: tail ⇒ {
          lazy val newsubjar = CookieJar_(head, Map.empty, Map.empty)
          val subjar = subdomains.getOrElse(head, newsubjar)
          this.copy(subdomains = subdomains + (head -> subjar._setCookie(tail, cookie)))
        }
      }
    }

    def removeStale(cookies: Map[String, HttpCookie], cutoff: DateTime) = {
      cookies.filter(c ⇒ {
        c._2.expires match {
          case None                                ⇒ true
          case Some(datetime) if datetime > cutoff ⇒ true
          case _                                   ⇒ false
        }
      })
    }
    
  }
}