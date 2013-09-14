package spray.client

import org.specs2.mutable.Specification
import spray.http.HttpCookie
import spray.http.Uri

class CookieJarSpec extends Specification {
  "A cookie jar" should {
    "retrieve a single set cookie" in {
      val jar = new CookieJar
      jar.setCookie(HttpCookie("myname", "myvalue"), Uri("https://www.example.org/"))
      val result = jar.cookiesfor(Uri("https://www.example.org/"))
      result match {
        case head :: tail if tail == Nil ⇒ s"$head should have name 'myname' and value 'myvalue'" ! (head.name == "myname" && head.content == "myvalue")
        case _                           ⇒ s"$result should contain a single cookie" ! false
      }
    }

    "allow retrieving cookies set for a parent domain" in {
      val jar = new CookieJar
      val set = jar.setCookie(HttpCookie("myname", "myvalue", None, None, Some("example.org")), Uri("https://www.example.org/"))
      "the cookies should be accepted" ! set == true
      val result = jar.cookiesfor(Uri("https://www.example.org/"))
      result match {
        case head :: tail if tail == Nil ⇒ s"$head should have name 'myname' and value 'myvalue'" ! (head.name == "myname" && head.content == "myvalue")
        case _                           ⇒ s"$result should contain a single cookie" ! false
      }
    }

    "disallow setting cookies for a sibling domain" in {
      val jar = new CookieJar
      val set = jar.setCookie(HttpCookie("myname", "myvalue", None, None, Some("www2.example.org")), Uri("https://www.example.org/"))
      !set
    }

    "disallow setting cookies for an effective TLD" in {
      val jar = new CookieJar
      val set = jar.setCookie(HttpCookie("myname", "myvalue", None, None, Some("co.uk")), Uri("https://somthing.example.co.uk/"))
      !set
    }

    "don't retrieve cookies set for a subdomain" in {
      val jar = new CookieJar
      val set = jar.setCookie(HttpCookie("myname", "myvalue", None, None, Some("subdomain.www.example.org")), Uri("https://subdomain.www.example.org/"))
      "the cookies should be accepted" ! set
      val result = jar.cookiesfor(Uri("https://www.example.org/"))
      "no cookies should be returned" ! (result.size == 0)
    }

    "retrieve cookies for matching paths" in {
      val jar = new CookieJar
      val set = jar.setCookie(HttpCookie("myname", "myvalue", None, None, None), Uri("https://www.example.org/path"))
      "the cookie should be accepted" ! set
      val result = jar.cookiesfor(Uri("https://www.example.org/path"))
      result match {
        case head :: tail if tail == Nil ⇒ s"$head should have name 'myname' and value 'myvalue'" ! (head.name == "myname" && head.content == "myvalue")
        case _                           ⇒ s"$result should contain a single cookie" ! false
      }
    }

    "retrieve cookies for subpaths of a set path" in {
      val jar = new CookieJar
      val set = jar.setCookie(HttpCookie("myname", "myvalue", None, None, None), Uri("https://www.example.org/path"))
      "the cookie should be accepted" ! set
      val result = jar.cookiesfor(Uri("https://www.example.org/path/two"))
      result match {
        case head :: tail if tail == Nil ⇒ s"$head should have name 'myname' and value 'myvalue'" ! (head.name == "myname" && head.content == "myvalue")
        case _                           ⇒ s"$result should contain a single cookie" ! false
      }
    }

    "don't retrieve cookies if the url has a less specific path than the cookie path" in {
      val jar = new CookieJar
      val set = jar.setCookie(HttpCookie("myname", "myvalue", None, None, None), Uri("https://www.example.org/path/two"))
      "the cookie should be accepted" ! set
      val result = jar.cookiesfor(Uri("https://www.example.org/path"))
      "no cookies should be returned" ! (result.size == 0)
    }

    "allow non-secure cookies to be retrieved over either http or https" in {
      val jar = new CookieJar
      jar.setCookie(HttpCookie("myname", "myvalue"), Uri("https://www.example.org/"))
      val result = jar.cookiesfor(Uri("http://www.example.org/"))
      result match {
        case head :: tail if tail == Nil ⇒ s"$head should have name 'myname' and value 'myvalue'" ! (head.name == "myname" && head.content == "myvalue")
        case _                           ⇒ s"$result should contain a single cookie" ! false
      }
      val result2 = jar.cookiesfor(Uri("https://www.example.org/"))
      result2 match {
        case head :: tail if tail == Nil ⇒ s"$head should have name 'myname' and value 'myvalue'" ! (head.name == "myname" && head.content == "myvalue")
        case _                           ⇒ s"$result should contain a single cookie" ! false
      }
    }

    "allow secure cookies to be retrieved only over https" in {
      val jar = new CookieJar
      jar.setCookie(HttpCookie("myname", "myvalue", None, None, None, None, true), Uri("https://www.example.org/"))
      val result = jar.cookiesfor(Uri("http://www.example.org/"))
      "no cookies should be returned" ! (result.size == 0)
      val result2 = jar.cookiesfor(Uri("https://www.example.org/"))
      result2 match {
        case head :: tail if tail == Nil ⇒ s"$head should have name 'myname' and value 'myvalue'" ! (head.name == "myname" && head.content == "myvalue")
        case _                           ⇒ s"$result should contain a single cookie" ! false
      }
    }
  }

}