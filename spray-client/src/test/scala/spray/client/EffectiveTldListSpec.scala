package spray.client

import org.specs2.mutable.Specification

class EffectiveTldListSpec extends Specification {

  val list = EffectiveTldList

  val toplevels = List("ac", "ae", "aero")
  val allowedToplevels = List("henk", "paul", "johan")
  val nonlatintoplevels = List("বাংলা", "中国")
  val allowedSecondLevel = List("om.ws", "ccom.ws", "johan.aero")
  val disallowedSecondLevel = List("com.ws", "int.vn", "info.ve")
  val disallowedByWildcard = List("something.ye", "something.za", "something.zm")
  val allowedByException = List("congresodelalengua3.ar", "educ.ar", "gobiernoelectronico.ar")

  "The Effective TLD list" should {
    "disallow single valued top level domains" in {
      for (dom ← toplevels) {
        list.contains(dom) must beEqualTo(true)
      }
    }
    "disallow single valued non-latin top level domains" in {
      for (dom ← nonlatintoplevels) {
        list.contains(dom) must beEqualTo(true)
      }
    }
    "allow non-listed top level domains" in {
      for (dom ← allowedToplevels) {
        list.contains(dom) must beEqualTo(false)
      }
    }
    "allow subdomains of listed toplevel domains" in {
      for (dom ← allowedSecondLevel) {
        list.contains(dom) must beEqualTo(false)
      }
    }
    "disallow listed two level domains" in {
      for (dom ← disallowedSecondLevel) {
        list.contains(dom) must beEqualTo(true)
      }
    }
    "disallow domains listed by wildcard" in {
      for (dom ← disallowedByWildcard) {
        list.contains(dom) must beEqualTo(true)
      }
    }
    "allow subdomains under wildcards" in {
      for (dom ← disallowedByWildcard) {
        list.contains("subdomain." + dom) must beEqualTo(false)
      }
    }
    "allow subdomains of wildcards explicitly excempted" in {
      for (dom ← allowedByException) {
        list.contains(dom) must beEqualTo(false)
      }
    }

  }

}