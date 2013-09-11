/*
 * Copyright © 2011-2013 the spray project <http://spray.io>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package spray.client

import com.typesafe.config.ConfigFactory
import scala.util.Random
import scala.concurrent.Future
import scala.concurrent.duration._
import org.specs2.mutable.Specification
import org.specs2.time.NoTimeConversions
import akka.pattern.ask
import akka.util.Timeout
import akka.io.IO
import akka.actor._
import spray.httpx.encoding.Gzip
import spray.can.client.HostConnectorSettings
import spray.can.Http
import spray.client.pipelining._
import spray.http._
import spray.util._
import spray.http.HttpHeaders.`Set-Cookie`

class HttpHostConnectorSpec extends Specification with NoTimeConversions {
  implicit val timeout: Timeout = 5.seconds
  val testConf = ConfigFactory.parseString("""
    akka.event-handlers = ["akka.testkit.TestEventListener"]
    akka.loglevel = WARNING
    akka.io.tcp.trace-logging = off
    spray.can.host-connector.max-retries = 2
    spray.can.host-connector.client.request-timeout = 400ms
    spray.can.client.user-agent-header = "RequestMachine"""")
  implicit val system = ActorSystem(Utils.actorSystemNameFrom(getClass), testConf)
  import system.dispatcher
  val (interface, port) = Utils.temporaryServerHostnameAndPort()

  sequential

  step {
    val testService = system.actorOf(Props(
      new Actor with ActorLogging {
        var dropNext = true
        val random = new Random(38)
        def receive = {
          case _: Http.Connected ⇒ sender ! Http.Register(self)
          case HttpRequest(_, Uri.Path("/compressedResponse"), _, _, _) ⇒
            sender ! Gzip.encode(HttpResponse(entity = "content"))
          case x: HttpRequest if x.uri.toString.startsWith("/drop1of2") && dropNext ⇒
            log.debug("Dropping " + x)
            dropNext = random.nextBoolean()
          case HttpRequest(_, Uri.Path("/setscookie"), _, _, _) ⇒
            log.debug("Setting cookie")
            val cookie = HttpCookie("cookiename", "cookievalue")
            val cookieheader = `Set-Cookie`(cookie)
            sender ! HttpResponse(entity = "content", headers = List(cookieheader))
          case x @ HttpRequest(method, uri, _, entity, _) ⇒
            log.debug("Responding to " + x)
            dropNext = random.nextBoolean()
            val mirroredHeaders = x.header[HttpHeaders.`User-Agent`].toList
            sender ! HttpResponse(entity = method + "|" + uri.path + (if (entity.isEmpty) "" else "|" + entity.asString), headers = mirroredHeaders)
          case Timedout(request)         ⇒ sender ! HttpResponse(entity = "TIMEOUT")
          case ev: Http.ConnectionClosed ⇒ log.debug("Received " + ev)
        }
      }), "handler")
    IO(Http).ask(Http.Bind(testService, interface, port))(3.seconds).await
  }

  "An HttpHostConnector with max. 4 connections and pipelining enabled" should {
    "properly deliver the result of a simple request (pipelining)" in {
      oneRequest(pipelined = true)
    }
    "properly deliver the results of 10 requests (pipelining)" in {
      hundredRequests(pipelined = true)
    }
  }
  "An HttpHostConnector with max. 4 connections and pipelining disabled" should {
    "properly deliver the result of a simple request (no pipelining)" in {
      oneRequest(pipelined = false)
    }
    "properly deliver the results of 100 requests (no pipelining)" in {
      hundredRequests(pipelined = false)
    }
  }

  "An HttpHostConnector" should {
    "retry GET requests whose sending has failed" in {
      val pipeline = newPipeline(pipelined = false)
      def send = pipeline(HttpRequest(uri = "/drop1of2"))
      val fut = send.flatMap(r1 ⇒ send.map(r2 ⇒ r1 -> r2))
      val (r1, r2) = fut.await
      r1.entity === r2.entity
    }
    "honor the pipelined strategy when retrying" in {
      val pipeline = newPipeline(pipelined = true, maxConnections = 1)
      val requests = List(
        HttpRequest(uri = "/drop1of2/a"),
        HttpRequest(uri = "/drop1of2/b"),
        HttpRequest(uri = "/drop1of2/c"))
      val future = Future.traverse(requests)(pipeline).flatMap { responses1 ⇒
        Future.traverse(requests)(pipeline).map(responses2 ⇒ responses1.zip(responses2))
      }
      future.await.map { case (a, b) ⇒ a.entity === b.entity }.reduceLeft(_ and _)
    }
    "should honor the global spray.can.client settings" in {
      val Http.HostConnectorInfo(connector, _) = IO(Http).ask(Http.HostConnectorSetup(interface, port)).await
      val pipeline = sendReceive(connector)
      pipeline(HttpRequest()).await.header[HttpHeaders.`User-Agent`].get.value === "RequestMachine"
    }
    "store the cookies the host sets" in {
      val jar = new CookieJar
      val pipeline = newPipeline(pipelined = false, cookiejar = jar)
      val request = HttpRequest(uri = "/setscookie")
      val response = pipeline(request).await
      !jar.cookiesfor("").isEmpty
    }
  }

  "Shutdown" should {
    "perform cleanly" in {
      //      val probe = TestProbe()
      //      probe.send(IO(Http), Http.CloseAll)
      //      probe.expectMsg(5.seconds, Http.ClosedAll)
      success
    }
  }

  step(system.shutdown())

  def newPipeline(pipelined: Boolean, maxConnections: Int = 4, cookiejar: CookieJar = new CookieJar) = {
    implicit val cookiejar_ = cookiejar
    val settings = HostConnectorSettings(system).copy(maxConnections = maxConnections, pipelining = pipelined)
    val Http.HostConnectorInfo(connector, _) = IO(Http).ask(Http.HostConnectorSetup(interface, port, false, Nil, Some(settings))).await
    cookiedSendReceive(connector, "")
  }

  def oneRequest(pipelined: Boolean) = {
    val pipeline = newPipeline(pipelined)
    pipeline(HttpRequest()).await.copy(headers = Nil) === HttpResponse(200, "GET|/")
  }

  def hundredRequests(pipelined: Boolean) = {
    val pipeline = newPipeline(pipelined)
    val requests = Seq.tabulate(10)(index ⇒ HttpRequest(uri = "/" + index))
    val responseFutures = requests.map(pipeline)
    responseFutures.zipWithIndex.map {
      case (future, index) ⇒
        future.await.copy(headers = Nil) === HttpResponse(200, "GET|/" + index)
    }.reduceLeft(_ and _)
  }

}