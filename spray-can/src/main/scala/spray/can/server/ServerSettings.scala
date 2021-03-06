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

package spray.can.server

import com.typesafe.config.Config
import scala.concurrent.duration.Duration
import spray.can.parsing.ParserSettings
import spray.http.parser.HttpParser
import spray.http.HttpHeaders._
import spray.util._

case class BackpressureSettings(noAckRate: Int, readingLowWatermark: Int)

case class ServerSettings(
    serverHeader: String,
    sslEncryption: Boolean,
    pipeliningLimit: Int,
    idleTimeout: Duration,
    requestTimeout: Duration,
    timeoutTimeout: Duration,
    reapingCycle: Duration,
    statsSupport: Boolean,
    remoteAddressHeader: Boolean,
    rawRequestUriHeader: Boolean,
    transparentHeadRequests: Boolean,
    timeoutHandler: String,
    chunklessStreaming: Boolean,
    verboseErrorMessages: Boolean,
    requestChunkAggregationLimit: Int,
    responseHeaderSizeHint: Int,
    bindTimeout: Duration,
    unbindTimeout: Duration,
    registrationTimeout: Duration,
    defaultHostHeader: Host,
    backpressureSettings: Option[BackpressureSettings],
    parserSettings: ParserSettings) {

  requirePositive(idleTimeout)
  requirePositive(requestTimeout)
  requirePositive(timeoutTimeout)
  requirePositive(idleTimeout)
  require(0 <= pipeliningLimit && pipeliningLimit <= 128, "pipelining-limit must be >= 0 and <= 128")
  require(0 <= requestChunkAggregationLimit && requestChunkAggregationLimit <= Int.MaxValue,
    "request-chunk-aggregation-limit must be >= 0 and <= Int.MaxValue")
  require(0 <= responseHeaderSizeHint && responseHeaderSizeHint <= Int.MaxValue,
    "response-size-hint must be >= 0 and <= Int.MaxValue")
  requirePositive(bindTimeout)
  requirePositive(unbindTimeout)
  requirePositive(registrationTimeout)

  require(!requestTimeout.isFinite || idleTimeout > requestTimeout,
    "idle-timeout must be > request-timeout (if the latter is not 'infinite')")
}

object ServerSettings extends SettingsCompanion[ServerSettings]("spray.can.server") {
  def fromSubConfig(c: Config) = apply(
    c getString "server-header",
    c getBoolean "ssl-encryption",
    c.getString("pipelining-limit") match { case "disabled" ⇒ 0; case _ ⇒ c getInt "pipelining-limit" },
    c getDuration "idle-timeout",
    c getDuration "request-timeout",
    c getDuration "timeout-timeout",
    c getDuration "reaping-cycle",
    c getBoolean "stats-support",
    c getBoolean "remote-address-header",
    c getBoolean "raw-request-uri-header",
    c getBoolean "transparent-head-requests",
    c getString "timeout-handler",
    c getBoolean "chunkless-streaming",
    c getBoolean "verbose-error-messages",
    c getBytes "request-chunk-aggregation-limit" toInt,
    c getBytes "response-header-size-hint" toInt,
    c getDuration "bind-timeout",
    c getDuration "unbind-timeout",
    c getDuration "registration-timeout",
    defaultHostHeader =
      HttpParser.parseHeader(RawHeader("Host", c getString "default-host-header")) match {
        case Right(x: Host) ⇒ x
        case Left(error)    ⇒ sys.error(error.withSummary("Configured `default-host-header` is illegal").formatPretty)
        case Right(_)       ⇒ throw new IllegalStateException
      },
    backpressureSettings =
      if (c.getBoolean("automatic-back-pressure-handling"))
        Some(BackpressureSettings(
        c getInt "back-pressure.noack-rate",
        c getPossiblyInfiniteInt "back-pressure.reading-low-watermark"))
      else None,
    ParserSettings fromSubConfig c.getConfig("parsing"))
}
