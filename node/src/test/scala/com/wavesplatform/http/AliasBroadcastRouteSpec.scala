package com.wavesplatform.http

import com.wavesplatform.RequestGen
import com.wavesplatform.api.common.CommonTransactionsApi
import com.wavesplatform.api.http.ApiError.*
import com.wavesplatform.api.http.RouteTimeout
import com.wavesplatform.api.http.alias.AliasApiRoute
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.TransactionDiffer.TransactionValidationError
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.utils.{Schedulers, Time}
import com.wavesplatform.wallet.Wallet
import org.scalamock.scalatest.PathMockFactory
import play.api.libs.json.*
import play.api.libs.json.Json.*

import scala.concurrent.duration.DurationInt

class AliasBroadcastRouteSpec extends RouteSpec("/alias/broadcast/") with RequestGen with PathMockFactory with RestAPISettingsHelper {
  private[this] val utxPoolSynchronizer = DummyTransactionPublisher.rejecting(tx => TransactionValidationError(GenericError("foo"), tx))

  val route = AliasApiRoute(restAPISettings, stub[CommonTransactionsApi], stub[Wallet], utxPoolSynchronizer, stub[Time], () => stub[Blockchain],
    new RouteTimeout(60.seconds)(Schedulers.fixedPool(1, "heavy-request-scheduler"))
  ).route

  "returns StateCheckFiled" - {

    def posting(url: String, v: JsValue): RouteTestResult = Post(routePath(url), v) ~> route

    "when state validation fails" in {
      forAll(createAliasGen.retryUntil(_.version == 1)) { t: Transaction =>
        posting("create", t.json()) should produce(StateCheckFailed(t, "foo"))
      }
    }
  }

  "returns appropriate error code when validation fails for" - {

    "create alias transaction" in forAll(createAliasReq) { req =>
      import com.wavesplatform.api.http.requests.SignedCreateAliasV1Request.jsonFormat

      def posting(v: JsValue): RouteTestResult = Post(routePath("create"), v) ~> route

      forAll(invalidBase58) { s =>
        posting(toJson(req.copy(senderPublicKey = s))) should produce(InvalidAddress)
      }
      forAll(nonPositiveLong) { q =>
        posting(toJson(req.copy(fee = q))) should produce(InsufficientFee)
      }
      forAll(invalidAliasStringByLength) { q =>
        val obj = toJson(req).as[JsObject] ++ Json.obj("alias" -> JsString(q))
        posting(obj) should produce(CustomValidationError(s"Alias '$q' length should be between 4 and 30"))
      }
    }
  }
}
