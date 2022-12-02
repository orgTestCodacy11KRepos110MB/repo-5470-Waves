package com.wavesplatform.api.http.transaction

import com.wavesplatform.account.Address
import com.wavesplatform.api.http.transaction.TransactionsApiRoute.LeaseStatus
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.InvokeScriptResult.{Lease, LeaseCancel}
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.transaction.lease.LeaseCancelTransaction
import play.api.libs.json.*
import play.api.libs.json.JsonConfiguration.Aux

private[transaction] final case class LeaseRef(
    id: ByteStr,
    originTransactionId: Option[ByteStr],
    sender: Option[Address],
    recipient: Option[Address],
    amount: Long,
    height: Option[Int],
    status: LeaseStatus,
    cancelHeight: Option[Int],
    cancelTransactionId: Option[ByteStr]
)

private[transaction] object LeaseRef {
  implicit val config: Aux[Json.MacroOptions] = JsonConfiguration(optionHandlers = OptionHandlers.WritesNull)
  implicit val byteStrWrites: Format[ByteStr] = com.wavesplatform.utils.byteStrFormat
  implicit val jsonWrites: OWrites[LeaseRef]  = Json.writes[LeaseRef]

  def fromLease(lease: Lease, blockchain: Blockchain): LeaseRef = {
    val recipient = blockchain.resolveAlias(lease.recipient).toOption
    val (height, invokeId, senderAddress, status, cancelHeight, cancelTxId) =
      blockchain
        .leaseDetails(lease.id)
        .fold {
          val senderAddress = lease.senderAddress.flatMap(b => Address.fromBytes(b.arr).toOption)
          (lease.height, lease.invokeId, senderAddress, true, Option.empty[Int], Option.empty[ByteStr])
        } { details =>
          val (status, cancelHeight, cancelTxId) = details.status match {
            case LeaseDetails.Status.Active                  => (true, None, None)
            case LeaseDetails.Status.Cancelled(height, txId) => (false, Some(height), txId)
            case LeaseDetails.Status.Expired(height)         => (false, Some(height), None)
          }
          (Some(details.height), Some(details.sourceId), Some(details.sender.toAddress), status, cancelHeight, cancelTxId)
        }
    LeaseRef(lease.id, invokeId, senderAddress, recipient, lease.amount, height, LeaseStatus(status), cancelHeight, cancelTxId)
  }

  def fromLeaseCancel(cancel: LeaseCancel, blockchain: Blockchain): LeaseRef = {
    val (height, sourceId, cancelId, amount, senderAddress, recipient) =
      blockchain
        .leaseDetails(cancel.id)
        .fold {
          val senderAddress = cancel.senderAddress.flatMap(b => Address.fromBytes(b.arr).toOption)
          val recipient     = cancel.recipient.flatMap(a => Address.fromBytes(a.arr).toOption)
          (cancel.height, cancel.sourceId, cancel.invokeId, cancel.amount.getOrElse(0L), senderAddress, recipient)
        } { details =>
          val cancelId = details.status match {
            case LeaseDetails.Status.Cancelled(_, txId) => txId
            case _                                      => None
          }
          val recipient = blockchain.resolveAlias(details.recipient).toOption
          (Some(details.height), Some(details.sourceId), cancelId, details.amount, Some(details.sender.toAddress), recipient)
        }
    LeaseRef(cancel.id, sourceId, senderAddress, recipient, amount, height, LeaseStatus(false), height, cancelId)
  }

  def fromLeaseCancelTransaction(leaseCancel: LeaseCancelTransaction, blockchain: Blockchain): LeaseRef = {
    val details   = blockchain.leaseDetails(leaseCancel.leaseId)
    val txMeta    = details.flatMap(d => blockchain.transactionMeta(d.sourceId))
    val recipient = details.flatMap(d => blockchain.resolveAlias(d.recipient).toOption)

    val (status, cancelHeight, cancelTxId) = details.map(_.status) match {
      case Some(LeaseDetails.Status.Active) | None           => (true, None, None)
      case Some(LeaseDetails.Status.Cancelled(height, txId)) => (false, Some(height), txId)
      case Some(LeaseDetails.Status.Expired(height))         => (false, Some(height), None)
    }

    LeaseRef(
      leaseCancel.leaseId,
      details.map(_.sourceId),
      details.map(_.sender.toAddress),
      recipient,
      details.map(_.amount).getOrElse(0),
      txMeta.map(_.height),
      LeaseStatus(status),
      cancelHeight,
      cancelTxId
    )
  }
}
