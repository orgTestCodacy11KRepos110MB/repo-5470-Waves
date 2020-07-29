package com.wavesplatform.events.repo

import com.wavesplatform.events.{BlockAppended, BlockchainUpdated, MicroBlockAppended, MicroBlockRollbackCompleted, RollbackCompleted}
import monix.reactive.Observable

import scala.util.Try

object UpdatesRepo {
  trait Read {
    def height: Int

    def updateForHeight(height: Int): Try[Option[BlockAppended]]

    // inclusive from both sides
    def updatesRange(from: Int, to: Int): Try[Seq[BlockAppended]]
  }

  trait Write {
    //  def dropLiquidState(afterId: Option[ByteStr] = None): Unit

    def appendMicroBlock(microBlockAppended: MicroBlockAppended): Try[Unit]

    def appendBlock(blockAppended: BlockAppended): Try[Unit]

    def rollback(rollback: RollbackCompleted): Try[Unit]

    def rollbackMicroBlock(microBlockRollback: MicroBlockRollbackCompleted): Try[Unit]
  }

  trait Stream {
    // inclusive
    def stream(from: Int): Observable[BlockchainUpdated]
  }
}