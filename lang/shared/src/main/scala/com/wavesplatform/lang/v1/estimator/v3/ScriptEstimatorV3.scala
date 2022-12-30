package com.wavesplatform.lang.v1.estimator.v3

import cats.implicits.*
import cats.{Id, Monad}
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.estimator.v3.EstimatorContext.EvalM
import com.wavesplatform.lang.v1.estimator.v3.EstimatorContext.Lenses.*
import com.wavesplatform.lang.v1.estimator.{EstimationError, ScriptEstimator}
import com.wavesplatform.lang.v1.task.imports.*
import monix.eval.Coeval

import scala.util.Try

case class ScriptEstimatorV3(fixOverflow: Boolean, overhead: Boolean, letFixes: Boolean) extends ScriptEstimator {
  private val overheadCost: Long = if (overhead) 1 else 0

  override val version: Int = 3

  override def apply(
      vars: Set[String],
      funcs: Map[FunctionHeader, Coeval[Long]],
      expr: EXPR
  ): Either[String, Long] = {
    val ctxFuncs = funcs.view.mapValues((_, Set[String]())).toMap
    evalExpr(expr, Nil).run(EstimatorContext(ctxFuncs)).value._2
  }

  private def evalExpr(t: EXPR, funcArgs: List[String]): EvalM[Long] =
    if (Thread.currentThread().isInterrupted)
      raiseError("Script estimation was interrupted")
    else
      t match {
        case LET_BLOCK(let, inner)       => evalLetBlock(let, inner, funcArgs)
        case BLOCK(let: LET, inner)      => evalLetBlock(let, inner, funcArgs)
        case BLOCK(f: FUNC, inner)       => evalFuncBlock(f, inner, funcArgs)
        case BLOCK(_: FAILED_DEC, _)     => const(0)
        case REF(str)                    => evalRef(str, funcArgs)
        case _: EVALUATED                => const(overheadCost)
        case IF(cond, t1, t2)            => evalIF(cond, t1, t2, funcArgs)
        case GETTER(expr, _)             => evalGetter(expr, funcArgs)
        case FUNCTION_CALL(header, args) => evalFuncCall(header, args, funcArgs)
        case _: FAILED_EXPR              => const(0)
      }

  private def evalHoldingFuncs(expr: EXPR, funcArgs: List[String]): EvalM[Long] =
    for {
      startCtx <- get[Id, EstimatorContext, EstimationError]
      cost     <- evalExpr(expr, funcArgs)
      _        <- update(funcs.set(_)(startCtx.funcs))
    } yield cost

  private def evalLetBlock(let: LET, inner: EXPR, funcArgs: List[String]): EvalM[Long] =
    for {
      startCtx <- get[Id, EstimatorContext, EstimationError]
      overlap = startCtx.usedRefs.contains(let.name)
      _ <- update(usedRefs.modify(_)(_ - let.name))
      letEval = evalHoldingFuncs(let.value, funcArgs)
      nextCost <- evalExpr(inner, funcArgs)
      ctx      <- get[Id, EstimatorContext, EstimationError]
      letCost  <- if (ctx.usedRefs.contains(let.name)) letEval else const(0L)
      _        <- update(usedRefs.modify(_)(r => if (overlap) r + let.name else r - let.name))
      result   <- sum(nextCost, letCost)
    } yield result

  private def evalFuncBlock(func: FUNC, inner: EXPR, funcArgs: List[String]): EvalM[Long] =
    for {
      startCtx <- get[Id, EstimatorContext, EstimationError]
      _ <-
        if (fixOverflow && startCtx.funcs.contains(FunctionHeader.User(func.name)))
          raiseError(s"Function '${func.name}${func.args.mkString("(", ", ", ")")}' shadows preceding declaration"): EvalM[Long]
        else const(0L)
      funcCost    <- evalHoldingFuncs(func.body, funcArgs ++ func.args)
      bodyEvalCtx <- get[Id, EstimatorContext, EstimationError]
      usedRefsInBody = bodyEvalCtx.usedRefs diff startCtx.usedRefs
      _ <- update(
        (funcs ~ usedRefs).modify(_) { case (funcs, _) =>
          (
            funcs + ((FunctionHeader.User(func.name), (Coeval.now(funcCost), usedRefsInBody))),
            startCtx.usedRefs
          )
        }
      )
      nextCost <- evalExpr(inner, funcArgs)
    } yield nextCost

  private def evalIF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR, funcArgs: List[String]): EvalM[Long] =
    for {
      cond  <- evalHoldingFuncs(cond, funcArgs)
      right <- evalHoldingFuncs(ifTrue, funcArgs)
      left  <- evalHoldingFuncs(ifFalse, funcArgs)
      r1    <- sum(cond, Math.max(right, left))
      r2    <- sum(r1, overheadCost)
    } yield r2

  private def evalRef(key: String, funcArgs: List[String]): EvalM[Long] =
    if (funcArgs.contains(key) && letFixes)
      const(overheadCost)
    else
      update(usedRefs.modify(_)(_ + key)).map(_ => overheadCost)

  private def evalGetter(expr: EXPR, funcArgs: List[String]): EvalM[Long] =
    evalExpr(expr, funcArgs).flatMap(sum(_, overheadCost))

  private def evalFuncCall(header: FunctionHeader, args: List[EXPR], funcArgs: List[String]): EvalM[Long] =
    for {
      ctx <- get[Id, EstimatorContext, EstimationError]
      (bodyCost, bodyUsedRefs) <- funcs
        .get(ctx)
        .get(header)
        .map(const)
        .getOrElse(
          raiseError[Id, EstimatorContext, EstimationError, (Coeval[Long], Set[String])](s"function '$header' not found")
        )
      _ <- update(
        (funcs ~ usedRefs).modify(_) { case (funcs, usedRefs) =>
          (
            funcs + ((header, (bodyCost, Set[String]()))),
            usedRefs ++ bodyUsedRefs
          )
        }
      )
      argsCosts    <- args.traverse(evalHoldingFuncs(_, funcArgs))
      argsCostsSum <- argsCosts.foldM(0L)(sum)
      bodyCostV         = bodyCost.value()
      correctedBodyCost = if (!overhead && bodyCostV == 0) 1 else bodyCostV
      result <- sum(argsCostsSum, correctedBodyCost)
    } yield result

  private def update(f: EstimatorContext => EstimatorContext): EvalM[Unit] =
    modify[Id, EstimatorContext, EstimationError](f)

  private def const[A](a: A): EvalM[A] =
    Monad[EvalM].pure(a)

  private def sum(a: Long, b: Long): EvalM[Long] = {
    def r = if (fixOverflow) Math.addExact(a, b) else a + b
    liftEither(Try(r).toEither.leftMap(_ => "Illegal script"))
  }
}
