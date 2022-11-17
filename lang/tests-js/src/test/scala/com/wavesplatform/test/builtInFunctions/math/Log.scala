package com.wavesplatform.test.builtInFunctions.math

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V5
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{
  randomAddressDataArrayElement,
  randomAliasDataArrayElement,
  randomStringArrayElement,
  randomUnionArrayElement,
  randomInt
}
import utest.{Tests, test}

object Log extends JsTestBase {
  var union: String          = randomUnionArrayElement
  val logInt                 = s"log(callerTestData, $randomInt, $randomInt, 4, 2, $union)"
  val logIntArgBeforeFunc    = s"callerTestData.log($randomInt, $randomInt, 4, 2, $union)"
  val logBigInt              = s"log(callerTestData, 6, callerTestData, $randomInt, 2, $union)"
  val logBigIntArgBeforeFunc = s"callerTestData.log(6, callerTestData, $randomInt, 2, $union)"

  val invalidLogInt              = s"log(callerTestData, 10, $union)"
  val invalidLogIntArgBeforeFunc = s"callerTestData.log(10, $union)"
  private val fractionError: String = testData.invalidFunctionError("log", 6)

  val tests: Tests = Tests {
    test.apply("check: log Int function compiles") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.onlyMatcherContract(randomInt.toString, logInt)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: log Int function compiles (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.onlyMatcherContract(randomInt.toString, logIntArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: log BigInt function compiles") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script = precondition.onlyMatcherContract(s"toBigInt(${randomInt.toString})", logBigInt)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: log BigInt function compiles (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script = precondition.onlyMatcherContract(s"toBigInt(${randomInt.toString})", logBigIntArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: invalid log function") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.onlyMatcherContract(randomInt.toString, invalidLogInt)
        if (version < V5) {
          assertCompileErrorDApp(script, version, fractionError)
        } else {
          assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }

    test.apply("compilation error: invalid log function (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.onlyMatcherContract(randomInt.toString, invalidLogIntArgBeforeFunc)
        if (version < V5) {
          assertCompileErrorDApp(script, version, fractionError)
        } else {
          assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }

    test.apply("compilation error: invalid log data") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.onlyMatcherContract(randomStringArrayElement, logInt)
        if (version < V5) {
          assertCompileErrorDApp(script, version, testData.nonMatchingTypes("Int"))
        } else {
          assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }

    test.apply("compilation error: invalid log data (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.onlyMatcherContract(randomAddressDataArrayElement, logIntArgBeforeFunc)
        if (version < V5) {
          assertCompileErrorDApp(script, version, testData.nonMatchingTypes("Int"))
        } else {
          assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }

    test.apply("compilation error: invalid log data BigInt") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script = precondition.onlyMatcherContract(randomStringArrayElement, logBigInt)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: invalid log data BigInt (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script = precondition.onlyMatcherContract(randomAliasDataArrayElement, logBigIntArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }
  }
}
