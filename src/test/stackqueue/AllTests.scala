package test.stackqueue

import org.junit.runner.RunWith
import org.junit.runners.Suite

@RunWith(classOf[Suite])
@Suite.SuiteClasses(Array(
  classOf[test.stackqueue.adt.TestArrayStack],
  classOf[test.stackqueue.adt.TestArrayQueue],
  classOf[test.stackqueue.util.TestRPNCalc]))
class AllTests {}