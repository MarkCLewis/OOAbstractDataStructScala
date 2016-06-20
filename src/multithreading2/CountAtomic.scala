package multithreading2

import java.util.concurrent.atomic.AtomicInteger

object CountAtomic extends App {
  var cnt = new AtomicInteger(0)
  for (i <- (1 to 10000000).par) {
    cnt.incrementAndGet()
  }
  println(cnt.get)
}