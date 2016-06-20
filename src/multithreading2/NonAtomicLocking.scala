package multithreading2

object NonAtomicLocking extends App {
  
  // Note that this code doesn't do what it is attempting because the checking and setting of
  // the isSafe var is not atomic.
  var isSafe = true
  var cnt = 0
  for (i <- (1 to 1000000000).par) {
    if(isSafe) {
      isSafe = false
      cnt += 1
      isSafe = true
    }
  }
  println(cnt)
}