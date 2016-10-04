package polymorphism.themepark2

/**
 * This is a Time of Day Values collection to help reduce code duplication
 * when dealing with values that are associated with the time of day.
 *
 * @tparam A the type of data being stored.
 */
class ToDValues[A] private (private val values: Array[Option[A]]) {
  /**
   * This allows you to get a value for a particular hour. If there isn't
   * a value, it will throw an exception.
   *
   * @param hour the hour of the day to get. Should be between 0 and 23 inclusive.
   * @return the value stored for that hour.
   */
  def apply(hour: Int): A = values(hour).get

  /**
   * This allows you to get a value for a particular hour. If there isn't
   * a value, it will return None.
   *
   * @param hour the hour of the day to get. Should be between 0 and 23 inclusive.
   * @return an Option of the value stored for that hour.
   */
  def get(hour: Int): Option[A] = values(hour)

  /**
   * Allows you to set the value in a particular hour.
   *
   * @param hour the hour of the day. Should be between 0 and 23 inclusive.
   * @param v the new value to set.
   */
  def update(hour: Int, v: A) = values(hour) = Some(v)

  /**
   * Allows you to set the value in a particular hour using a String for time.
   *
   * @param hour the hour of the day. Should be between 0 and 23 inclusive.
   * @param v the new value to set.
   */
  def update(time: String, v: A) = {
    val hour = hourFromTime(time)
    values(hour) = Some(v)
  }

  /**
   * This method clears the value at a particular time.
   *
   *  @param hour the hour to clear.
   */
  def clear(hour: Int): Unit = { values(hour) = None }

  /**
   * This method clears the value at a particular time.
   *
   *  @param hour the hour to clear.
   */
  def clear(time: String): Unit = {
    val hour = hourFromTime(time)
    values(hour) = None
  }

  /**
   * Allows you to combine two sets of data using a specified function.
   *
   * @param o the other set of data.
   * @param f The function to apply to the two data types.
   */
  def combine[B, C](o: ToDValues[B])(f: (Option[A], Option[B]) => Option[C]): ToDValues[C] = {
    new ToDValues((values, o.values).zipped.map((v1, v2) => f(v1, v2)))
  }

  override def toString(): String = "ToD :\n"+
    (for ((o, i) <- values.zipWithIndex) yield i+" : "+o).mkString("\n")

  private def hourFromTime(time: String): Int = {
    time.substring(0, time.indexOf(':')).toInt +
      (if (time.endsWith("PM") && !time.startsWith("12")) 12 else 0)
  }
}

object ToDValues {
  def apply[A]() = new ToDValues[A](Array.fill(24)(None))

  def apply[A](a: A*) = {
    val d = a.map(Option(_)).toArray
    new ToDValues[A](if (d.length < 24) d.padTo(24, None) else if (d.length > 24) d.take(24) else d)
  }
}
