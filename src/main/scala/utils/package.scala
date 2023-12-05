package object utils {
  object ExtractInt {
    def unapply(s: String): Option[Int] = util.Try(s.toInt).toOption
  }
  object ExtractLong {
    def unapply(s: String): Option[Long] = util.Try(s.toLong).toOption
  }
}
