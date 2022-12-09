package object utils {
  object Int {
    def unapply(s: String): Option[Int] = util.Try(s.toInt).toOption
  }
}
