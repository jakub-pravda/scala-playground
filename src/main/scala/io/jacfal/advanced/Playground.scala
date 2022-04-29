package io.jacfal.advanced

object Email extends ((String, String) => String) {
  // the injection method
  def apply(user: String, domain: String): String = s"$user@$domain"

  // the extractor method
  def unapply(str: String): Option[(String, String)] = {
    val parts = str.split("@")
    if (parts.length == 2) {
      Some(parts(0), parts(1))
    } else {
      None
    }
  }
}

object Domain {
  def apply(parts: String*): String = parts.reverse.mkString(".")

  def unapplySeq(whole: String): Option[Seq[String]] =
    Some(whole.split("\\.").reverse)
}

object Playground extends App {
  // Extractors playground
  "jakub@pravda@tech" match {
    case Email(user, domain) => println(s"$user and $domain")
    case _ => println("not match")
  }

  "lol.ham.rofl.jaj" match {
    case Domain(o, t, th) => println("test 1")
    case Domain(o, t, th, u) => println("test 2")
    case Domain(o, _*) => println("test 3")
  }

  "jacob@true.cz" match {
    case Email("jacob", Domain("cz", "true")) => println("JACOB TRUE")
    case _ => println("FALSE")
  }
}
