package nlp

class FreqMapBuilder:
  private val register = scala.collection.mutable.Map.empty[String, Int]
  def toMap: Map[String, Int] = register.toMap
  def add(s: String): Unit = 
    val oldVal = register.getOrElse(s, 0)
    register += ((s, oldVal + 1))

object FreqMapBuilder:
  /** Skapa ny FreqMapBuilder och räkna strängarna i xs */
  def apply(xs: String*): FreqMapBuilder = 
    val fmb = new FreqMapBuilder
    xs.foreach(fmb.add)
    fmb