package nlp

case class Text(source: String):
  lazy val words: Vector[String] = // dela upp source i ord
    val noNonLetters = source.map(c => if c.isLetter then c else ' ')
    noNonLetters.split(' ').filter(_.nonEmpty).toVector.map(word => word.toLowerCase())

  lazy val distinct: Vector[String] = words.distinct

  lazy val wordSet: Set[String] = words.toSet

  lazy val wordsOfLength: Map[Int, Set[String]] = wordSet.groupBy(_.length)

  lazy val wordFreq: Map[String, Int] = // använd FreqMapBuilder
    FreqMapBuilder(words: _*).toMap

  def ngrams(n: Int): Vector[Vector[String]] = // använd sliding
    words.sliding(n).toVector

  lazy val bigrams: Vector[(String, String)] =
    ngrams(2).map(xs => (xs(0), xs(1)))

  lazy val followFreq: Map[String, Map[String, Int]] = //nästlad tabell
    val result = scala.collection.mutable.Map.empty[String, FreqMapBuilder]
    for (key, next) <- bigrams do
        if result.contains(key) then 
            /* på "platsen" result(key): lägg till next i frekvenstabellen */
            result(key).add(next)
        else
            /* lägg till (key -> ny frekvenstabell med next) i result*/
            result += ((key, FreqMapBuilder(next)))
    result.map(p => p._1 -> p._2.toMap).toMap

  lazy val follows: Map[String, String] =
    followFreq.map( (key, followMap) => 
      val maxByFreq: (String, Int) = followMap.maxBy(_._2)
      val mostCommonFollower: String = maxByFreq._1
      (key, mostCommonFollower) 
    )
    //eller kortare med samma resultat: (lättare eller svårare att läsa?)
    //  followFreq.map((k, v) => k -> v.maxBy(_._2)._1)

object Text:
  def fromFile(fileName: String, encoding: String = "UTF-8"): Text =
    val source = scala.io.Source.fromFile(fileName, encoding)
    val txt = try source.mkString finally source.close()
    Text(txt)
  
  def fromURL(url: String, encoding: String = "UTF-8"): Text =
    val source = scala.io.Source.fromURL(url, encoding)
    val txt = try source.mkString finally source.close()
    Text(txt)