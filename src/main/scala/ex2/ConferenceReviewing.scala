package ex2

/**
 * For each article, the reviewer has to reply to all the following questions
 */
enum Question:
  case RELEVANCE
  case SIGNIFICANCE
  case CONFIDENCE
  case FINAL

trait ConferenceReviewing:
  /**
   * Loads a review for the specified article, with complete scores as a map.
   *
   * @param article
   * @param scores
   */
  def loadReview(article: Int, scores: Map[Question, Int]): Unit

  /**
   * Loads a review for the specified article, with the 4 explicit scores.
   *
   * @param article
   * @param relevance
   * @param significance
   * @param confidence
   * @param fin
   */
  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit

  /**
   * @param article
   * @param question
   * @return the scores given to the specified article and specified question, as an (ascending-ordered) list
   */
  def orderedScores(article: Int, question: Question): List[Int]

  /**
   * @param article
   * @return the average score to question FINAL taken by the specified article
   */
  def averageFinalScore(article: Int): Double

  /**
   * An article is considered accepted if its averageFinalScore (not weighted) is > 5,
   * and at least one RELEVANCE score that is >= 8.
   *
   * @return the set of accepted articles
   */
  def acceptedArticles: Set[Int]

  /**
   * @return accepted articles as a list of pairs article+averageFinalScore, ordered from worst to best based on averageFinalScore
   */
  def sortedAcceptedArticles: List[(Int, Double)]

  /**
   * @return a map from articles to their average "weighted final score", namely, the average value of CONFIDENCE*FINAL/10
   */
  def averageWeightedFinalScoreMap: Map[Int, Double]

object ConferenceReviewing:
  def apply(): ConferenceReviewing = ConferenceReviewingImpl()

  private class ConferenceReviewingImpl(private var reviews: List[(Int, Map[Question, Int])] = List.empty) extends ConferenceReviewing:
    import Question.*

    def loadReview(article: Int, scores: Map[Question, Int]): Unit =
      if scores.size < Question.values.length then
        throw new IllegalArgumentException()
      else
        reviews = reviews.::((article, scores))

    def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
      val map = Map(
        RELEVANCE -> relevance,
        SIGNIFICANCE -> significance,
        CONFIDENCE -> confidence,
        FINAL -> fin
      )

      reviews = reviews.::((article, map))

    def orderedScores(article: Int, question: Question): List[Int] =
      reviews.filter((a, _) => a == article).map((_, m) => m(question)).sorted

    def averageFinalScore(article: Int): Double =
      val filteredReviews = reviews.filter((a, _) => a == article).map((_, m) => m(FINAL))
      filteredReviews.sum.toDouble / filteredReviews.size

    def acceptedArticles: Set[Int] =
      reviews.map(_._1).toSet.filter(isArticleAccepted)

    def sortedAcceptedArticles: List[(Int, Double)] =
      acceptedArticles.toList.map(e => (e, averageFinalScore(e))).sortBy(_._2)

    def averageWeightedFinalScoreMap: Map[Int, Double] =
      reviews.map(_._1).distinct.map(a => a -> averageWeightedFinalScore(a)).toMap

    private def isArticleAccepted(article: Int): Boolean =
      averageFinalScore(article) > 5.0 &&
        reviews.exists { case (x, y) => x == article && y.get(RELEVANCE).exists(_ >= 8) }

    private def averageWeightedFinalScore(article: Int): Double =
      val scores = reviews.filter((a, _) => a == article).map((_, m) => m(FINAL) * m(CONFIDENCE) / 10.0)
      scores.sum / scores.length