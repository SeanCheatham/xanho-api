package brain.neuronTypes

import slick.driver.MySQLDriver.api._

package object research {

  /**
    * Table queries for research models
    */
  object tableQueries {

    /**
      * The [[TableQuery]] for [[research.Critiques]]
      */
    val critiques =
      TableQuery[Critiques]

    /**
      * The [[TableQuery]] for [[research.PeerReviews]]
      */
    val peerReviews =
      TableQuery[PeerReviews]

    /**
      * The [[TableQuery]] for [[research.Projects]]
      */
    val projects =
      TableQuery[research.Projects]

    /**
      * The [[TableQuery]] for [[research.ProjectDrafts]]
      */
    val projectDrafts =
      TableQuery[research.ProjectDrafts]

  }

}
