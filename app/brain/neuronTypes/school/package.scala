package brain.neuronTypes

import slick.driver.MySQLDriver.api._

package object school {

  /**
    * Table queries for school models
    */
  object tableQueries {

    /**
      * The [[TableQuery]] for [[school.Topics]]
      */
    val topics =
      TableQuery[school.Topics]

    /**
      * The [[TableQuery]] for [[school.TopicRevisions]]
      */
    val topicRevisions =
      TableQuery[school.TopicRevisions]

    /**
      * The [[TableQuery]] for [[school.TopicRevisionProposals]]
      */
    val topicRevisionProposals =
      TableQuery[school.TopicRevisionProposals]

    /**
      * The [[TableQuery]] for [[school.Microdegrees]]
      */
    val microdegrees =
      TableQuery[school.Microdegrees]

    /**
      * The [[TableQuery]] for [[school.MicrodegreeRevisions]]
      */
    val microdegreeRevisions =
      TableQuery[school.MicrodegreeRevisions]

    /**
      * The [[TableQuery]] for [[school.MicrodegreeRevisionProposals]]
      */
    val microdegreeRevisionProposals =
      TableQuery[school.MicrodegreeRevisionProposals]

    /**
      * The [[TableQuery]] for [[school.TopicRequirements]]
      */
    val topicRequirements =
      TableQuery[school.TopicRequirements]

    /**
      * The [[TableQuery]] for [[school.Enrollments]]
      */
    val enrollments =
      TableQuery[school.Enrollments]

  }


}
