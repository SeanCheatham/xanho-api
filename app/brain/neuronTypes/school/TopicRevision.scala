package brain.neuronTypes.school

import java.util.UUID

import models.Helpers.Columns
import play.api.libs.json.{JsObject, JsValue, Json, Writes}
import slick.driver.MySQLDriver.api._
import system.helpers.SlickHelper._
import system.helpers.{Validator$, Resource, ResourceCollection, SlickHelper}

import scala.util.{Success, Try}

/**
  * Represents a revision to a [[Topic]]
  * @param id The revision's ID
  * @param revisionNumber The revision number
  * @param topicId @see [[Topic.id]]
  * @param proposalId @see [[TopicRevisionProposal.id]]
  */
case class TopicRevision(id: UUID,
                         revisionNumber: Int,
                         topicId: UUID,
                         proposalId: UUID) extends Resource {

  /**
    * The parent [[Topic]] of this revision
    */
  lazy val topic: Topic =
    topicId.fk[Topics, Topic](tableQueries.topics)

  /**
    * The parent [[TopicRevisionProposal]] of this revision
    */
  lazy val topicRevisionProposal: TopicRevisionProposal =
    proposalId.fk[TopicRevisionProposals, TopicRevisionProposal](tableQueries.topicRevisionProposals)

}


/**
  * A [[slick.profile.RelationalTableComponent.Table]] for [[TopicRevision]]s
  * @param tag @see [[slick.lifted.Tag]]
  */
class TopicRevisions(tag: Tag)
  extends Table[TopicRevision](tag, "topic_revisions")
  with Columns.Id[TopicRevision]
  with Columns.RevisionNumber[TopicRevision]
  with Columns.TopicId[TopicRevision]
  with Columns.ProposalId[TopicRevision] {

  /**
    * @inheritdoc
    */
  def * =
    (id, revisionNumber, topicId, proposalId).<>(TopicRevision.tupled, TopicRevision.unapply)

  def topic =
    foreignKey("fk_topic_revision_topic_id", topicId, tableQueries.topics)(_.id)

  /**
    * The proposal used in this revision
    */
  def proposal =
    foreignKey("fk_topic_revision_proposal_id", proposalId, tableQueries.topicRevisionProposals)(_.id)
}

object TopicRevisions extends ResourceCollection[TopicRevisions, TopicRevision] {

  /**
    * @inheritdoc
    */
  val tableQuery =
    TableQuery[TopicRevisions]

  /**
    * @inheritdoc
    */
  implicit val writes: Writes[TopicRevision] =
    Json.writes[TopicRevision]

  /**
    * @inheritdoc
    */
  val validators =
    Set(
      ("topicId", true, Set(Validator.uuid4 _)),
      ("proposalId", true, Set(Validator.uuid4 _))
    )

  /**
    * @inheritdoc
    * @param uuid The UUID to use in the creation
    * @param arguments A map containing values to be updated
    * @return A new [[TopicRevision]]
    */
  def creator(uuid: UUID,
              arguments: Map[String, JsValue]) =
    TopicRevision(
      uuid,
      SlickHelper.queryResult(
        tableQuery
          .filter(tr => tr.topicId === arguments("topicId").as[UUID] && tr.proposalId === arguments("proposalId").as[UUID])
          .map(_.revisionNumber).max.result
      ).fold(0)(_ + 1),
      arguments("topicId").as[UUID],
      arguments("proposalId").as[UUID]
    )

  /**
    * @inheritdoc
    * @param row A [[TopicRevision]]
    * @param arguments A map containing values to be updated
    * @return A new [[TopicRevision]]
    */
  def updater(row: TopicRevision,
              arguments: Map[String, JsValue]) =
    row


  /**
    * @inheritdoc
    */
  def canRead(resourceId: Option[UUID],
              userId: Option[UUID],
              data: JsObject = Json.obj()): Boolean =
    true

  /**
    * @inheritdoc
    */
  def canDelete(resourceId: Option[UUID],
                userId: Option[UUID],
                data: JsObject = Json.obj()): Boolean =
    false

  /**
    * @inheritdoc
    */
  def canModify(resourceId: Option[UUID],
                userId: Option[UUID],
                data: JsObject = Json.obj()): Boolean =
    false

  /**
    * @inheritdoc
    */
  def canCreate(resourceId: Option[UUID],
                userId: Option[UUID],
                data: JsObject = Json.obj()): Boolean =
    userId
      .fold(false)(uid =>
        Try((data \ "topicId").as[UUID].toInstance[Topics, Topic](tableQueries.topics)) match {
          case Success(topic) =>
            topic.ownerId.fold(true)(_ == uid)
          case _ =>
            false
        }
      )


}