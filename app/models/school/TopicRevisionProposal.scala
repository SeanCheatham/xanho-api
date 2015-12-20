package models.school

import java.util.UUID

import models.Helpers.{Columns, ForeignKeys}
import models.helpers.Ownable
import play.api.libs.json.{Writes, JsObject, JsValue, Json}
import slick.driver.MySQLDriver.api._
import system.helpers.SlickHelper._
import system.helpers.{Resource, PropertyValidators, ResourceCollection, SlickHelper}

import scala.util.{Failure, Success, Try}


/**
  * Represents a proposed [[TopicRevision]] to a [[Topic]]
  * @param id The Topic Revision Proposal's ID
  * @param topicId @see [[Topic.id]]
  * @param newRevisionNumber The next/target revision number.  @see [[TopicRevision.revisionNumber]]
  *                          For example, if the currently active revision number is 19, then the newRevisionNumber would be 20
  * @param content The topic's content and materials
  */
case class TopicRevisionProposal(id: UUID,
                                 ownerId: UUID,
                                 topicId: UUID,
                                 newRevisionNumber: Int,
                                 content: String) extends Resource with Ownable {

  /**
    * The parent [[Topic]] of this revision
    */
  lazy val topic: Topic =
    topicId.fk[Topics, Topic](tableQueries.topics)

}

/**
  * A [[slick.profile.RelationalTableComponent.Table]] for [[TopicRevisionProposal]]s
  * @param tag @see [[slick.lifted.Tag]]
  */
class TopicRevisionProposals(tag: Tag)
  extends Table[TopicRevisionProposal](tag, "topic_revision_proposals")
  with Columns.Id[TopicRevisionProposal]
  with Columns.OwnerId[TopicRevisionProposal]
  with Columns.TopicId[TopicRevisionProposal]
  with Columns.NewRevisionNumber[TopicRevisionProposal] {

  /**
    * @see [[TopicRevisionProposal.content]]
    */
  def content =
    column[String]("content")

  /**
    * @inheritdoc
    */
  def * =
    (id, ownerId, topicId, newRevisionNumber, content).<>(TopicRevisionProposal.tupled, TopicRevisionProposal.unapply)

  def owner =
    foreignKey("fk_topic_revision_proposal_owner_id", ownerId, models.tableQueries.users)(_.id)

  def topic =
    foreignKey("fk_topic_revision_proposal_topic_id", topicId, tableQueries.topics)(_.id)

}

object TopicRevisionProposals extends ResourceCollection[TopicRevisionProposals, TopicRevisionProposal] {

  /**
    * @inheritdoc
    */
  val tableQuery =
    TableQuery[TopicRevisionProposals]

  /**
    * @inheritdoc
    */
  implicit val writes: Writes[TopicRevisionProposal] =
    Json.writes[TopicRevisionProposal]

  /**
    * @inheritdoc
    */
  val validators =
    Set(
      ("topicId", true, Set(PropertyValidators.uuid4 _)),
      ("content", true, Set(PropertyValidators.content _))
    )

  /**
    * @inheritdoc
    * @param uuid The UUID to use in the creation
    * @param arguments A map containing values to be updated
    * @return A new [[TopicRevisionProposal]]
    */
  def creator(uuid: UUID,
              arguments: Map[String, JsValue]) =
    TopicRevisionProposal(
      uuid,
      arguments("userId").as[UUID],
      arguments("topicId").as[UUID],
      SlickHelper.queryResult(
        TopicRevisions.tableQuery
          .filter(tr => tr.topicId === arguments("topicId").as[UUID])
          .map(_.revisionNumber).max.result
      ).fold(0)(_ + 1),
      arguments("content").as[String]
    )

  /**
    * @inheritdoc
    * @param row A [[TopicRevisionProposal]]
    * @param arguments A map containing values to be updated
    * @return A new [[TopicRevisionProposal]]
    */
  def updater(row: TopicRevisionProposal,
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
    userId.nonEmpty

}