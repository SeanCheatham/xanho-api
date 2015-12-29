package brain.neuronTypes.school

import java.util.UUID

import models.Helpers.{Columns, ForeignKeys}
import play.api.libs.json.{JsObject, JsValue, Json}
import slick.driver.MySQLDriver.api._
import system.helpers.SlickHelper._
import system.helpers.{Resource, Validator$, ResourceCollection}

/**
  * Represents a Topic Requirement for completion of a Microdegree
  * @param id The requirement ID
  * @param microdegreeRevisionProposalId @see [[MicrodegreeRevisionProposal.id]]
  * @param topicId @see [[Topic.id]]
  * @param minimumRevision @see [[TopicRevision.revisionNumber]]
  * @param maximumRevision @see [[TopicRevision.revisionNumber]]
  */
case class TopicRequirement(id: UUID,
                            microdegreeRevisionProposalId: UUID,
                            topicId: UUID,
                            minimumRevision: Option[Int],
                            maximumRevision: Option[Int]) extends Resource {

  /**
    * The [[MicrodegreeRevisionProposal]] in which this requirement is used
    */
  lazy val microdegreeRevisionProposal: MicrodegreeRevisionProposal =
    microdegreeRevisionProposalId.fk[MicrodegreeRevisionProposals, MicrodegreeRevisionProposal](tableQueries.microdegreeRevisionProposals)

  /**
    * The [[Topic]] used for this revision
    */
  lazy val topic: Topic =
    topicId.fk[Topics, Topic](tableQueries.topics)

}

/**
  * A [[slick.profile.RelationalTableComponent.Table]] for [[TopicRequirement]]s
  * @param tag @see [[slick.lifted.Tag]]
  */
class TopicRequirements(tag: Tag)
  extends Table[TopicRequirement](tag, "topic_requirement")
  with Columns.Id[TopicRequirement]
  with Columns.ProposalId[TopicRequirement]
  with Columns.TopicId[TopicRequirement] {

  /**
    * @see [[TopicRequirement.minimumRevision]]
    */
  def minimumRevision =
    column[Option[Int]]("minimum_revision")

  /**
    * @see [[TopicRequirement.maximumRevision]]
    */
  def maximumRevision =
    column[Option[Int]]("maximum_revision")

  /**
    * @inheritdoc
    * @return
    */
  def * =
    (id, proposalId, topicId, minimumRevision, maximumRevision).<>(TopicRequirement.tupled, TopicRequirement.unapply)

  /**
    * The proposal containing this requirement
    */
  def proposal =
    foreignKey("fk_topic_requirement_microdegree_revision_proposal", proposalId, tableQueries.microdegreeRevisionProposals)(_.id)

  def topic =
    foreignKey("fk_topic_requirement_topic_id", topicId, tableQueries.topics)(_.id)

}

object TopicRequirements extends ResourceCollection[TopicRequirements, TopicRequirement] {

  /**
    * @inheritdoc
    */
  val tableQuery =
    TableQuery[TopicRequirements]

  /**
    * @inheritdoc
    */
  implicit val writes =
    Json.writes[TopicRequirement]

  /**
    * @inheritdoc
    */
  val validators =
    Set(
      ("microdegreeRevisionProposalId", true, Set(Validator.uuid4 _)),
      ("topicId", true, Set(Validator.uuid4 _)),
      ("minimumRevision", false, Set(Validator.integer _)),
      ("maximumRevision", false, Set(Validator.integer _))
    )

  /**
    * @inheritdoc
    * @param uuid The UUID to use in the creation
    * @param arguments A map containing values to be updated
    * @return A new [[TopicRequirement]]
    */
  def creator(uuid: UUID,
              arguments: Map[String, JsValue]) =
    TopicRequirement(
      uuid,
      arguments("microdegreeRevisionProposalId").as[UUID],
      arguments("topicId").as[UUID],
      arguments.get("minimumRevision") map (_.as[Int]),
      arguments.get("maximumRevision") map (_.as[Int])
    )

  /**
    * @inheritdoc
    * @param row A [[TopicRequirement]]
    * @param arguments A map containing values to be updated
    * @return A new [[TopicRequirement]]
    */
  def updater(row: TopicRequirement,
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
    false

}