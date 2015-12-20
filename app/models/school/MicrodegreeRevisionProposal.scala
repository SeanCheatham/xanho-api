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
  * Represents a proposed [[MicrodegreeRevision]] to a [[Microdegree]]
  * @param id The Microdegree Revision Proposal's ID
  * @param microdegreeId @see [[Microdegree.id]]
  * @param newRevisionNumber The next/target revision number.  @see [[MicrodegreeRevision.revisionNumber]]
  *                          For example, if the currently active revision number is 19, then the newRevisionNumber would be 20
  * @param content The microdegree's content and materials
  */
case class MicrodegreeRevisionProposal(id: UUID,
                                       ownerId: UUID,
                                       microdegreeId: UUID,
                                       newRevisionNumber: Int,
                                       content: String) extends Resource with Ownable {

  /**
    * The parent [[Microdegree]] of this revision
    */
  lazy val microdegree: Microdegree =
    microdegreeId.fk[Microdegrees, Microdegree](tableQueries.microdegrees)

}


/**
  * A [[slick.profile.RelationalTableComponent.Table]] for [[MicrodegreeRevisionProposal]]s
  * @param tag @see [[slick.lifted.Tag]]
  */
class MicrodegreeRevisionProposals(tag: Tag)
  extends Table[MicrodegreeRevisionProposal](tag, "microdegree_revision_proposals")
  with Columns.Id[MicrodegreeRevisionProposal]
  with Columns.OwnerId[MicrodegreeRevisionProposal]
  with Columns.MicrodegreeId[MicrodegreeRevisionProposal]
  with Columns.NewRevisionNumber[MicrodegreeRevisionProposal] {

  /**
    * @see [[MicrodegreeRevisionProposal.content]]
    */
  def content =
    column[String]("content")

  /**
    * @inheritdoc
    */
  def * =
    (id, ownerId, microdegreeId, newRevisionNumber, content).<>(MicrodegreeRevisionProposal.tupled, MicrodegreeRevisionProposal.unapply)

  def owner =
    foreignKey("fk_microdegree_revision_proposal_owner_id", ownerId, models.tableQueries.users)(_.id)

  def microdegree =
    foreignKey("fk_microdegree_revision_proposal_microdegree_id", microdegreeId, tableQueries.microdegrees)(_.id)

}

object MicrodegreeRevisionProposals extends ResourceCollection[MicrodegreeRevisionProposals, MicrodegreeRevisionProposal] {

  /**
    * @inheritdoc
    */
  val tableQuery =
    TableQuery[MicrodegreeRevisionProposals]

  /**
    * @inheritdoc
    */
  implicit val writes: Writes[MicrodegreeRevisionProposal] =
    Json.writes[MicrodegreeRevisionProposal]

  /**
    * @inheritdoc
    */
  val validators =
    Set(
      ("microdegreeId", true, Set(PropertyValidators.uuid4 _)),
      ("content", true, Set(PropertyValidators.content _))
    )

  /**
    * @inheritdoc
    * @param uuid The UUID to use in the creation
    * @param arguments A map containing values to be updated
    * @return A new [[MicrodegreeRevisionProposal]]
    */
  def creator(uuid: UUID,
              arguments: Map[String, JsValue]) =
    MicrodegreeRevisionProposal(
      uuid,
      arguments("userId").as[UUID],
      arguments("microdegreeId").as[UUID],
      SlickHelper.queryResult(
        MicrodegreeRevisions.tableQuery
          .filter(tr => tr.microdegreeId === arguments("microdegreeId").as[UUID])
          .map(_.revisionNumber).max.result
      ).fold(0)(_ + 1),
      arguments("content").as[String]
    )

  /**
    * @inheritdoc
    * @param row A [[MicrodegreeRevisionProposal]]
    * @param arguments A map containing values to be updated
    * @return A new [[MicrodegreeRevisionProposal]]
    */
  def updater(row: MicrodegreeRevisionProposal,
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

