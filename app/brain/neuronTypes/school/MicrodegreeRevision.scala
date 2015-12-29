package brain.neuronTypes.school

import java.util.UUID

import models.Helpers.Columns
import play.api.libs.json.{JsObject, JsValue, Json}
import slick.driver.MySQLDriver.api._
import system.helpers.SlickHelper._
import system.helpers.{Validator$, Resource, ResourceCollection, SlickHelper}

import scala.util.{Failure, Success, Try}

/**
  * Represents a revision to a [[Microdegree]]
  * @param id The revision's ID
  * @param revisionNumber The revision number
  * @param microdegreeId @see [[Microdegree.id]]
  * @param proposalId @see [[MicrodegreeRevisionProposal.id]]
  */
case class MicrodegreeRevision(id: UUID,
                               revisionNumber: Int,
                               microdegreeId: UUID,
                               proposalId: UUID) extends Resource {

  /**
    * The parent [[Microdegree]] of this revision
    */
  lazy val microdegree: Microdegree =
    microdegreeId.fk[Microdegrees, Microdegree](tableQueries.microdegrees)

  /**
    * The [[MicrodegreeRevisionProposal]] used for this revision
    */
  lazy val microdegreeRevisionProposal: MicrodegreeRevisionProposal =
    proposalId.fk[MicrodegreeRevisionProposals, MicrodegreeRevisionProposal](tableQueries.microdegreeRevisionProposals)

}


/**
  * A [[slick.profile.RelationalTableComponent.Table]] for [[MicrodegreeRevision]]s
  * @param tag @see [[slick.lifted.Tag]]
  */
class MicrodegreeRevisions(tag: Tag)
  extends Table[MicrodegreeRevision](tag, "microdegree_revisions")
  with Columns.Id[MicrodegreeRevision]
  with Columns.RevisionNumber[MicrodegreeRevision]
  with Columns.MicrodegreeId[MicrodegreeRevision]
  with Columns.ProposalId[MicrodegreeRevision] {

  /**
    * @inheritdoc
    */
  def * =
    (id, revisionNumber, microdegreeId, proposalId).<>(MicrodegreeRevision.tupled, MicrodegreeRevision.unapply)

  def microdegree =
    foreignKey("fk_microdegree_revision_microdegree_id", microdegreeId, tableQueries.microdegrees)(_.id)

  /**
    * The proposal used in this revision
    */
  def proposal =
    foreignKey("fk_microdegree_revision_proposal_id", proposalId, tableQueries.microdegreeRevisionProposals)(_.id)
}

object MicrodegreeRevisions extends ResourceCollection[MicrodegreeRevisions, MicrodegreeRevision] {

  /**
    * @inheritdoc
    */
  val tableQuery =
    TableQuery[MicrodegreeRevisions]

  /**
    * @inheritdoc
    */
  implicit val writes =
    Json.writes[MicrodegreeRevision]

  /**
    * @inheritdoc
    */
  val validators =
    Set(
      ("microdegreeId", true, Set(Validator.uuid4 _)),
      ("proposalId", true, Set(Validator.uuid4 _))
    )

  /**
    * @inheritdoc
    * @param uuid The UUID to use in the creation
    * @param arguments A map containing values to be updated
    * @return A new [[MicrodegreeRevision]]
    */
  def creator(uuid: UUID,
              arguments: Map[String, JsValue]) =
    MicrodegreeRevision(
      uuid,
      SlickHelper.queryResult(
        tableQuery
          .filter(tr => tr.microdegreeId === arguments("microdegreeId").as[UUID] && tr.proposalId === arguments("proposalId").as[UUID])
          .map(_.revisionNumber).max.result
      ).fold(0)(_ + 1),
      arguments("microdegreeId").as[UUID],
      arguments("proposalId").as[UUID]
    )

  /**
    * @inheritdoc
    * @param row A [[MicrodegreeRevision]]
    * @param arguments A map containing values to be updated
    * @return A new [[MicrodegreeRevision]]
    */
  def updater(row: MicrodegreeRevision,
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
        Try((data \ "microdegreeId").as[UUID].toInstance[Microdegrees, Microdegree](tableQueries.microdegrees)) match {
          case Success(microdegree) =>
            microdegree.ownerId.fold(true)(_ == uid)
          case _ =>
            false
        }
      )

}