package models.research

import java.util.UUID

import models.Helpers.{Columns, ForeignKeys}
import models.helpers.Ownable
import play.api.libs.json.{JsObject, JsValue, Json}
import slick.driver.MySQLDriver.api._
import system.helpers.{Resource, Validator$, ResourceCollection}
import system.helpers.SlickHelper._

import scala.util.{Failure, Success, Try}

/**
  * Represents a student or peer's peerReview of a [[Project]]
  * @param id The peer review's ID
  * @param ownerId The peer review owner's ID
  * @param projectDraftId The ID of the draft being peer reviewd
  * @param content The body of the peer review
  */
case class PeerReview(id: UUID,
                      ownerId: UUID,
                      projectDraftId: UUID,
                      content: String) extends Ownable with Resource {

  /**
    * The [[ProjectDraft]] being reviewed
    */
  lazy val projectDraft: ProjectDraft =
    projectDraftId.fk[ProjectDrafts, ProjectDraft](tableQueries.projectDrafts)
}

/**
  * A [[slick.profile.RelationalTableComponent.Table]] for [[PeerReview]]s
  * @param tag @see [[slick.lifted.Tag]]
  */
class PeerReviews(tag: Tag)
  extends Table[PeerReview](tag, "peer_reviews")
  with Columns.Id[PeerReview]
  with Columns.OwnerId[PeerReview] {

  /**
    * @see [[PeerReview.content]]
    */
  def content =
    column[String]("content")

  /**
    * @see [[PeerReviews.projectDraftId]]
    */
  def projectDraftId =
    column[UUID]("project_draft_id")

  /**
    * @inheritdoc
    */
  def * =
    (id, ownerId, projectDraftId, content).<>(PeerReview.tupled, PeerReview.unapply)

  def owner =
    foreignKey("fk_peer_review_owner_id", ownerId, models.tableQueries.users)(_.id)

  /**
    * Foreign key for [[PeerReviews.projectDraftId]]
    */
  def projectDraft =
    foreignKey("fk_peer_review_project_draft_id", projectDraftId, tableQueries.projectDrafts)(_.id)
}

object PeerReviews extends ResourceCollection[PeerReviews, PeerReview] {

  /**
    * @inheritdoc
    */
  val tableQuery =
    TableQuery[PeerReviews]

  /**
    * @inheritdoc
    */
  implicit val writes =
    Json.writes[PeerReview]

  /**
    * @inheritdoc
    */
  val validators =
    Set(
      ("ownerId", true, Set(Validator.uuid4 _)),
      ("projectDraftId", true, Set(Validator.uuid4 _)),
      ("content", true, Set[JsValue => Option[Int]]())
    )

  /**
    * @inheritdoc
    * @param uuid The UUID to use in the creation
    * @param arguments A map containing values to be updated
    * @return A new [[PeerReview]]
    */
  def creator(uuid: UUID,
              arguments: Map[String, JsValue]) =
    PeerReview(
      uuid,
      arguments("ownerId").as[UUID],
      arguments("projectDraftId").as[UUID],
      arguments("content").as[String]
    )

  /**
    * @inheritdoc
    * @param row A [[PeerReview]]
    * @param arguments A map containing values to be updated
    * @return A new [[PeerReview]]
    */
  def updater(row: PeerReview,
              arguments: Map[String, JsValue]) =
    row.copy(
      row.id,
      row.ownerId,
      row.projectDraftId,
      arguments.get("content")
        .fold(row.content)(_.as[String])
    )


  /**
    * @inheritdoc
    */
  def canRead(resourceId: Option[UUID],
              userId: Option[UUID],
              data: JsObject = Json.obj()): Boolean =
    Try(resourceId.get.toInstance[PeerReviews, PeerReview](tableQueries.peerReviews)) match {
      case Success(instance) =>
        userId.fold(false)(_ == instance.ownerId) ||
          userId.fold(false)(_ == instance.projectDraft.project.ownerId)
      case Failure(_) =>
        false
    }

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
  // TODO: Only allow certain users to critique a Project
  def canCreate(resourceId: Option[UUID],
                userId: Option[UUID],
                data: JsObject = Json.obj()): Boolean =
    userId.nonEmpty

}

