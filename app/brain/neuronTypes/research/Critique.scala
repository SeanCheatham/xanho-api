package brain.neuronTypes.research

import java.util.UUID

import models.Helpers.{Columns, ForeignKeys}
import models.helpers.Ownable
import play.api.libs.json.{JsObject, JsValue, Json}
import slick.driver.MySQLDriver.api._
import system.helpers.SlickHelper._
import system.helpers._

import scala.util.{Failure, Success, Try}

/**
  * Represents a student or peer's critique of a [[Project]]
  * @param id The critique's ID
  * @param ownerId The critique owner's ID
  * @param projectDraftId The ID of the draft being critiqued
  * @param content The body of the critique
  */
case class Critique(id: UUID,
                    ownerId: UUID,
                    projectDraftId: UUID,
                    content: String,
                    score: Int) extends Ownable with Resource {

  /**
    * The [[ProjectDraft]] being critiqued
    */
  lazy val projectDraft: ProjectDraft =
    projectDraftId.fk[ProjectDrafts, ProjectDraft](tableQueries.projectDrafts)

  /**
    * @inheritdoc
    */
  def canRead(userId: Option[UUID]): Boolean =
    userId.fold(false)(_ == ownerId) || userId.fold(false)(_ == projectDraft.project.ownerId)

  /**
    * @inheritdoc
    */
  def canDelete(userId: Option[UUID]): Boolean =
    false

  /**
    * @inheritdoc
    */
  def canModify(userId: Option[UUID]): Boolean =
    false
}
/**
  * A [[slick.profile.RelationalTableComponent.Table]] for [[Critique]]s
  * @param tag @see [[slick.lifted.Tag]]
  */
class Critiques(tag: Tag)
  extends Table[Critique](tag, "critiques")
  with Columns.Id[Critique]
  with Columns.OwnerId[Critique] {

  /**
    * @see [[Critique.content]]
    */
  def content =
    column[String]("content")

  /**
    * @see [[Critique.projectDraftId]]
    */
  def projectDraftId =
    column[UUID]("project_draft_id")

  /**
    * @see [[Critique.score]]
    */
  def score =
    column[Int]("score")

  /**
    * @inheritdoc
    */
  def * =
    (id, ownerId, projectDraftId, content, score).<>(Critique.tupled, Critique.unapply)

  def owner =
    foreignKey("fk_critique_owner_id", ownerId, models.tableQueries.users)(_.id)

  /**
    * Foreign key for [[Critique.projectDraftId]]
    */
  def projectDraft =
    foreignKey("fk_project_draft", projectDraftId, tableQueries.projectDrafts)(_.id)
}

object Critiques extends ResourceCollection[Critiques, Critique] {

  /**
    * @inheritdoc
    */
  val tableQuery =
    TableQuery[Critiques]

  /**
    * @inheritdoc
    */
  implicit val writes =
    Json.writes[Critique]

  /**
    * @inheritdoc
    */
  val validators =
    Set(
      ("ownerId", true, Set(Validator.uuid4 _)),
      ("projectDraftId", true, Set(Validator.uuid4 _)),
      ("content", true, Set[JsValue => Option[Int]]()),
      ("score", true, Set(Validator.score _))
    )

  /**
    * @inheritdoc
    * @param uuid The UUID to use in the creation
    * @param arguments A map containing values to be updated
    * @return A new [[Critique]]
    */
  def creator(uuid: UUID,
              arguments: Map[String, JsValue]) =
    Critique(
      uuid,
      arguments("ownerId").as[UUID],
      arguments("projectDraftId").as[UUID],
      arguments("content").as[String],
      arguments("score").as[Int]
    )

  /**
    * @inheritdoc
    * @param row A [[Critique]]
    * @param arguments A map containing values to be updated
    * @return A new [[Critique]]
    */
  def updater(row: Critique,
              arguments: Map[String, JsValue]) =
    row.copy(
      row.id,
      row.ownerId,
      row.projectDraftId,
      arguments.get("content")
        .fold(row.content)(_.as[String]),
      row.score
    )


  /**
    * @inheritdoc
    */
  def canRead(resourceId: Option[UUID],
              userId: Option[UUID],
              data: JsObject = Json.obj()): Boolean =
    Try(resourceId.get.toInstance[Critiques, Critique](tableQueries.critiques)) match {
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

