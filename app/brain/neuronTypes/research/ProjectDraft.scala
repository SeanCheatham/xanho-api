package brain.neuronTypes.research

import java.util.UUID

import models.Helpers.Columns
import play.api.libs.json.{Writes, JsObject, JsValue, Json}
import slick.driver.MySQLDriver.api._
import system.helpers.SlickHelper._
import system.helpers.{Resource, Validator$, ResourceCollection, SlickHelper}

import scala.util.{Failure, Success, Try}


/**
  * Represents a draft for a [[Project]]
  * @param id The draft's ID
  * @param revisionNumber The revision number
  * @param projectId The [[Project.id]] to which this draft belongs
  * @param content The content body of the draft
  */
case class ProjectDraft(id: UUID,
                        revisionNumber: Int,
                        projectId: UUID,
                        content: String) extends Resource {

  /**
    * The [[Project]] to which this draft belongs
    */
  lazy val project: Project =
    projectId.fk[Projects, Project](tableQueries.projects)

}

/**
  * A [[slick.profile.RelationalTableComponent.Table]] for [[ProjectDraft]]s
  * @param tag @see [[slick.lifted.Tag]]
  */
class ProjectDrafts(tag: Tag)
  extends Table[ProjectDraft](tag, "project_drafts")
  with Columns.Id[ProjectDraft]
  with Columns.RevisionNumber[ProjectDraft] {

  /**
    * @see [[ProjectDraft.content]]
    */
  def content =
    column[String]("content")

  /**
    * @see [[ProjectDraft.projectId]]
    */
  def projectId =
    column[UUID]("project_id")

  /**
    * @inheritdoc
    */
  def * =
    (id, revisionNumber, projectId, content).<>(ProjectDraft.tupled, ProjectDraft.unapply)

  /**
    * Foreign key to the [[Project]]
    */
  def project =
    foreignKey("fk_draft_project_id", projectId, tableQueries.projects)(_.id)

}

object ProjectDrafts extends ResourceCollection[ProjectDrafts, ProjectDraft] {

  /**
    * @inheritdoc
    */
  val tableQuery =
    TableQuery[ProjectDrafts]

  /**
    * @inheritdoc
    */
  implicit val writes: Writes[ProjectDraft] =
    Json.writes[ProjectDraft]

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
    * @return A new [[ProjectDraft]]
    */
  def creator(uuid: UUID,
              arguments: Map[String, JsValue]) =
    ProjectDraft(
      uuid,
      SlickHelper.queryResult(
        tableQuery
          .filter(_.projectId === arguments("projectId").as[UUID])
          .map(_.revisionNumber).max.result
      ).getOrElse(0),
      arguments("projectId").as[UUID],
      arguments("content").as[String]
    )

  /**
    * @inheritdoc
    * @param row A [[ProjectDraft]]
    * @param arguments A map containing values to be updated
    * @return A new [[ProjectDraft]]
    */
  def updater(row: ProjectDraft,
              arguments: Map[String, JsValue]) =
    row.copy(
      row.id,
      row.revisionNumber,
      row.projectId,
      arguments.get("content")
        .fold(row.content)(_.as[String])
    )


  /**
    * @inheritdoc
    */
  def canRead(resourceId: Option[UUID],
              userId: Option[UUID],
              data: JsObject = Json.obj()): Boolean =
    Try(resourceId.get.toInstance[ProjectDrafts, ProjectDraft](tableQueries.projectDrafts)) match {
      case Success(instance) =>
        userId.fold(false)(_ == instance.project.ownerId)
      case Failure(_) =>
        false
    }

  /**
    * @inheritdoc
    */
  def canDelete(resourceId: Option[UUID],
                userId: Option[UUID],
                data: JsObject = Json.obj()): Boolean =
    Try(resourceId.get.toInstance[ProjectDrafts, ProjectDraft](tableQueries.projectDrafts)) match {
      case Success(instance) =>
        userId.fold(false)(_ == instance.project.ownerId)
      case Failure(_) =>
        false
    }

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
    (data \ "projectId").asOpt[UUID]
      .fold(false)(pid =>
        userId
          .fold(false)(uid =>
            pid.toInstance[Projects, Project](tableQueries.projects).ownerId == uid
          )
      )

}

