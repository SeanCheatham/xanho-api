package brain.neuronTypes.research

import java.util.UUID

import _root_.play.api.libs.json.{JsObject, JsValue, Writes, _}
import models.Helpers.Columns
import models.helpers.Ownable
import slick.driver.MySQLDriver.api._
import system.helpers.SlickHelper._
import system.helpers.{Resource, ResourceCollection, _}

import scala.util.{Failure, Success, Try}

/**
  * Represents a Research Project, which consists of a series of drafts
  * @param id The project's ID
  * @param ownerId The project owner's ID
  * @param enrollmentId If the student is using this project for credit for a topic,
  *                     then this is the enrollment in that topic.  A student can submit
  *                     a project for credit in at most one topic
  */
case class Project(id: UUID,
                   ownerId: UUID,
                   enrollmentId: Option[UUID]) extends Ownable with Resource

/**
  * A [[slick.profile.RelationalTableComponent.Table]] for [[Project]]s
  * @param tag @see [[slick.lifted.Tag]]
  */
class Projects(tag: Tag)
  extends Table[Project](tag, "projects")
  with Columns.Id[Project]
  with Columns.OwnerId[Project] {

  def enrollmentId =
    column[Option[UUID]]("enrollmentId")

  /**
    * @inheritdoc
    */
  def * =
    (id, ownerId, enrollmentId).<>(Project.tupled, Project.unapply)

  def owner =
    foreignKey("fk_project_owner_id", ownerId, models.tableQueries.users)(_.id)

  def enrollment =
    foreignKey("fk_project_enrollment_id", enrollmentId, models.school.tableQueries.enrollments)(_.id.?)

}

object Projects extends ResourceCollection[Projects, Project] {

  /**
    * @inheritdoc
    */
  val tableQuery =
    TableQuery[Projects]

  /**
    * @inheritdoc
    */
  implicit val writes: Writes[Project] =
    new Writes[Project] {
      def writes(o: Project) =
        Json.obj(
          "id" -> o.id,
          "ownerId" -> o.ownerId,
          "enrollmentId" -> o.enrollmentId
        )
    }

  /**
    * @inheritdoc
    */
  val validators =
    Set(
      ("userId", true, Set(Validator.uuid4 _)),
      ("enrollmentId", false, Set(Validator.uuid4 _))
    )

  /**
    * @inheritdoc
    * @param uuid The UUID to use in the creation
    * @param arguments A map containing values to be updated
    * @return A new [[Project]]
    */
  def creator(uuid: UUID,
              arguments: Map[String, JsValue]) =
    Project(
      uuid,
      arguments("userId").as[UUID],
      arguments("enrollmentId").asOpt[UUID]
    )

  /**
    * @inheritdoc
    * @param row A [[Project]]
    * @param arguments A map containing values to be updated
    * @return A new [[Project]]
    */
  def updater(row: Project,
              arguments: Map[String, JsValue]) =
    row.copy(
      row.id,
      arguments.get("userId")
        .fold(row.ownerId)(_.as[UUID]),
      arguments.get("enrollmentId")
        .fold(row.enrollmentId)(_.asOpt[UUID])
    )

  /**
    * @inheritdoc
    * The project owner may read his or her own project
    */
  def canRead(resourceId: Option[UUID],
              userId: Option[UUID],
              data: JsObject = Json.obj()): Boolean =
    userId
      .fold(false)(uid =>
        resourceId
          .fold(false)(
            read(_)
              .fold(false)(_.ownerId == uid)
          )
      )

  /**
    * @inheritdoc
    * The project owner may delete his or her own project
    */
  def canDelete(resourceId: Option[UUID],
                userId: Option[UUID],
                data: JsObject = Json.obj()): Boolean =
    userId
      .fold(false)(uid =>
        resourceId
          .fold(false)(
            read(_)
              .fold(false)(_.ownerId == uid)
          )
      )

  /**
    * @inheritdoc
    * A user can modify his or her own project.  If the user provides an enrollment ID,
    * the user must be the student in that enrollment
    */
  def canModify(resourceId: Option[UUID],
                userId: Option[UUID],
                data: JsObject = Json.obj()): Boolean =
    userId
      .fold(false)(uid =>
        resourceId
          .fold(false)(
            read(_)
              .fold(false)(_.ownerId == uid &&
                (data \ "enrollmentId").asOpt[UUID]
                  .fold(true)((eid: UUID) =>
                    Try(eid.toInstance[models.school.Enrollments, models.school.Enrollment](models.school.tableQueries.enrollments)) match {
                      case Success(enrollment) =>
                        enrollment.studentId == uid
                      case Failure(_) =>
                        false
                    }
                  )
              )
          )
      )

  /**
    * @inheritdoc
    * A user can create a project if he or she is logged in.  If the user provides an enrollment ID,
    * then the user must be student in the enrollment
    */
  def canCreate(resourceId: Option[UUID] = None,
                userId: Option[UUID],
                data: JsObject = Json.obj()): Boolean =
    userId
      .fold(false)(uid =>
        (data \ "enrollmentId").asOpt[UUID]
          .fold(true)((eid: UUID) =>
            Try(eid.toInstance[models.school.Enrollments, models.school.Enrollment](models.school.tableQueries.enrollments)) match {
              case Success(enrollment) =>
                enrollment.studentId == uid
              case Failure(_) =>
                false
            }
          )
      )


}