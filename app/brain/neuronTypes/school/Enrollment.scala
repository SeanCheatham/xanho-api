package brain.neuronTypes.school

import java.util.UUID

import models.Helpers.Columns
import models.{User, Users}
import play.api.libs.json.{Writes, JsObject, JsValue, Json}
import slick.driver.MySQLDriver.api._
import system.helpers.SlickHelper._
import system.helpers.{Validator$, Resource, ResourceCollection, SlickHelper}

import scala.util.{Failure, Success, Try}

/**
  * Represents a revision to a [[Topic]]
  * @param id The revision's ID
  * @param topicRevisionId @see [[TopicRevision.id]]
  * @param studentId @see [[models.User.id]]
  */
case class Enrollment(id: UUID,
                      topicRevisionId: UUID,
                      studentId: UUID) extends Resource {

  /**
    * The particular [[TopicRevision]] in which the student is enrolled
    */
  lazy val topicRevision: TopicRevision =
    topicRevisionId.fk[TopicRevisions, TopicRevision](tableQueries.topicRevisions)

  /**
    * The [[User]] who is enrolled in the Topic
    */
  lazy val student: User =
    studentId.fk[Users, User](models.tableQueries.users)

}


/**
  * A [[slick.profile.RelationalTableComponent.Table]] for [[Enrollment]]s
  * @param tag @see [[slick.lifted.Tag]]
  */
class Enrollments(tag: Tag)
  extends Table[Enrollment](tag, "enrollments")
  with Columns.Id[Enrollment] {

  /**
    * @see [[Enrollment.topicRevisionId]]
    */
  def topicRevisionId =
    column[UUID]("topic_revision_id")

  /**
    * @see [[Enrollment.studentId]]
    */
  def studentId =
    column[UUID]("student_id")

  /**
    * @inheritdoc
    */
  def * =
    (id, topicRevisionId, studentId).<>(Enrollment.tupled, Enrollment.unapply)

  /**
    * @see [[Enrollment.topicRevision]]
    */
  def topicRevision =
    foreignKey("fk_enrollments_topic_revision_id", topicRevisionId, tableQueries.topicRevisions)(_.id)

  /**
    * @see [[Enrollment.student]]
    */
  def student =
    foreignKey("fk_enrollments_student_id", studentId, models.tableQueries.users)(_.id)
}

object Enrollments extends ResourceCollection[Enrollments, Enrollment] {

  /**
    * @inheritdoc
    */
  val tableQuery =
    TableQuery[Enrollments]

  /**
    * @inheritdoc
    */
  implicit val writes: Writes[Enrollment] =
    Json.writes[Enrollment]

  /**
    * @inheritdoc
    */
  val validators =
    Set(
      ("topicRevisionId", true, Set(Validator.uuid4 _)),
      ("studentId", true, Set(Validator.uuid4 _))
    )

  /**
    * @inheritdoc
    * @param uuid The UUID to use in the creation
    * @param arguments A map containing values to be updated
    * @return A new [[Enrollment]]
    */
  def creator(uuid: UUID,
              arguments: Map[String, JsValue]) =
    Enrollment(
      uuid,
      arguments("topicRevisionId").as[UUID],
      arguments("studentId").as[UUID]
    )

  /**
    * @inheritdoc
    * @param row A [[Enrollment]]
    * @param arguments A map containing values to be updated
    * @return A new [[Enrollment]]
    */
  def updater(row: Enrollment,
              arguments: Map[String, JsValue]) =
    row


  /**
    * @inheritdoc
    * An enrollment can be read if the given user ID is the student enrolled
    */
  def canRead(resourceId: Option[UUID],
              userId: Option[UUID],
              data: JsObject = Json.obj()): Boolean =
    Try(resourceId.get.toInstance[Enrollments, Enrollment](tableQueries.enrollments)) match {
      case Success(instance) =>
        userId.fold(false)(_ == instance.studentId)
      case Failure(_) =>
        false
    }

  /**
    * @inheritdoc
    * The enrollee can choose to disenroll
    */
  def canDelete(resourceId: Option[UUID],
                userId: Option[UUID],
                data: JsObject = Json.obj()): Boolean =
    Try(resourceId.get.toInstance[Enrollments, Enrollment](tableQueries.enrollments)) match {
      case Success(instance) =>
        userId.fold(false)(_ == instance.studentId)
      case Failure(_) =>
        false
    }

  /**
    * @inheritdoc
    * An enrollment can't be modified.
    */
  def canModify(resourceId: Option[UUID],
                userId: Option[UUID],
                data: JsObject = Json.obj()): Boolean =
    false

  /**
    * @inheritdoc
    * A user can only enroll in a topic (revision) if the student is not already enrolled in the topic (revision),
    * and the student has paid for the enrollment
    */
  // TODO: Check for confirmed payment
  def canCreate(resourceId: Option[UUID],
                userId: Option[UUID],
                data: JsObject = Json.obj()): Boolean =
    userId
      .fold(false)(uid =>
        (data \ "topicRevisionId").asOpt[UUID]
          .fold(false)(trid =>
            SlickHelper.queryResult(
              tableQueries.enrollments
                .filter(enrollment => enrollment.topicRevisionId === trid && enrollment.studentId === uid)
                .result
                .headOption
            ).fold((data \ "studentId").as[UUID] == uid)(_ => false)
          )
      )

}