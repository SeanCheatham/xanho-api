package models.school

import java.util.UUID

import models.Helpers.Columns
import models.helpers.OptionallyOwnable
import play.api.libs.json._
import slick.driver.MySQLDriver.api._
import system.helpers.SlickHelper._
import system.helpers.{PropertyValidators, Resource, ResourceCollection}

import scala.util.{Failure, Success, Try}

/**
  * Represents a topic or unit, which covers a specific area
  * @param id The topic's ID
  * @param title The topic's name or title
  * @param ownerId The optional [[models.User]] owner of this topic.  If [[None]], then
  *                this topic is considered to be owned by the public.
  */
case class Topic(id: UUID,
                 title: String,
                 ownerId: Option[UUID]) extends OptionallyOwnable with Resource

/**
  * A [[slick.profile.RelationalTableComponent.Table]] for [[Topic]]s
  * @param tag @see [[slick.lifted.Tag]]
  */
class Topics(tag: Tag)
  extends Table[Topic](tag, "topics")
  with Columns.Id[Topic]
  with Columns.Title[Topic]
  with Columns.OptionalOwnerId[Topic] {

  /**
    * @see [[slick.profile.RelationalTableComponent.Table.*]]
    */
  def * =
    (id, title, ownerId).<>(Topic.tupled, Topic.unapply)

  def owner =
    foreignKey("fk_topic_owner_id", ownerId, models.tableQueries.users)(_.id.?)
}

object Topics extends ResourceCollection[Topics, Topic] {

  /**
    * @inheritdoc
    */
  val tableQuery =
    TableQuery[Topics]

  /**
    * @inheritdoc
    */
  implicit val writes: Writes[Topic] =
    Json.writes[Topic]

  /**
    * @inheritdoc
    */
  val validators =
    Set(
      ("title", true, Set(PropertyValidators.title _)),
      ("public", false, Set(PropertyValidators.boolean _))
    )

  /**
    * @inheritdoc
    * @param uuid The UUID to use in the creation
    * @param arguments A map containing values to be updated
    * @return A new [[Topic]]
    */
  def creator(uuid: UUID,
              arguments: Map[String, JsValue]) =
    Topic(
      uuid,
      arguments("title").as[String],
      arguments.get("public") match {
        case Some(p: JsBoolean) if !p.value =>
          Some(arguments("userId").as[UUID])
        case None =>
          Some(arguments("userId").as[UUID])
        case _ =>
          None
      }
    )

  /**
    * @inheritdoc
    * @param row A [[Topic]]
    * @param arguments A map containing values to be updated
    * @return A new [[Topic]]
    */
  def updater(row: Topic,
              arguments: Map[String, JsValue]) =
    row.copy(
      row.id,
      arguments.get("title")
        .fold(row.title)(_.as[String]),
      arguments.get("public") match {
        case Some(p: JsBoolean) if !p.value =>
          Some(arguments("userId").as[UUID])
        case None =>
          Some(arguments("userId").as[UUID])
        case _ =>
          None
      }
    )


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
    Try(resourceId.get.toInstance[Topics, Topic](tableQueries.topics)) match {
      case Success(instance) =>
        userId.fold(false)(uid => instance.ownerId.fold(true)(_ == uid))
      case Failure(_) =>
        false
    }

  /**
    * @inheritdoc
    */
  def canCreate(resourceId: Option[UUID],
                userId: Option[UUID],
                data: JsObject = Json.obj()): Boolean =
    userId.nonEmpty

}

