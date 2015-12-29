package brain.neuronTypes.school

import java.util.UUID

import models.Helpers.Columns
import models.helpers.OptionallyOwnable
import play.api.libs.json._
import slick.driver.MySQLDriver.api._
import system.helpers.SlickHelper._
import system.helpers.{Validator$, Resource, ResourceCollection}

import scala.util.{Failure, Success, Try}

/**
  * Represents a microdegree or unit, which covers a specific area
  * @param id The microdegree's ID
  * @param title The microdegree's name or title
  * @param ownerId The optional [[models.User]] owner of this microdegree.  If [[None]], then
  *                this microdegree is considered to be owned by the public.
  */
case class Microdegree(id: UUID,
                       title: String,
                       ownerId: Option[UUID]) extends OptionallyOwnable with Resource

/**
  * A [[slick.profile.RelationalTableComponent.Table]] for [[Microdegree]]s
  * @param tag @see [[slick.lifted.Tag]]
  */
class Microdegrees(tag: Tag)
  extends Table[Microdegree](tag, "microdegrees")
  with Columns.Id[Microdegree]
  with Columns.Title[Microdegree]
  with Columns.OptionalOwnerId[Microdegree] {

  /**
    * @see [[slick.profile.RelationalTableComponent.Table.*]]
    */
  def * =
    (id, title, ownerId).<>(Microdegree.tupled, Microdegree.unapply)

  def owner =
    foreignKey("fk_microdegree_owner_id", ownerId, models.tableQueries.users)(_.id.?)

}

object Microdegrees extends ResourceCollection[Microdegrees, Microdegree] {

  /**
    * @inheritdoc
    */
  val tableQuery =
    TableQuery[Microdegrees]

  /**
    * @inheritdoc
    */
  implicit val writes: Writes[Microdegree] =
    Json.writes[Microdegree]

  /**
    * @inheritdoc
    */
  val validators =
    Set(
      ("title", true, Set(Validator.title _)),
      ("public", false, Set(Validator.boolean _))
    )

  /**
    * @inheritdoc
    * @param uuid The UUID to use in the creation
    * @param arguments A map containing values to be updated
    * @return A new [[Microdegree]]
    */
  def creator(uuid: UUID,
              arguments: Map[String, JsValue]) =
    Microdegree(
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
    * @param row A [[Microdegree]]
    * @param arguments A map containing values to be updated
    * @return A new [[Microdegree]]
    */
  def updater(row: Microdegree,
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
    Try(resourceId.get.toInstance[Microdegrees, Microdegree](tableQueries.microdegrees)) match {
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

