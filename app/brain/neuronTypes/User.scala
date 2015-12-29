package brain.neuronTypes

import java.util.UUID

import _root_.play.api.libs.json.{JsObject, JsValue, Json, Writes}
import brain.Neuron
import com.github.t3hnar.bcrypt._
import models.Helpers.Columns
import slick.driver.MySQLDriver.api._
import system.helpers.{SlickHelper, Validator$, Resource, ResourceCollection}

/**
  * A Xanho user
  * @param id @see [[Neuron.id]]
  * @param name @see [[Neuron.name]]
  * @param shortData @see [[Neuron.shortData]]
  * @param data @see [[Neuron.data]]
  */
case class User(id: UUID,
                name: Option[String],
                shortData: Option[JsObject],
                data: Option[JsObject]) extends Neuron {

  /**
    * @inheritdoc
    */
  val neuronType =
    "user"

}

object UserHelper {

  /**
    * Hashes a given raw password using bcrypt
    * @param rawPassword The raw, plain-text password
    * @return A bcrypt/hashed password
    */
  def hashPassword(rawPassword: String): String =
    rawPassword.bcrypt
}

/**
  * A [[slick.profile.RelationalTableComponent.Table]] for [[User]]s
  * @param tag @see [[slick.lifted.Tag]]
  */
class Users(tag: Tag)
  extends Table[User](tag, "users")
  with Columns.Id[User] {

  /**
    * @see [[User.id]]
    */
  def firstName =
    column[String]("first_name")

  /**
    * @see [[User.id]]
    */
  def lastName =
    column[String]("last_name")

  /**
    * @see [[User.id]]
    */
  def email =
    column[String]("email")

  /**
    * @see [[User.id]]
    */
  def password =
    column[String]("password")

  /**
    * @see [[slick.profile.RelationalTableComponent.Table.*]]
    */
  def * =
    (id, firstName, lastName, email, password).<>(User.tupled, User.unapply)
}

object Users extends ResourceColleoction[Users, User] {

  /**
    * @inheritdoc
    */
  val tableQuery =
    TableQuery[Users]

  /**
    * Attempts to authenticate the provided credentials, and if successful, returns the corresponding [[User]]
    * @param email @see [[User.email]]
    * @param password @see [[User.password]]
    * @return An optional [[User]]
    */
  def authenticate(email: String,
                   password: String): Option[User] =
    SlickHelper.queryResult(
      tableQueries.users
      .filter(_.email === email).result.headOption
    ) filter (user => password.isBcrypted(user.password))

  /**
    * @inheritdoc
    */
  implicit val writes: Writes[User] =
    new Writes[User] {
      def writes(o: User) =
        Json.obj(
          "id" -> o.id,
          "firstName" -> o.firstName,
          "lastName" -> o.lastName,
          "email" -> o.email
        )
    }

  /**
    * @inheritdoc
    */
  val validators =
    Set(
      ("firstName", true, Set(Validator.name _)),
      ("lastName", true, Set(Validator.name _)),
      ("email", true, Set(Validator.email _)),
      ("password", true, Set(Validator.password _))
    )

  /**
    * @inheritdoc
    * @param uuid The UUID to use in the creation
    * @param arguments A map containing values to be updated
    * @return A new [[User]]
    */
  def creator(uuid: UUID,
              arguments: Map[String, JsValue]) =
    User(
      uuid,
      arguments("firstName").as[String],
      arguments("lastName").as[String],
      arguments("email").as[String],
      UserHelper.hashPassword(arguments("password").as[String])
    )

  /**
    * @inheritdoc
    * @param row A [[User]]
    * @param arguments A map containing values to be updated
    * @return A new [[User]]
    */
  def updater(row: User,
              arguments: Map[String, JsValue]) =
    row.copy(
      row.id,
      arguments.get("firstName")
        .fold(row.firstName)(_.as[String]),
      arguments.get("lastName")
        .fold(row.lastName)(_.as[String]),
      arguments.get("email")
        .fold(row.email)(_.as[String]),
      arguments.get("password")
        .fold(row.password)(p => UserHelper.hashPassword(p.as[String]))
    )
  
  /**
    * @inheritdoc
    */
  def canRead(resourceId: Option[UUID],
              userId: Option[UUID],
              data: JsObject = Json.obj()): Boolean =
    userId
      .fold(false)(uid => resourceId.fold(false)(_ == uid))

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
    userId
      .fold(false)(uid => resourceId.fold(false)(_ == uid))

  /**
    * @inheritdoc
    */
  def canCreate(resourceId: Option[UUID] = None,
                userId: Option[UUID],
                data: JsObject = Json.obj()): Boolean =
    userId.isEmpty

}