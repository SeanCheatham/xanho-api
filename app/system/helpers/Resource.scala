package system.helpers

import java.util.UUID

import _root_.play.api.libs.json.Reads._
import _root_.play.api.libs.json._
import models.Helpers.Columns
import slick.driver.MySQLDriver.api._

/**
  * Represents a specific, identifiable application resource.
  */
trait Resource {

  /**
    * All resources must have a unique identifier
    * @return a UUID
    */
  def id: UUID

}

/**
  * Represents a container or collection of [[Resource]]s.  In essence, this allows requesting authorization
  * on a particular resource in the collection.
  * @tparam R The [[Resource]] of this collection
  * @tparam T Type bound used for the TableQuery to ensure the table has an ID column
  */
trait ResourceCollection[T <: Table[R] with Columns.Id[R], R <: Resource] {

  /**
    * This collection's [[TableQuery]]
    * @return A [[TableQuery]]
    */
  def tableQuery: TableQuery[T]

  /**
    * An implicit writer to convert this [[R]] to JSON
    * @return A [[Writes]] [[R]]
    */
  implicit def writes: Writes[R]

  /**
    * A set of validators which are used in [[validateArguments]]
    * @return A [[Set]] of tuples of (Field Name, Required, Set of validation requirements)
    */
  def validators: Set[(String, Boolean, Set[JsValue => Option[Int]])]


  /**
    * Creates a [[R]] with the given arguments
    * @param arguments A key-value argument pair
    * @return An optional [[R]]
    */
  def create(arguments: Map[String, JsValue]): Option[R] = {
    val uuid =
      system.helpers.uuid

    if (
      SlickHelper.queryResult(
        tableQuery += creator(uuid, arguments)
      ) > 0
    )
      SlickHelper.optionalFindById[T, R](tableQuery, uuid)
    else
      None
  }

  /**
    * Retrieves the [[R]] with the given ID
    * @param id @see [[Resource.id]]
    * @return An optional [[R]] if one is found
    */
  def read(id: UUID): Option[R] =
    SlickHelper.optionalFindById[T, R](tableQuery, id)

  /**
    * Deletes the [[system.helpers.Resource]] with the given [[system.helpers.Resource.id]]
    * @param id @see [[system.helpers.Resource.id]]
    * @return true if successful, false otherwise
    */
  def delete(id: UUID): Boolean =
    SlickHelper.queryResult((tableQuery filter (_.id === id)).delete) > 0

  /**
    * Given a row [[R]], updates the corresponding values in the given arguments
    * Assume that the data is valid.
    * @param row A [[R]]
    * @param arguments A map containing values to be updated
    * @return A new [[R]]
    */
  def updater(row: R,
              arguments: Map[String, JsValue]): R

  /**
    * Given a map of field names to values, creates a new [[R]]
    * @param uuid The UUID to use in the creation
    * @param arguments A map containing values to be updated
    * @return A new [[R]]
    */
  def creator(uuid: UUID,
              arguments: Map[String, JsValue]): R

  /**
    * Updates the [[R]] with the given ID, to the given arguments
    * @param id @see [[Resource.id]]
    * @param arguments A key-value argument pair
    * @return true if successful, false otherwise
    */
  def update(id: UUID,
             arguments: Map[String, JsValue]): Boolean =
    read(id)
      .map(
        row =>
          SlickHelper.queryResult(
            tableQuery
              .filter(_.id === id)
              .update(updater(row, arguments))
          )
      )
      .fold(false)(_ > 0)

  /**
    * Dictates if the user with the given ID is allowed READ access to the resource with the given ID
    * @param resourceId @see [[Resource.id]]
    * @param userId @see [[models.User.id]]
    * @param data The JSON object provided by the user containing data about the resource
    * @return true if authorized, false if unauthorized
    */
  def canRead(resourceId: Option[UUID],
              userId: Option[UUID],
              data: JsObject = Json.obj()): Boolean

  /**
    * Dictates if the user with the given ID is allowed MODIFY access to the resource with the given ID
    * @param resourceId @see [[Resource.id]]
    * @param userId @see [[models.User.id]]
    * @param data The JSON object provided by the user containing data about the resource
    * @return true if authorized, false if unauthorized
    */
  def canModify(resourceId: Option[UUID],
                userId: Option[UUID],
                data: JsObject = Json.obj()): Boolean

  /**
    * Dictates if the user with the given ID is allowed DELETE access to the resource with the given ID
    * @param resourceId @see [[Resource.id]]
    * @param userId @see [[models.User.id]]
    * @param data The JSON object provided by the user containing data about the resource
    * @return true if authorized, false if unauthorized
    */
  def canDelete(resourceId: Option[UUID],
                userId: Option[UUID],
                data: JsObject = Json.obj()): Boolean

  /**
    * Dictates if the user with the given ID is allowed CREATE access in this collection
    * @param userId @see [[models.User.id]]
    * @param data The JSON object provided by the user containing data about the resource
    * @return true if authorized, false if unauthorized
    */
  def canCreate(resourceId: Option[UUID],
                userId: Option[UUID],
                data: JsObject = Json.obj()): Boolean

  /**
    * Checks the provided arguments, and validates the necessary properties
    * @param arguments The arguments to be validated
    * @return A invalid property mapping from a property name to an error status
    */
  def validateArguments(arguments: Map[String, JsValue]): Map[String, Int] =
    Validator.validateArguments(arguments, validators)

}

object PropertyValidators {

  /**
    * Checks the provided arguments, and validates the necessary properties
    * @param arguments The arguments to be validated
    * @param validators @see [[ResourceCollection.validators]]
    * @return A invalid property mapping from a property name to an error status
    */
  def validateArguments(arguments: Map[String, JsValue],
                        validators: Set[(String, Boolean, Set[(JsValue => Option[Int])])]): Map[String, Int] =
    (validators map {
      case (key: String, required: Boolean, rules: Set[(JsValue => Option[Int])]) =>
        key -> Validator.validate(key, arguments, required, rules)
    }).toMap collect {
      case (key, Some(value)) =>
        key -> value
    }

  /**
    * Validates the given key against the given arguments, using the given set of rules
    * @param key The key in the arguments map
    * @param arguments The list of arguments containing the key
    * @param required Dictates if the property is required
    * @param rules The rules to match against the argument
    * @return An optional error code
    */
  def validate(key: String,
               arguments: Map[String, JsValue],
               required: Boolean,
               rules: Set[JsValue => Option[Int]]): Option[Int] =
    arguments.get(key)
      .fold(
        if (required)
          Some(PropertyErrorCodes.NO_VALUE)
        else
          None
      )(a => rules.foldLeft[Option[Int]](None) {
        case (None, rule: (JsValue => Option[Int])) =>
          rule(a)
        case (Some(x), _) =>
          Some(x)
      })

  /**
    * Validates a name field
    * @param s The given input
    * @return An optional error code
    */
  def name(s: JsValue): Option[Int] =
    s.validate(__.read[JsString])
      .fold(
        _ => Some(PropertyErrorCodes.INVALID_TYPE),
        _ => s.validate[String](__.read(minLength[String](2)))
          .fold(
            _ => Some(PropertyErrorCodes.TOO_SHORT),
            _ => s.validate[String](maxLength[String](30))
              .fold(
                _ => Some(PropertyErrorCodes.TOO_LONG), {
                  case namePattern(_*) =>
                    None
                  case _ =>
                    Some(PropertyErrorCodes.NOT_NAME)
                }
              )
          )
      )

  /**
    * Validates a title field
    * @param s The given input
    * @return An optional error code
    */
  def title(s: JsValue): Option[Int] =
    s.validate(__.read[JsString])
      .fold(
        _ => Some(PropertyErrorCodes.INVALID_TYPE),
        _ => s.validate[String](__.read(minLength[String](2)))
          .fold(
            _ => Some(PropertyErrorCodes.TOO_SHORT),
            _ => s.validate[String](maxLength[String](100))
              .fold(
                _ => Some(PropertyErrorCodes.TOO_LONG), {
                  case namePattern(_*) =>
                    None
                  case _ =>
                    Some(PropertyErrorCodes.NOT_TITLE)
                }
              )
          )
      )

  /**
    * Validates a content field
    * @param s The given input
    * @return An optional error code
    */
  def content(s: JsValue): Option[Int] =
    s.validate(__.read[JsString])
      .fold(
        _ => Some(PropertyErrorCodes.INVALID_TYPE),
        _ => s.validate[String](__.read(minLength[String](50)))
          .fold(
            _ => Some(PropertyErrorCodes.TOO_SHORT),
            _ => None
          )
      )

  /**
    * Validates an email address
    * @param s The given input
    * @return An optional error code
    */
  def email(s: JsValue): Option[Int] =
    s.validate(__.read[JsString])
      .fold(
        _ => Some(PropertyErrorCodes.INVALID_TYPE),
        _.validate(Reads.email)
          .fold(
            _ => Some(PropertyErrorCodes.INVALID_EMAIL),
            _ => None
          )
      )

  /**
    * Validates a password field, requiring it to be a string,
    * be at least two letters long, be at most 200 characters long,
    * contain at least one number,
    * and at least one non-alpha numeric character
    * @param s The given input
    * @return An optional error code
    */
  def password(s: JsValue): Option[Int] =
    s.validate(__.read[JsString])
      .fold(
        _ => Some(PropertyErrorCodes.INVALID_TYPE),
        _.validate[String](minLength[String](2))
          .fold(
            _ => Some(PropertyErrorCodes.TOO_SHORT),
            _ => s.validate[String](maxLength[String](255))
              .fold(
                _ => Some(PropertyErrorCodes.TOO_LONG),
                p => if ((nonAlphaNumericPattern findAllIn p).isEmpty || (numericPattern findAllIn p).isEmpty)
                  Some(PropertyErrorCodes.NOT_COMPLEX_ENOUGH)
                else
                  None
              )

          )
      )

  /**
    * Validates that the given input is a Boolean data type
    * @param s The given input
    * @return An optional error code
    */
  def boolean(s: JsValue): Option[Int] =
    s.validate(__.read[Boolean])
      .fold(
        _ => Some(PropertyErrorCodes.INVALID_TYPE),
        _ => None
      )

  /**
    * Validates an integer field
    * @param s The given input
    * @return An optional error code
    */
  def integer(s: JsValue): Option[Int] =
    s.validate(__.read[Int])
      .fold(
        _ => Some(PropertyErrorCodes.INVALID_TYPE),
        _ => None
      )

  /**
    * Validates a score field, meaning the integer is between 0 and 100
    * @param s The given input
    * @return An optional error code
    */
  def score(s: JsValue): Option[Int] =
    integer(s)
      .fold(
        s.validate(__.read(min[Int](0)))
          .fold(
            _ => Some(PropertyErrorCodes.TOO_SMALL),
            _ => s.validate(__.read(max[Int](100)))
              .fold(
                _ => None,
                _ => Some(PropertyErrorCodes.TOO_LARGE))
          )
      )(Some(_))

  /**
    * Validates a UUID4
    * @param s The given input
    * @return An optional error code
    */
  def uuid4(s: JsValue): Option[Int] =
    s.validate(__.read[String])
      .fold(
        _ => Some(PropertyErrorCodes.INVALID_TYPE), {
          case uuid4Pattern(_*) =>
            None
          case _ =>
            Some(PropertyErrorCodes.NOT_UUID)
        }
      )

  private val namePattern =
    """[A-Za-z- ]*""".r

  private val nonAlphaNumericPattern =
    """([^A-Za-z0-9])""".r

  private val numericPattern =
    """([0-9])""".r

  private val uuid4Pattern =
    """[0-9a-f\-]{36}""".r

  object PropertyErrorCodes {

    val NO_VALUE = 0
    val INVALID_TYPE = 1
    val TOO_SHORT = 2
    val TOO_LONG = 3
    val INVALID_EMAIL = 4
    val INVALID_CHARACTERS = 5
    val NOT_COMPLEX_ENOUGH = 6
    val NOT_UUID = 7
    val NOT_NAME = 8
    val NOT_TITLE = 9
    val TOO_SMALL = 10
    val TOO_LARGE = 11
  }

}
