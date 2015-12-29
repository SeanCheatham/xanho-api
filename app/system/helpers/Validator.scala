package system.helpers

import play.api.libs.json.Reads._
import play.api.libs.json._

case class Validator(propertyName: String,
                     required: Boolean,
                     rules: Vector[JsValue => Option[Int]]) {

  def validate(arguments: Map[String, JsValue]): Option[Int] =
    arguments.get(propertyName)
      .fold(
        if (required)
          Some(Validator.PropertyErrorCodes.NO_VALUE)
        else
          None
      )(a => rules.foldLeft[Option[Int]](None) {
        case (None, rule: (JsValue => Option[Int])) =>
          rule(a)
        case (Some(x), _) =>
          Some(x)
      })

}

object Validator {

  /**
    * Checks the provided arguments, and validates the necessary properties
    * @param arguments The arguments to be validated
    * @param validators @see [[brain.NeuronCollection.validators]]
    * @return A invalid property mapping from a property name to an error status
    */
  def validateArguments(arguments: Map[String, JsValue],
                        validators: Set[Validator]): Map[String, Int] =
    (validators map (validator => validator.propertyName -> validator.validate(arguments)))
      .toMap collect {
      case (key, Some(value)) =>
        key -> value
    }

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