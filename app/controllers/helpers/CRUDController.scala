package controllers.helpers

import java.util.UUID

import _root_.play.api.http.ContentTypes
import _root_.play.api.libs.json.{JsValue, Json}
import _root_.play.api.mvc.{Action, Controller}
import models.Helpers.Columns
import slick.driver.MySQLDriver.api._
import system.helpers.Validator.PropertyErrorCodes
import system.helpers._


trait CRUDController[T <: Table[R] with Columns.Id[R], R <: Resource] extends Controller with Secured {

  def resourceCollection: ResourceCollection[T, R]

  /**
    * An [[Action]] to create a new resource in the [[resourceCollection]]
    * @return An [[Action]]
    */
  def create =
    Authorized(None, Set(resourceCollection.canCreate))(parse.json)(request =>
      resourceCollection.validateArguments(request.data.as[Map[String, JsValue]]) match {
        case m if m.isEmpty =>
          resourceCollection.create(request.data.as[Map[String, JsValue]])
            .fold(InternalServerError(ResponseHelpers.message("Something broke.")))(item =>
              Ok(Json.toJson(item)(resourceCollection.writes))
            )
        case m =>
          BadRequest(ResponseHelpers.invalidFields(m))
      }
    )


  /**
    * An [[Action]] to read the resource with the provided ID in [[resourceCollection]]
    * @param uuid @see [[system.helpers.Resource.id]]
    * @return An [[Action]]
    */
  def read(uuid: String) =
    Authorized(Some(UUID.fromString(uuid)), Set(resourceCollection.canRead))(request =>
      resourceCollection.read(UUID.fromString(uuid))
        .fold(NotFound(ResponseHelpers.message("The resource with ID %s could not be found." format uuid)))(item =>
          Ok(Json.toJson(item)(resourceCollection.writes))
        )
    )

  /**
    * An [[Action]] to update the resource with the provided ID in [[resourceCollection]]
    * @param uuid @see [[system.helpers.Resource.id]]
    * @return An [[Action]]
    */
  def update(uuid: String) =
    Authorized(Some(UUID.fromString(uuid)), Set(resourceCollection.canModify))(parse.json)(request =>
      resourceCollection.validateArguments(request.data.as[Map[String, JsValue]]) filterNot (_._2 == PropertyErrorCodes.NO_VALUE) match {
        case m if m.isEmpty =>
          if (resourceCollection.update(UUID.fromString(uuid), request.data.as[Map[String, JsValue]]))
            Accepted
          else
            InternalServerError(ResponseHelpers.message("Something broke."))
        case m =>
          BadRequest(ResponseHelpers.invalidFields(m))
      }
    )

  /**
    * An [[Action]] to delete the resource with the provided ID in [[resourceCollection]]
    * @param uuid @see [[system.helpers.Resource.id]]
    * @return An [[Action]]
    */
  def delete(uuid: String) =
    Authorized(Some(UUID.fromString(uuid)), Set(resourceCollection.canDelete))(request =>
      if (resourceCollection.delete(UUID.fromString(uuid)))
        NoContent
      else
        NotFound(ResponseHelpers.message("The resource with ID %s could not be found." format uuid))
    )

}