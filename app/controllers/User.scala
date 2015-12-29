package controllers

import controllers.helpers.CRUDController
import models.Users
import models.Users._
import play.api.libs.json.Json
import system.helpers.JwtPlayImplicits._
import system.helpers.{Validator$, ResponseHelpers}


class User extends CRUDController[Users, models.User] {

  /**
    * @inheritdoc
    */
  def resourceCollection =
    Users

  /**
    * Authenticates a user, and if successful,
    * responds with a JWT, with its claim consisting of the user's information
    */
  def login =
    Authorized(resourceId = None)(parse.json)(implicit request => {
      request.userId
        .fold({
          Validator.validateArguments(
            request.data.value.toMap,
            Set(
              ("email", true, Set(Validator.email _)),
              ("password", true, Set(Validator.password _))
            )
          ) match {
            case m if m.nonEmpty =>
              BadRequest(ResponseHelpers.invalidFields(m))
            case _ =>
              Users.authenticate((request.data \ "email").as[String], (request.data \ "password").as[String])
                .fold(
                  Unauthorized(ResponseHelpers.message("Invalid credentials provided."))
                )(u => Ok.withNewJwtSession.addingToJwtSession("user", Json.toJson(u)))
          }
        }
        )(_ => Unauthorized(ResponseHelpers.message("You are already logged in.")))
    }
    )

}
