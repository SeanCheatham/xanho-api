package brain

import java.util.UUID

import play.api.libs.json.{JsObject, JsValue, Json, Writes}
import slick.driver.MySQLDriver.api._
import system.helpers.SlickHelper._
import system.helpers.{PropertyValidators, Resource, ResourceCollection, SlickHelper}

case class Synapse(id: UUID,
                   aId: UUID,
                   bId: UUID,
                   data: Option[JsObject]) {

}
