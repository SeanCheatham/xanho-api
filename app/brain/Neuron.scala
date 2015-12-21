package brain

import java.util.UUID
import play.api.libs.json.{JsObject, JsValue, Json, Writes}
import slick.driver.MySQLDriver.api._
import slick.lifted.ProvenShape
import system.helpers.SlickHelper._
import system.helpers.{PropertyValidators, Resource, ResourceCollection, SlickHelper}

/**
  * An abstract representation of an object, node, vertex, or piece of data/information
  * A Neuron is of a certain type (a sub-class, that is); various types are
  * responsible for various functions.  A Neuron has several incoming and
  * outgoing [[Synapse]]s which represent relationships between Neurons.
  */
trait Neuron {

  /**
    * The [[UUID]] of this Neuron
    */
  val id: UUID

  /**
    * An optional brief previous or short description of the Neuron
    */
  val neuronType: String

  /**
    * An optional title, identifier, or name of the Neuron
    */
  val name: Option[String]

  /**
    * An optional brief previous or short description of the Neuron
    */
  val shortData: Option[JsObject]

  /**
    * The optional data within this Neuron
    */
  val data: Option[JsObject]


}

object Neuron {
  def apply(id: UUID,
            neuronType: String,
            name: Option[String],
            shortData: Option[String],
            data: Option[String]): Neuron =
    ???

  def unapply(neuron: Neuron): Option[(UUID, String, Option[String], Option[String], Option[String])] =
    Some(neuron.id, neuron.neuronType, neuron.name, neuron.shortData map Json.stringify, neuron.data map Json.stringify)
}

class Neurons(tag: Tag) extends Table[Neuron](tag, "neurons") {

  def id =
    column[UUID]("id", O.PrimaryKey, O.Default[UUID](java.util.UUID.randomUUID()))

  def neuronType =
    column[String]("neuron_type")

  def name =
    column[Option[String]]("name")

  def shortData =
    column[Option[String]]("short_data")

  def data =
    column[Option[String]]("data")

  def * : ProvenShape[Neuron] =
    (id, neuronType, name, shortData, data).<>(
      (Neuron.apply _).tupled,
      Neuron.unapply
    )

}