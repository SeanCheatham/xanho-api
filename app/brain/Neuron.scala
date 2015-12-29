package brain

import java.util.UUID
import play.api.libs.json.{JsObject, JsValue, Json, Writes}
import slick.driver.MySQLDriver.api._
import slick.lifted.ProvenShape
import system.helpers.SlickHelper._
import system.helpers.{Validator$, Resource, ResourceCollection, SlickHelper}

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

  /**
    * Retrieves all of the synapses for which this neuron is the root/head
    */
  lazy val outgoingSynapses: Vector[Synapse] =
    SlickHelper.queryResult(Synapses.synapses.filter(_.aId === id).result).toVector

  /**
    * Retrieves all neurons to which this neuron connects
    */
  lazy val outgoingNeurons: Vector[Neuron] =
    outgoingSynapses map (_.b)

  /**
    * Retrieves all of the synapses for which this neuron is the target/destination
    */
  lazy val incomingSynapses: Vector[Synapse] =
    SlickHelper.queryResult(Synapses.synapses.filter(_.bId === id).result).toVector

  /**
    * Retrieves all neurons which connect to this neuron
    */
  lazy val incomingNeurons =
    incomingSynapses map (_.a)

}

object Neuron {

  /**
    * Factory to create a [[Neuron]]
    * @param id @see [[Neuron.id]]
    * @param neuronType @see [[Neuron.neuronType]]
    * @param name @see [[Neuron.name]]
    * @param shortData @see [[Neuron.shortData]]
    * @param data @see [[Neuron.data]]
    * @return A [[Neuron]]
    */
  def apply(id: UUID,
            neuronType: String,
            name: Option[String],
            shortData: Option[String],
            data: Option[String]): Neuron =
    ???

  /**
    * Converts a [[Neuron]] to a tuple
    * @param neuron @see [[Neuron]]
    * @return Option[(UUID, String, Option[String], Option[String], Option[String])]
    */
  def unapply(neuron: Neuron): Option[(UUID, String, Option[String], Option[String], Option[String])] =
    Some(neuron.id, neuron.neuronType, neuron.name, neuron.shortData map Json.stringify, neuron.data map Json.stringify)

}

/**
  * The [[Table]] for [[Neuron]]s
  * @param tag @see [[slick.lifted.Tag]]
  */
class Neurons(tag: Tag) extends Table[Neuron](tag, "neurons") {

  /**
    * @see [[Neuron.id]]
    */
  def id =
    column[UUID]("id", O.PrimaryKey, O.Default[UUID](java.util.UUID.randomUUID()))

  /**
    * @see [[Neuron.neuronType]]
    */
  def neuronType =
    column[String]("neuron_type")

  /**
    * @see [[Neuron.name]]
    */
  def name =
    column[Option[String]]("name")

  /**
    * @see [[Neuron.shortData]]
    */
  def shortData =
    column[Option[String]]("short_data")

  /**
    * @see [[Neuron.data]]
    */
  def data =
    column[Option[String]]("data")

  /**
    * @inheritdoc
    */
  def * =
    (id, neuronType, name, shortData, data).<>(
      (Neuron.apply _).tupled,
      Neuron.unapply
    )

}

/**
  * Companion object for [[Neurons]]
  */
object Neurons {

  /**
    * The [[TableQuery]] for [[[Neurons]]
    */
  val neurons =
    TableQuery[Neurons]

}