package brain

import java.util.UUID

import play.api.libs.json.{JsObject, JsValue, Json, Writes}
import slick.driver.MySQLDriver.api._
import system.helpers.SlickHelper._
import system.helpers.{Validator$, Resource, ResourceCollection, SlickHelper}

/**
  * A bridge/relation/connection/edge between two Neurons (neuron A and neuron B)
  * A synapse is unidirectional, meaning that the information flows
  * from one neuron to another, but not both ways.  In order
  * to obtain bidirectional flow, there must be a separate synapse.
  * @param id The [[UUID]] of this synapse
  * @param aId The source/head [[Neuron.id]]
  * @param bId The destination/target [[Neuron.id]]
  * @param data An optional data JSON object which may contain additional
  *             meta information about the synapse including a custom type
  */
case class Synapse(id: UUID,
                   aId: UUID,
                   bId: UUID,
                   data: Option[JsObject]) {

  /**
    * The source [[Neuron]]
    */
  lazy val a =
    aId.fk[Neurons, Neuron](Neurons.neurons)

  /**
    * The destination [[Neuron]]
    */
  lazy val b =
    bId.fk[Neurons, Neuron](Neurons.neurons)

}

/**
  * The [[Table]] for [[Synapse]]s
  * @param tag @see [[slick.lifted.Tag]]
  */
class Synapses(tag: Tag) extends Table[Synapse](tag, "synapses") {

  /**
    * @see [[Synapse.id]]
    */
  def id =
    column[UUID]("id", O.PrimaryKey, O.Default[UUID](java.util.UUID.randomUUID()))

  /**
    * @see [[Synapse.aId]]
    */
  def aId =
    column[UUID]("aId")

  /**
    * @see [[Synapse.bId]]
    */
  def bId =
    column[UUID]("bId")

  /**
    * @see [[Synapse.data]]
    */
  def data =
    column[Option[JsObject]]("data")

  /**
    * @inheritdoc
    */
  def * =
    (id, aId, bId, data).<>(Synapse.tupled, Synapse.unapply)

  /**
    * Foreign key reference for [[Synapse.aId]]
    */
  def a =
    foreignKey("fk_synapse_a", aId, Neurons.neurons)(_.id)

  /**
    * Foreign key reference for [[Synapse.aId]]
    */
  def b =
    foreignKey("fk_synapse_b", bId, Neurons.neurons)(_.id)

}

/**
  * The companion object for [[Synapses]]
  */
object Synapses {

  /**
    * The [[TableQuery]] for [[[Synapses]]
    */
  val synapses =
    TableQuery[Synapses]

}