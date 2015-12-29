package brain

import java.util.UUID

import _root_.play.api.libs.json.Reads._
import _root_.play.api.libs.json._
import models.Helpers.Columns
import slick.driver.MySQLDriver.api._
import system.helpers.Validator

/**
  * Represents a container or collection of [[Neuron]]s,
  * providing neuron permission, access, and CRUD methods
  * @tparam N The [[Neuron]] of this collection
  */
trait NeuronCollection[N <: Neuron] {

  /**
    * An implicit writer to convert this [[N]] to JSON
    * @return A [[Writes]] [[N]]
    */
  implicit val writes: Writes[N] =
    new Writes[N] {
      def writes(o: N) =
        Json.obj(
          "id" -> o.id,
          "name" -> o.name,
          "shortData" -> o.shortData,
          "data" -> o.data,
          "incomingNeurons" -> (o.incomingNeurons groupBy (_.neuronType) map (kv => kv._1 -> (kv._2 map (_.id)))),
          "outgoingNeurons" -> (o.outgoingNeurons groupBy (_.neuronType) map (kv => kv._1 -> (kv._2 map (_.id))))
        )
    }

  /**
    * A set of validators which are used in [[validateArguments]]
    * @return A [[Set]] of [[Validator]] to be exectued on a map of arguments
    */
  def validators: Set[Validator]

  /**
    * Creates a [[N]] with the given arguments
    * @param arguments A key-value argument pair
    * @return An optional [[N]]
    */
  def create(arguments: Map[String, JsValue]): Option[N]

  /**
    * Retrieves the [[N]] with the given ID
    * @param id @see [[Neuron.id]]
    * @return An optional [[N]] if one is found
    */
  def read(id: UUID): Option[N]

  /**
    * Deletes the [[Neuron]] with the given [[Neuron.id]]
    * @param id @see [[Neuron.id]]
    * @return true if successful, false otherwise
    */
  def delete(id: UUID): Boolean

  /**
    * Given a row [[N]], updates the corresponding values in the given arguments
    * Assume that the data is valid.
    * @param row A [[N]]
    * @param arguments A map containing values to be updated
    * @return A new [[N]]
    */
  def updater(row: N,
              arguments: Map[String, JsValue]): N

  /**
    * Given a map of field names to values, creates a new [[N]]
    * @param uuid The UUID to use in the creation
    * @param arguments A map containing values to be updated
    * @return A new [[N]]
    */
  def creator(uuid: UUID,
              arguments: Map[String, JsValue]): N

  /**
    * Updates the [[N]] with the given ID, to the given arguments
    * @param id @see [[Neuron.id]]
    * @param arguments A key-value argument pair
    * @return true if successful, false otherwise
    */
  def update(id: UUID,
             arguments: Map[String, JsValue]): Boolean

  /**
    * Dictates if the user with the given ID is allowed READ access to the Neuron with the given ID
    * @param NeuronId @see [[Neuron.id]]
    * @param userId @see [[models.User.id]]
    * @param data The JSON object provided by the user containing data about the Neuron
    * @return true if authorized, false if unauthorized
    */
  def canRead(NeuronId: Option[UUID],
              userId: Option[UUID],
              data: JsObject = Json.obj()): Boolean

  /**
    * Dictates if the user with the given ID is allowed MODIFY access to the Neuron with the given ID
    * @param NeuronId @see [[Neuron.id]]
    * @param userId @see [[models.User.id]]
    * @param data The JSON object provided by the user containing data about the Neuron
    * @return true if authorized, false if unauthorized
    */
  def canModify(NeuronId: Option[UUID],
                userId: Option[UUID],
                data: JsObject = Json.obj()): Boolean

  /**
    * Dictates if the user with the given ID is allowed DELETE access to the Neuron with the given ID
    * @param NeuronId @see [[Neuron.id]]
    * @param userId @see [[models.User.id]]
    * @param data The JSON object provided by the user containing data about the Neuron
    * @return true if authorized, false if unauthorized
    */
  def canDelete(NeuronId: Option[UUID],
                userId: Option[UUID],
                data: JsObject = Json.obj()): Boolean

  /**
    * Dictates if the user with the given ID is allowed CREATE access in this collection
    * @param userId @see [[models.User.id]]
    * @param data The JSON object provided by the user containing data about the Neuron
    * @return true if authorized, false if unauthorized
    */
  def canCreate(NeuronId: Option[UUID],
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
