package controllers.school

import controllers.helpers.CRUDController
import models.school

class Enrollment extends CRUDController[school.Enrollments, school.Enrollment] {

  /**
    * @inheritdoc
    */
  def resourceCollection =
    school.Enrollments

}
