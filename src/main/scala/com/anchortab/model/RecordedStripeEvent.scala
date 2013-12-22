package com.anchortab.model

import net.liftweb._
  import common._
  import mongodb._
    import BsonDSL._
  import util._
    import Helpers._

import org.bson.types.ObjectId

case class RecordedStripeEvent(stripeEventId: String, _id: ObjectId = ObjectId.get) extends MongoDocument[RecordedStripeEvent] {
  val meta = RecordedStripeEvent
}
object RecordedStripeEvent extends MongoDocumentMeta[RecordedStripeEvent] {
  override def formats = allFormats
}
