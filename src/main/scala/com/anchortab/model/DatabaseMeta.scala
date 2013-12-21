package com.anchortab.model

import net.liftweb._
  import common._
  import mongodb._
  import util.Helpers._
  import json._
    import ext._
    import JsonDSL._
    import Extraction._

import org.joda.time.DateTime

import org.bson.types.ObjectId

sealed trait DatabaseMetaValue
case class DatabaseVersion(version: Int) extends DatabaseMetaValue
case class ArbitraryData(data: String) extends DatabaseMetaValue

case class DatabaseMeta(
  _id: String,
  value: DatabaseMetaValue,
  createdAt: DateTime = new DateTime(),
  updatedAt: DateTime = new DateTime()
) extends MongoDocument[DatabaseMeta] {
  val meta = DatabaseMeta
}
object DatabaseMeta extends MongoDocumentMeta[DatabaseMeta] {
  override def formats =
    allFormats ++
    JodaTimeSerializers.all +
    ShortTypeHints(classOf[DatabaseVersion] :: classOf[ArbitraryData] :: Nil)

  val databaseVersionKey = "database-version"
}
