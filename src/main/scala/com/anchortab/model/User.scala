package com.anchortab.model

import net.liftweb._
  import mongodb._

import org.joda.time.DateTime

import org.bson.types.ObjectId

case class UserSession(userId:ObjectId, ip:String, userAgent:String,
                       createdAt:DateTime = new DateTime,
                       _id: ObjectId = ObjectId.get) extends MongoDocument[UserSession] {
  val meta = UserSession
}
object UserSession extends MongoDocumentMeta[UserSession] {
}

case class User(email:String, password:String,
                createdAt:DateTime = new DateTime,
                _id:ObjectId = ObjectId.get) extends MongoDocument[User] {
  val meta = User
}
object User extends MongoDocumentMeta[User] {
}
