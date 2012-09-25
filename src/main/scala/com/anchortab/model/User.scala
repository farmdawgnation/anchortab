package com.anchortab.model

import net.liftweb._
  import mongodb._
  import util.Helpers._

import org.joda.time.DateTime

import org.bson.types.ObjectId

case class UserSession(userId:ObjectId, ip:String, userAgent:String,
                       createdAt:DateTime = new DateTime,
                       _id: ObjectId = ObjectId.get) extends MongoDocument[UserSession] {
  val meta = UserSession
}
object UserSession extends MongoDocumentMeta[UserSession] {
}

case class UserAuthorizationKey(appName:String, key:String = randomString(32),
                                createdAt:DateTime = new DateTime)

case class UserProfile(firstName:Option[String], lastName:Option[String], organization:Option[String])

case class User(email:String, password:String, profile:Option[UserProfile] = None,
                authorizations:List[UserAuthorizationKey] = List(), role:Option[String] = None,
                createdAt:DateTime = new DateTime, _id:ObjectId = ObjectId.get) extends MongoDocument[User] {
  val meta = User
}
object User extends MongoDocumentMeta[User] {
}
