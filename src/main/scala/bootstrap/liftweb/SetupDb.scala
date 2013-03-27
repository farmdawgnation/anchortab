package bootstrap.liftweb

import net.liftweb.util._
import net.liftweb.mongodb._

object SetupDb {
  def setup = {
    for {
      hostname <- Props.get("mongodb.host")
      port <- Props.get("mongodb.port").map(_.toInt)
      database <- Props.get("mongodb.database")
    } {
      val mongoHost = MongoHost(hostname, port)
      MongoDB.defineDb(DefaultMongoIdentifier, MongoAddress(mongoHost, database))
    }
  }
}
