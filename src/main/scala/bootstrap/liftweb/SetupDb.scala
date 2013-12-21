package bootstrap.liftweb

import net.liftweb.common._
import net.liftweb.util._
  import Helpers._
import net.liftweb.mongodb._

import scala.io._

import com.anchortab.model.{DatabaseMeta, DatabaseVersion}

object SetupDb extends Loggable {
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

  def ensureDbVersionIsSet = {
    DatabaseMeta.find(DatabaseMeta.databaseVersionKey) match {
      case None =>
        for {
          databaseVersionInputStream <- tryo(getClass.getClassLoader.getResourceAsStream("migrations/DBVERSION"))
          databaseVersionAsString <- Source.fromInputStream(databaseVersionInputStream).getLines.toList.headOption
          databaseVersion <- AsInt.unapply(databaseVersionAsString)
        } yield {
          DatabaseMeta(
            DatabaseMeta.databaseVersionKey,
            DatabaseVersion(databaseVersion)
          ).save

          logger.info(s"Database version set to $databaseVersion.")
        }

      case _ =>
    }
  }
}
