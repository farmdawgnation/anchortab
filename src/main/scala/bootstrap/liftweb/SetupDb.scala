package bootstrap.liftweb

import net.liftweb.common._
import net.liftweb.util._
  import Helpers._
import net.liftweb.mongodb._

import org.joda.time._

import scala.io._

import com.anchortab.model.{DatabaseMeta, DatabaseVersion}

object SetupDb extends SetupDb
trait SetupDb extends Loggable {
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

  protected def latestDatabaseVersion = {
    for {
      databaseVersionInputStream <- tryo(getClass.getClassLoader.getResourceAsStream("migrations/DBVERSION"))
      databaseVersionAsString <- Source.fromInputStream(databaseVersionInputStream).getLines.toList.headOption
      databaseVersion <- AsInt.unapply(databaseVersionAsString)
    } yield {
      databaseVersion
    }
  }

  protected def migrationFilenameForVersion(version: Int) =
    s"migrations/anchortab-$version.js"

  def ensureDbVersionIsSet = {
    DatabaseMeta.find(DatabaseMeta.databaseVersionKey) match {
      case None =>
        for {
          databaseVersion <- latestDatabaseVersion
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

  def migrateDatabase = {
    val latestVersion = latestDatabaseVersion.openOrThrowException("Can't complete db boot w/o version.")

    DatabaseMeta.find(DatabaseMeta.databaseVersionKey) match {
      case Some(DatabaseMeta(_, DatabaseVersion(version), _, _)) if version < latestVersion =>
        logger.info("Database is out of date. Running migrations.")

        (version until latestVersion).foreach { versionMigrating =>
          logger.info("Upgrading to database version " + (versionMigrating + 1))
          runMigration(versionMigrating + 1)
        }

      case None =>
        throw new Exception("Couldn't retrieve current DB version. Things may be broken.")

      case _ =>
        logger.info("No database migration needed.")
    }
  }

  def runMigration(version: Int) = {
    val migrationFilename = migrationFilenameForVersion(version)
    val migrationStream = getClass.getClassLoader.getResourceAsStream(migrationFilename)
    val migrationFunction = Source.fromInputStream(migrationStream).getLines.mkString("\n")

    DatabaseMeta.useDb { db =>
      db.doEval(migrationFunction)
    }

    for(metaVersion <- DatabaseMeta.find(DatabaseMeta.databaseVersionKey)) {
      val updatedMetaVersion = metaVersion.copy(
        value = DatabaseVersion(version),
        updatedAt = new DateTime()
      )

      updatedMetaVersion.save
    }
  }
}
