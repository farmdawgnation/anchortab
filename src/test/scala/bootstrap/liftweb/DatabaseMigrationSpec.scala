package bootstrap.liftweb

import org.scalatest._

import com.anchortab.model._

import net.liftweb.common._

class DatabaseMigrationSpec extends FunSpec with ShouldMatchers with BeforeAndAfterAll {
  override def beforeAll() {
    SetupDb.setup
  }

  override def afterAll() {
    DatabaseMeta.drop
  }

  describe("The database migrations code") {
    it("should create the database version meta if it doesn't exist") {
      val testSetupDb = new SetupDb {
        override val latestDatabaseVersion = Full(1)
      }

      testSetupDb.ensureDbVersionIsSet

      val storedVersion = DatabaseMeta.find(DatabaseMeta.databaseVersionKey).get

      storedVersion.value match {
        case DatabaseVersion(version) =>
          version should equal (1)

        case _ =>
          fail("Something other than DatabaseVersion in meta.")
      }
    }

    it("should not change the database version when just checking for existance") {
      val testSetupDb = new SetupDb {
        override val latestDatabaseVersion = Full(2)
      }

      testSetupDb.ensureDbVersionIsSet

      val storedVersion = DatabaseMeta.find(DatabaseMeta.databaseVersionKey).get

      storedVersion.value match {
        case DatabaseVersion(version) =>
          version should equal (1)

        case _ =>
          fail("Something other than DatabaseVersion in meta.")
      }
    }

    it("should invoke the correct number of migrations in the correct order") {
      var migrationsInvoked: List[Int] = List()

      val testSetupDb = new SetupDb {
        override val latestDatabaseVersion = Full(4)

        override def runMigration(version: Int) = {
          migrationsInvoked = migrationsInvoked :+ version
        }
      }

      testSetupDb.migrateDatabase

      migrationsInvoked should equal (List(2, 3, 4))
    }

    it("should throw a null pointer exception for a migration that doesn't exist") {
      val testSetupDb = new SetupDb {
        override def migrationFilenameForVersion(version: Int) =
          s"migrations/test-$version.js"
      }

      an [NullPointerException] should be thrownBy testSetupDb.runMigration(3)
    }

    it("should correctly retrieve and execute a migration on the DB") {
      val testSetupDb = new SetupDb {
        override def migrationFilenameForVersion(version: Int) =
          s"migrations/test-$version.js"
      }

      testSetupDb.runMigration(2)

      DatabaseMeta.find("bacon") match {
        case Some(metaRecord) =>
          metaRecord.value match {
            case ArbitraryData(data) =>
              data should equal ("is awesome")

            case _ =>
              fail("The meta record created didn't have an ArbitraryDataValue.")
          }

        case None =>
          fail("Meta record the migration should have created was not found.")
      }
    }
  }
}
