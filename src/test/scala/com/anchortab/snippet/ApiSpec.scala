package com.anchortab.snippet

import org.scalatest._

import net.liftweb.mockweb._
  import MockWeb._
import net.liftweb.mocks._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.json._
  import Extraction._

import com.anchortab.model._

object ApiSpecExamples {
  lazy val validTab = {
    // TODO
  }

  lazy val disabledTab = {
    // TODO
  }

  val nonAdminUserAuthorizationKey = "JIVEDADDY"
  lazy val nonAdminUser = {
    val user = User(
      email = "stephen@west.com",
      password = "abc123",
      authorizations = UserAuthorizationKey("ApiSpec", nonAdminUserAuthorizationKey) :: Nil
    )

    user.save

    user
  }

  val adminUserAuthorizationKey = "IAMANAWESOMEADMIN123"
  lazy val adminUser = {
    val user = User(
      email = "admin@anchortab.com",
      password = "abc123",
      role = Some("admin"),
      authorizations = UserAuthorizationKey("ApiSpec", adminUserAuthorizationKey) :: Nil
    )

    user.save

    user
  }

  lazy val userToDelete = {
    val user = User(
      email = "delete@me.com",
      password = "abc123"
    )

    user.save

    user
  }
}

class ApiSpec extends FunSpec with ShouldMatchers with BeforeAndAfterAll {
  import ApiSpecExamples._

  MockWeb.useLiftRulesGlobally = true

  override def beforeAll() {
    bootstrap.liftweb.SetupDb.setup

    nonAdminUser
    adminUser
    userToDelete
  }

  override def afterAll() {
    nonAdminUser.delete
    adminUser.delete
    userToDelete.delete
  }

  private def runApiRequest(url: String, authorization: Option[String] = None, requestModifier: (MockHttpServletRequest)=>Any = (req)=>false)(responseHandler: (Box[LiftResponse])=>Any) = {
    val mockReq = new MockHttpServletRequest("http://test.anchortab.com" + url)
    requestModifier(mockReq)

    authorization.foreach { authToken =>
      mockReq.headers = Map("Authorization" -> List("Bearer " + authToken))
    }

    testS(mockReq) {
      testReq(mockReq) {req =>
        Authentication.earlyInStateless(Full(req))

        responseHandler(Api(req)())
      }
    }
  }

  private def invalidCredentialsTest(url: String, requestModifier: (MockHttpServletRequest)=>Any = (req)=>false) = {
    runApiRequest(url, Some("ZZZZZZZZZ")) { response =>
      response match {
        case Full(InMemoryResponse(data, _, _, code)) =>
          code should equal (401)
          (new String(data)) should equal ("Authentication Failed.")

        case _ => fail()
      }
    }
  }

  private def nonAdminCredentialsTest(url: String, requestModifier: (MockHttpServletRequest)=>Any = (req)=>false) = {
    runApiRequest(url, Some(nonAdminUserAuthorizationKey)) { response =>
      response match {
        case Full(InMemoryResponse(data, _, _, code)) =>
          code should equal (403)
          (new String(data)) should equal ("Not authorized.")

        case _ => fail()
      }
    }
  }

  private def noCredentialsTest(url: String, requestModifier: (MockHttpServletRequest)=>Any = (req)=>false) = {
    runApiRequest(url) { response =>
      response match {
        case Full(InMemoryResponse(data, _, _, code)) =>
          code should equal (401)
          (new String(data)) should equal ("Authentication Failed.")

        case _ => fail()
      }
    }
  }

  describe("GET /api/v1/embed") {
    it("should return tab JSON and increment the view count for valid tab information") (pending)

    it("should return a 404 and error for invalid tab information") (pending)

    it("should return a 403 and error if the tab has been disabled") (pending)

    it("should return a 400 and error if the callback parameter wasn't specified") (pending)
  }

  describe("GET /api/v1/embed/*/submit") {
    it("should record the submission in the database for a valid, enabled tab") (pending)

    it("should return a 404 and error for invalid tab information") (pending)

    it("should return a 403 and error for a missing callback") (pending)

    it("should return a 403 and error for a missing email") (pending)
  }

  describe("GET /api/v1/user") {
    it("should return the user as a JSON object for valid user credentials") {
      runApiRequest("/api/v1/user", Some(nonAdminUserAuthorizationKey)) { response =>
        response match {
          case Full(JsonResponse(json, _, _, code)) =>
            code should equal (200)

            implicit val formats = DefaultFormats
            val userJvalue = Serialization.read[JValue](json.toJsCmd)
            val diff = userJvalue.diff(nonAdminUser.asJson)

            diff.changed should equal(JNothing)
            diff.deleted should equal(JNothing)

          case somethingElse => fail(somethingElse.toString)
        }
      }
    }

    it("should return a 401 for no user credentials") {
      noCredentialsTest("/api/v1/user")
    }

    it("should return a 401 for invalid user credentials") {
      invalidCredentialsTest("/api/v1/user")
    }
  }

  describe("GET /api/v1/tab/*") {
    it("should return the tab as JSON for valid credentials and tab ID") (pending)

    it("should return a 404 for valid credentials and an invalid tab ID") (pending)

    it("should return a 401 for invalid credentials and an invalid tab ID") (pending)

    it("should return a 401 for invalid credentials and a valid tab ID") (pending)

    it("should return a 404 for valid non-admin credentials and a another user's valid tab ID") (pending)

    it("should return the tab as JSON for valid admin credentials and another user's valid tab ID") (pending)
  }

  describe("PUT /api/v1/tab/*/appearance") {
    it("should update the appearance information for the tab w/ valid creds/tab ID") (pending)

    it("should return a 401 for invalid credentails and valid tab ID") (pending)

    it("should return a 404 for valid non-admin credentials and other user's tab ID") (pending)

    it("should update the appearance of another user's tab w/ admin credentials") (pending)
  }

  describe("GET /api/v1/admin/users") {
    it("should return all users as JSON for valid admin credentials") {
      runApiRequest("/api/v1/admin/users", Some(adminUserAuthorizationKey)) { response =>
        response match {
          case Full(JsonResponse(json, _, _, code)) =>
            code should equal (200)

            implicit val formats = DefaultFormats
            val usersJvalue = Serialization.read[JValue](json.toJsCmd)
            val diff = usersJvalue.diff(decompose(User.findAll.map(_.asJson)))

            diff.changed should equal(JNothing)
            diff.deleted should equal(JNothing)
          
          case somethingUnexpected => fail(somethingUnexpected.toString)
        }
      }
    }

    it("should return a 401 and error for no credentials") {
      noCredentialsTest("/api/v1/admin/users")
    }

    it("should return a 401 and error for invalid credentials") {
      invalidCredentialsTest("/api/v1/admin/users")
    }

    it("should return a 403 and error for valid non-admin credentials") {
      nonAdminCredentialsTest("/api/v1/admin/users")
    }
  }

  describe("POST /api/v1/admin/users") {
    it("should add a new user and return its ID for valid admin credentials and params") (pending)

    it("should return a 400 for invalid user JSON in body") (pending)

    it("should return a 401 for invalid credentials") {
      invalidCredentialsTest("/api/v1/admin/users", (req) => req.method = "POST")
    }

    it("should return a 403 for valid non-admin credentials") {
      nonAdminCredentialsTest("/api/v1/admin/users", (req) => req.method = "POST")
    }
  }

  describe("GET /api/v1/admin/user/*") {
    it("should return a user as JSON for valid admin credentials and valid user id") {
      runApiRequest("/api/v1/admin/user/" + nonAdminUser._id, Some(adminUserAuthorizationKey)) { response =>
        response match {
          case Full(JsonResponse(json, _, _, code)) =>
            code should equal (200)

            implicit val formats = DefaultFormats
            val userJvalue = Serialization.read[JValue](json.toJsCmd)
            val diff = userJvalue.diff(nonAdminUser.asJson)

            diff.changed should equal(JNothing)
            diff.deleted should equal(JNothing)

          case somethingUnexpected => fail(somethingUnexpected.toString)
        }
      }
    }

    it("should return a 401 for invalid credentials and valid user id") {
      invalidCredentialsTest("/api/v1/admin/user/" + nonAdminUser._id)
    }

    it("should return a 401 for invalid credentials and invalid user id") {
      invalidCredentialsTest("/api/v1/admin/user/BACON")
    }

    it("should return a 403 for valid non-admin credentials and valid user ID") {
      nonAdminCredentialsTest("/api/v1/admin/user/" + adminUser._id)
    }

    it("should return a 403 for valid non-admin credentials and an invalid user ID") {
      nonAdminCredentialsTest("/api/v1/admin/user/BACON")
    }

    it("should return a 404 for valid credentials and invalid user ID") {
      runApiRequest("/api/v1/admin/user/BACON", Some(adminUserAuthorizationKey)) { response =>
        response match {
          case Full(InMemoryResponse(data, _, _, code)) =>
            code should equal (404)
            (new String(data)) should equal ("User not found.")

          case somethingUnexpected => fail(somethingUnexpected.toString)
        }
      }
    }
  }

  describe("DELETE /api/v1/admin/user/*") {
    it("should delete a user for a valid id and admin credentials") {
      runApiRequest("/api/v1/admin/user/" + userToDelete._id.toString, Some(adminUserAuthorizationKey), _.method = "DELETE") { response =>
        response match {
          case Full(OkResponse()) =>
            (User.find(userToDelete._id)) should equal (None)

          case _ => fail()
        }
      }
    }

    it("should return a 401 for invalid credentials and a valid user id") {
      invalidCredentialsTest("/api/v1/admin/user/" + nonAdminUser._id, (req) => req.method = "DELETE")
    }

    it("should return a 401 for invalid credentials and an invalid user id") {
      invalidCredentialsTest("/api/v1/admin/user/BACON", (req) => req.method = "DELETE")
    }

    it("should return a 403 for valid non-admin credentials and a valid user id") {
      nonAdminCredentialsTest("/api/v1/admin/user/" + nonAdminUser._id, (req) => req.method = "DELETE")
    }

    it("should return a 403 for valid non-admin credentials and an invalid user id") {
      nonAdminCredentialsTest("/api/v1/admin/user/BACON", (req) => req.method = "DELETE")
    }

    it("should return a 404 for valid credentials and an invalid user id") {
      runApiRequest("/api/v1/admin/user/BACON", Some(adminUserAuthorizationKey), (req) => req.method = "DELETE") { response =>
        response match {
          case Full(InMemoryResponse(data, _, _, code)) =>
            code should equal (404)
            (new String(data)) should equal ("User not found.")

          case somethingUnexpected => fail(somethingUnexpected.toString)
        }
      }
    }
  }
}
