package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import play.api.libs.json._
import scala.collection.mutable

import models._


@Singleton
class UserListController @Inject()(val controllerComponents: ControllerComponents)
extends BaseController {
   
   private val userList = new mutable.ListBuffer[User]()
   userList += User(1, "peanutbutter03", "peanutbutterlover@gmail.com", "P3n4uTbUtT3R!!")
   userList += User(2, "micheal_scott", "mscott@dundermifflin.com", "iloveholly123")
   userList += User(3, "bigfishsmallhat", "tahmkench@river.com", "MUHv-'t3s<Q+^5")


   implicit val userListJson = Json.format[userList]


      def getAll(): Action[AnyContent] = Action {
         if (userList.isEmpty) {
            NoContent
         } else {
            Ok(Json.toJson(userList))
         }
      }

      def getById(userId: Long) = Action {
         val foundUser = userList.find(_.id == userId)
         foundItem match {
            case Some(user) => Ok(Json.toJson(user))
            case None => NotFound
         }
      }

      def changePassword() = Action { implicit request => 
         val content = request.body 
         val jsonObject = content.asJson 
         val userItem: Option[User] = 
            jsonObject.flatMap( 
               Json.fromJson[User](_).asOpt 
            )

         userItem match {
         case Some(newUserInfo) =>
            val oldUserInfo = userList.find(_.id == newUserInfo.id)
            val newUserInfoToAdd = User(oldUserInfo.id, oldUserInfo.username, oldUserInfo.email, newUserInfo.password)
            userList -= oldUserInfo
            userList += newUserInfoToAdd
            Created(Json.toJson(newUserInfoToAdd))
         case None =>
            BadRequest
         }
      }

      def removeById(userId: Long) = Action {
         val foundUser = userList.find(_.id == userId)
         foundUser match {
            case Some(user) => {
               userList -= user
               Ok(Json.toJson(userList))
            }
            case None => NotFound
         }
      }

      def registerNewUser() = Action { implicit request => 
         val content = request.body 
         val jsonObject = content.asJson 
         val userItem: Option[NewUser] = 
            jsonObject.flatMap( 
               Json.fromJson[NewUser](_).asOpt 
            )

         productItem match {
         case Some(newUser) =>
            val nextId = userList.map(_.id).max + 1
            val toBeAdded = User(nextId, newUser.username, newUser.email, newUser.password)
            userList += toBeAdded
            Created(Json.toJson(toBeAdded))
         case None =>
            BadRequest
         }
      }

}