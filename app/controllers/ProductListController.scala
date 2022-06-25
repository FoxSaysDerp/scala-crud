package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import play.api.libs.json._
import scala.collection.mutable

import models._


@Singleton
class ProductListController @Inject()(val controllerComponents: ControllerComponents)
extends BaseController {
   
   private val productList = new mutable.ListBuffer[Product]()
   productList += Product(1, "GOODRAM IRDM X DDR4 16GB 3200MHz", 10, 5908267961186,true)
   productList += Product(2, "MSI RTX 3080Ti SUPRIM X 12GB GDDR6X", 2, 4719072813390,true)
   productList += Product(3, "XPG SUMMONER SILVER CHERRY-MX RGB LED", 30, 4710273772295, false)
   productList += Product(4, "Fractal Design Define R5 Black", 25, 7350041082583, true)

   implicit val productListJson = Json.format[productList]


      def getAll(): Action[AnyContent] = Action {
         if (productList.isEmpty) {
            NoContent
         } else {
            Ok(Json.toJson(productList))
         }
      }

      def getById(itemId: Long) = Action {
         val foundItem = productList.find(_.id == itemId)
         foundItem match {
            case Some(item) => Ok(Json.toJson(item))
            case None => NotFound
         }
      }

      def hideById(itemId: Long) = Action {
         val foundItem = productList.find(_.id == itemId)
         foundItem match {
            case Some(item) => {
               productList -= item
               val newItem = Product(item.id, item.description, false)
               productList += newItem
               Ok(Json.toJson(productList))
            }
            case None => NotFound
         }
      }

      def showById(itemId: Long) = Action {
         val foundItem = productList.find(_.id == itemId)
         foundItem match {
            case Some(item) => {
               productList -= item
               val newItem = Product(item.id, item.name, item.qty, item.ean, true)
               productList += newItem
               Ok(Json.toJson(productList))
            }
            case None => NotFound
         }
      }

      def editProduct() = Action { implicit request => 
         val content = request.body 
         val jsonObject = content.asJson 
         val productItem: Option[Product] = 
            jsonObject.flatMap( 
               Json.fromJson[Product](_).asOpt 
            )

         productItem match {
         case Some(newItem) =>
            val oldItem = productList.find(_.id == newItem.id)
            productList -= oldItem
            val newItemToAdd = Product(newItem.id, newItem.name, newItem.qty, newItem.ean, false)
            productList += newItemToAdd
            Created(Json.toJson(newItemToAdd))
         case None =>
            BadRequest
         }
      }

      def removeById(itemId: Long) = Action {
         val foundItem = productList.find(_.id == itemId)
         foundItem match {
            case Some(item) => {
               productList -= item
               Ok(Json.toJson(productList))
            }
            case None => NotFound
         }
      }

      def addNewProduct() = Action { implicit request => 
         val content = request.body 
         val jsonObject = content.asJson 
         val productItem: Option[NewProduct] = 
            jsonObject.flatMap( 
               Json.fromJson[NewProduct](_).asOpt 
            )

         productItem match {
         case Some(newItem) =>
            val nextId = productList.map(_.id).max + 1
            val toBeAdded = Product(nextId, newItem.name, newItem.qty, newItem.ean, false)
            productList += toBeAdded
            Created(Json.toJson(toBeAdded))
         case None =>
            BadRequest
         }
      }

}