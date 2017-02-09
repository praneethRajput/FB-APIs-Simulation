

/**
 * @author Praneeth Rajput
 */
import java.security.SecureRandom
import java.util.Base64

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

import org.json4s.native.Serialization.read
import org.json4s.native.Serialization.write
import org.json4s.native.Serialization.writePretty

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.PoisonPill
import akka.actor.Props
import spray.http.MediaTypes
import spray.http.StatusCodes
import spray.httpx.Json4sSupport
import spray.httpx.marshalling.ToResponseMarshallable.isMarshallable
import spray.routing.HttpService
import spray.routing.HttpServiceActor
import spray.routing.RejectionHandler.Default
import spray.routing.RequestContext
import spray.routing.SimpleRoutingApp
import java.security.spec.X509EncodedKeySpec
import java.security.{KeyFactory, PublicKey, SecureRandom, Signature}
import java.util.Base64
import java.security.spec.{KeySpec, X509EncodedKeySpec}
import javax.crypto.spec.{IvParameterSpec, PBEKeySpec, SecretKeySpec}
import javax.crypto.{SecretKeyFactory, Cipher, SecretKey}


class SprayFBInterface extends Actor with SpraySimpleService{
	import FacebookProtocol._

	def receive = runRoute(
			serviceStatus ~
			createUser ~
			postMessage ~
			buildNetwork ~
			profile ~
			friends ~ 
			deactivate ~
			createPage ~
			getPage ~
			authenticate ~
			addPublicKey ~
			getPublicKey ~
			authorize ~
      getFriendToPost ~
      getPost
			)  
			def actorRefFactory = context
}

trait SpraySimpleService extends HttpService { actor: Actor =>
import FacebookProtocol._

var fbUserMap = scala.collection.mutable.Map[String, ActorRef]()
var fbUsers = new ArrayBuffer[String]
		var pageIds = new ArrayBuffer[String]
				var pageIdActorMap = scala.collection.mutable.Map[String, ActorRef]()
				var authenticationMap = scala.collection.mutable.Map[String, String]()
				var publicKeyMap = scala.collection.mutable.Map[String, String]()


				val serviceStatus = get{
	path("hello"){
		complete{
			"Facebook Service is Live!"            
		}
	}
}

val authenticate = get{
	path("authenticate" / Segment){ id => requestContext =>
	if(!fbUserMap.contains(id))
		requestContext.complete(id + " is not a registered Facebook User")
		else{
			var randomValue = SecureRandom.getInstance("SHA1PRNG").nextInt()
					authenticationMap += (id -> randomValue.toString())
					requestContext.complete(randomValue.toString())
		}
	}
}

val authorize = post{
	path("authorize" / Segment){ id =>
	entity(as[Authorize]){ authorizeId => requestContext =>
	if(fbUserMap.contains(id)){
		authorizeUser(id, authorizeId.id, requestContext)
	}else
		requestContext.complete(id + " is not a registered Facebook User")
	}
	}
}

val addPublicKey = put{
  respondWithMediaType(MediaTypes.`application/json`)
	path( Segment / "addPublicKey"  /Segment){ (id, publicKey) => requestContext =>
	if(!fbUserMap.contains(id))
		requestContext.complete(ReturnJsonResponse(id + " is not a registered Facebook User"))
		else{
			publicKeyMap += (id -> publicKey)
					requestContext.complete(ReturnJsonResponse("Public Key added for User " +id))
		}

	}
}

val getPublicKey = get{
	path("getPublicKey" /Segment){ id => requestContext =>
	if(fbUserMap.contains(id) && publicKeyMap.contains(id))
		requestContext.complete(publicKeyMap(id))
		else
			requestContext.complete(StatusCodes.BadRequest)

	}
}

val getFriendToPost = get{
  respondWithMediaType(MediaTypes.`application/json`)
  path("getFriendToPost" /Segment){ id => requestContext =>
    returnFriendToPost(id, requestContext)
    
  }
}

val createUser = put{
	respondWithMediaType(MediaTypes.`application/json`)
	path("createUser"){
		entity(as[UserProfile]){ user => requestContext =>
		registerUser(user, requestContext)
		}
	}
}

val buildNetwork = post{
	path("buildNetwork"){
		respondWithMediaType(MediaTypes.`application/json`)
		entity(as[Friend]){ user => requestContext =>
		registerFriendList(user) 
		requestContext.complete(user)
		}
	}
}

val friends = get{ 
	respondWithMediaType(MediaTypes.`application/json`)
	path(Segment / "friends"){ id => requestContext =>
	getFriends(requestContext, id)
	}
}

val deactivate = delete{
	respondWithMediaType(MediaTypes.`application/json`)
	path(Segment / "deactivate"){ id => requestContext =>
	deactivateUser(id, requestContext, fbUserMap)
	}
}

val postMessage = put{
	respondWithMediaType(MediaTypes.`application/json`)
	path("postMessage"){
		entity(as[MessagePost]){ messagePost => requestContext =>
		completePost(messagePost, requestContext)
		}
	}
}

val getPost = get{
	respondWithMediaType(MediaTypes.`application/json`)
	path(Segment / "getPost"){ id => requestContext =>
	getFBPosts(id, requestContext)
	}
}


val profile = get{
	respondWithMediaType(MediaTypes.`application/json`)
	path(Segment / "profile"){ id => requestContext =>
	retrieveProfile(id, requestContext)
	}
}

val postToPage = put{
	respondWithMediaType(MediaTypes.`application/json`)
	entity(as[PagePost]){ pagePost => requestContext =>

	}
}

val createPage = put{
	respondWithMediaType(MediaTypes.`application/json`)
	path(Segment / "createPage"){ id => requestContext =>
	createFBPage(id, requestContext)
	}
}

val getPage = get{
	respondWithMediaType(MediaTypes.`application/json`)
	path(Segment / "getPage"){ id => requestContext =>
	getFBPage(id, requestContext)
	}
}

private def getFBPosts(userId: String, requestContext: RequestContext){
	fbUserMap(userId) ! RetrievePosts(requestContext)
}

private def returnFriendToPost(userId: String, requestContext: RequestContext){
  fbUserMap(userId) ! ReturnFriendAndPublicKey(requestContext, publicKeyMap)
  
}

private def authorizeUser(id: String, hashId: String, requestContext: RequestContext){
	if(decryptKey(hashId, publicKeyMap(id)) == authenticationMap(id))
		requestContext.complete("Authorized")
		else
			requestContext.complete("Unauthorized")
}

private def decryptKey(hashId: String, publicKey: String): String ={

		var publicKeyBytes: Array[Byte] = Base64.getUrlDecoder().decode(publicKey)
				var keySpec: X509EncodedKeySpec = new X509EncodedKeySpec(publicKeyBytes)
		val keyFactory: KeyFactory = KeyFactory.getInstance("RSA")
		val publickey: PublicKey = keyFactory.generatePublic(keySpec)
		var encBytes = java.util.Base64.getDecoder.decode(hashId)
		var cipher:Cipher = Cipher.getInstance("RSA")
		cipher.init(Cipher.DECRYPT_MODE, publickey)
		var decKey = cipher.doFinal(encBytes)
		var decKeyString= new String(decKey)
		return decKeyString


}
private def postOnPage(pagePost: PagePost, requestContext: RequestContext){
	pageIdActorMap(pagePost.id) ! AddPagePost(pagePost, requestContext)
}
private def getFBPage(pageId: String, requestContext: RequestContext){
	pageIdActorMap(pageId) ! RetrievePage(requestContext)
}

private def createFBPage(pageId: String, requestContext: RequestContext){

	var actorRef = actorRefFactory.actorOf(Props( new PageActor(pageId)), pageId)
			var randomFollowerCount = Random.nextInt(fbUsers.length/2);
	pageIdActorMap.put(pageId, actorRef)
	pageIds += pageId
	var pageFollowers = new ArrayBuffer[String]
			var pageFollowerMap = Map[String, ActorRef]()

			for(i <- 1 to randomFollowerCount){

				var randomIndex = Random.nextInt(fbUsers.size)
						if(!pageFollowers.contains(fbUsers(randomIndex)))
						{
							var userId = fbUsers(randomIndex)
									pageFollowers += userId
									pageFollowerMap += (userId -> fbUserMap(userId))
						}
			}
	actorRef ! AddFollowers(pageFollowers, pageFollowerMap)
	requestContext.complete(pageFollowers.toList)

}

private def deactivateUser(id: String, requestContext: RequestContext, fbUserMap: scala.collection.mutable.Map[String, ActorRef]){
	fbUserMap(id) ! Deactivate(requestContext, fbUserMap)
}

private def retrieveProfile(id: String, requestContext: RequestContext){
	fbUserMap(id) ! RetrieveProfile(requestContext)
}

private def getFriends(requestContext: RequestContext, id: String){
	fbUserMap(id) ! RetrieveFriends(requestContext)
}

private def encodeFBId(name: String): Integer = {
		val messageDigest = java.security.MessageDigest.getInstance("SHA-1")
				var temp: Integer = 0
				for (i <- 0 until 16) {
					temp += (messageDigest.digest(name.getBytes)(i) & 0xFF)

				}
		return temp
}

private def registerUser(userProfile: UserProfile, requestContext: RequestContext) = {

	var registered: Boolean = false
			var actorRef = actorRefFactory.actorOf(Props(new Responder(userProfile.id)), userProfile.id)
			fbUsers += userProfile.id
			fbUserMap += (userProfile.id -> actorRef)
			actorRef ! RegisterProfile(userProfile, requestContext)  

}

private def registerFriendList(user: Friend): Boolean = {
	val registered: Boolean = false
			if(fbUsers.contains(user.id)){
				var fbUsersCount = fbUsers.length
						var friends = new ArrayBuffer[Friend]
								var friendsMap = Map[String, ActorRef]()
								var actorRef = fbUserMap(user.id)
								for(i <- 0 until 30){
									var randomUserIndex = Random.nextInt(fbUsersCount)  

											if(fbUsers(randomUserIndex) != user.id && !friends.contains(Friend(fbUsers(randomUserIndex)))){
												var userId: String = fbUsers(randomUserIndex)
														friends += Friend(userId)
														friendsMap += (userId -> fbUserMap(userId))
														fbUserMap(userId) ! BuildFriendShip(user, actorRef)
											}
								}
				actorRef ! RegisterFriends(friends, friendsMap)
			}
registered
}

private def completePost(message: MessagePost, requestContext: RequestContext) {
	fbUserMap(message.toUser) ! AddPost(message, requestContext)
}
}

class PageActor(actorName: String) extends Actor{
	import FacebookProtocol._

	val pageId = actorName
	var followers = new ArrayBuffer[String]
			var posts = new ArrayBuffer[PagePost]
					var followerMap = Map[String, ActorRef]()

					def receive = {

					case AddFollowers(pageFollowers: ArrayBuffer[String], pageFollowerMap: Map[String, ActorRef]) =>
					followers ++=  pageFollowers
					followerMap ++=  pageFollowerMap

					case AddPagePost(pagePost: PagePost, requestContext: RequestContext) =>
					posts += pagePost
					requestContext.complete(pagePost)

					case RetrievePage(requestContext: RequestContext) =>
					var pageResponse = PageResponse(followers.toList, posts.toList)
					requestContext.complete(pageResponse)

					case RetrieveFollowers(requestContext: RequestContext) =>
					requestContext.complete(followers.toList)

	}
}

class Responder(actorName:String) extends Actor{
	import FacebookProtocol._

	val userId = actorName
	var friends = new ArrayBuffer[Friend]
			var friendMap = Map[String, ActorRef]()
			var posts = new ArrayBuffer[MessagePost]
					var profile: UserProfile = null

					def receive = {

					case BuildFriendShip(friend: Friend, actorRef: ActorRef) =>
					if(!friends.contains(friend.id)){
						friends += friend
								friendMap += (friend.id -> actorRef)
					}

					case RegisterFriends(friendsBuffer: ArrayBuffer[Friend], friendsMap: Map[String, ActorRef]) =>
					friends = friendsBuffer
					friendMap = friendsMap

					case RetrieveFriends(requestContext: RequestContext) =>
					requestContext.complete(friends.toList)
          
          case ReturnFriendAndPublicKey(requestContext: RequestContext, publicKeyMap: scala.collection.mutable.Map[String, String]) =>
            var randomUserIndex = Random.nextInt(friends.size)
            var friendId: String = friends(randomUserIndex).id
            var publicKey: String = ""
            
            if(!friends.isEmpty && publicKeyMap.contains(friendId)){
              publicKey = publicKeyMap(friendId)
            }
            println(publicKey + "---" + friendId)
            var response: FriendToPost = FriendToPost(friendId,publicKey)
            requestContext.complete(response)

					case RegisterProfile(userProfile: UserProfile, requestContext: RequestContext) =>
					profile = userProfile
					requestContext.complete(userProfile)

					case RetrieveProfile(requestContext: RequestContext) =>
					requestContext.complete(profile)

					case Deactivate(requestContext, fbUserMap) =>
					friends.foreach { x => 
					fbUserMap(x.id) ! RemoveFriend(Friend(userId))
					}
					requestContext.complete(StatusCodes.Accepted)
					killYourself

					case RemoveFriend(friend: Friend) =>
					friends -= friend

					case RetrievePosts(requestContext: RequestContext) =>
            println(writePretty(posts.toList))
					requestContext.complete(posts.toList)

					case AcceptPost(message: MessagePost) =>
					if(friends.contains(message.fromUser))
						posts += message

					case AddPost(message: MessagePost, requestContext: RequestContext) =>
					posts += message
					requestContext.complete(message)

					case RetrievePage =>

					case _ =>


			}

			private def killYourself = self ! PoisonPill

}