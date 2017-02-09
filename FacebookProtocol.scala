

import scala.collection.mutable.ListBuffer
import akka.actor.ActorRef
import scala.collection.mutable.ArrayBuffer
import spray.routing.RequestContext

object FacebookProtocol extends JsonSupport {

case class UserProfile(name: String, age: String, fromLoc: String, currentLoc: String, email: String, contactNo: String,id: String) 

case class RegistrationProfile(name: String, age: String, fromLoc: String, currentLoc: String, email: String, contactNo: String)

case class MessagePost(fromUser: String, toUser: String, message: String, key: String, time: String)

case class UnencryptedMessagePost(fromUser: String, toUser: String, message: String, time: String)

case class UserPost(id: String, messages: ListBuffer[MessagePost])

case class Friend(id: String) 

case class Authorize(id: String)

case class AuthorizeUser(id: String)

case class FriendList(id: String, friends: List[Friend]) extends Serializable

case class BaseProfile(name: String, fromLoc: String, currentLoc: String, contactNo: String, email: String)

case object UserExists

case object UserCreated

case object PostSucceeded

case object Authenticate

case object PostFailed

case class FriendsPost(id: String, message: String, key: Array[Byte])

case class RetrieveFriendsPosts(userMap:Map[String, ActorRef])

case class RespondWithPosts(id: String, key: String, actorRef: ActorRef)

case object PostForbidden

case object FriendNetworkEstablished

case object FriendNetworkFailed

case class BuildFriendShip(friend: Friend, actorRef: ActorRef)

case class RegisterFriends(friends: ArrayBuffer[Friend], friendMap: Map[String, ActorRef])

case object StartPosting

case class ReturnFriendAndPublicKey(requestContext: RequestContext, publicKeyMap: scala.collection.mutable.Map[String, String])

case class FriendToPost(id: String, publicKey: String)

case object InitiatePostMessage

case object PostMessageToFriend

case class ReturnJsonResponse(response: String)

case class RetrieveFriends(requestContext: RequestContext)

case class RegisterProfile(userProfile: UserProfile, requestContext: RequestContext)

case class RetrieveProfile(requestContext:RequestContext)

case class Deactivate(requestContext: RequestContext, fbUserMap: scala.collection.mutable.Map[String, ActorRef])

case class RemoveFriend(friend: Friend)

case class AcceptPost(message: MessagePost)

case class RetrieveFollowers(requestContext: RequestContext)

case class AddPost(message: MessagePost, requestContext: RequestContext)

case class PagePost(id: String, message: String, time: String)

case class AddFollowers(pageFollowers: ArrayBuffer[String], followerMap: Map[String, ActorRef])

case class PageResponse(pageFollowers: List[String], posts: List[PagePost])

case class RetrievePage(requestContext: RequestContext)

case class AddPagePost(post: PagePost, requestContext: RequestContext)

case class RetrievePosts(requestContext: RequestContext)

case object PagePosting

case object PublishPublicKeyToServer

}