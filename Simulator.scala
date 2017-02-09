
/*
  @author Praneeth Rajput
 */
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import akka.actor._
import akka.io._
import akka.util.Timeout
import spray.client.pipelining._
import spray.http._
import spray.http.HttpCharsets._
import spray.http.HttpMethods._
import spray.http.MediaTypes._
import spray.httpx.unmarshalling._
import spray.httpx._
import org.json4s.native.Serialization.{ read, write, writePretty }
import java.security.spec.X509EncodedKeySpec
import java.security._
import javax.crypto.spec.SecretKeySpec
import javax.crypto.{Cipher, KeyGenerator, SecretKey}
import java.util.Base64
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object Simulator extends App{
	import FacebookProtocol._

	implicit val simulatorSystem = ActorSystem("Simulator")

	var attributeHelper = new AttributeCoordinator()
	var userBuffer = new ArrayBuffer[String]
			var userMap = Map[String, ActorRef]()
			var pageBuffer = new ArrayBuffer[String]
					registerUsers(20)
					//	  createPage()

					private def registerUsers(fbUserCount: Int){

		val vectorList = retrieveGenericProfileList()

				for(i <- 1 to fbUserCount){

					val baseProfile = vectorList(Random.nextInt(vectorList.size))
							val (age, friendCount) = attributeHelper.returnAgeFriendAttributes(i)
							var fbId: String = baseProfile.name.split(" ")(0) + String.valueOf(i)


							var userProfile = UserProfile(baseProfile.name, String.valueOf(age), baseProfile.fromLoc, baseProfile.currentLoc, baseProfile.email,
									baseProfile.contactNo,fbId)
									userBuffer += userProfile.id
									var actorRef: ActorRef = simulatorSystem.actorOf(Props(new Client(fbId)), fbId)
									userMap += (fbId -> actorRef)
									val pipeline: HttpRequest => Future[UserProfile] = sendReceive ~> unmarshal[UserProfile]
											val f: Future[UserProfile] = pipeline(Put("http://localhost:8000/createUser", userProfile))
											f.onComplete { 
											case Success(userProfile) => {
												println("Registered User " + userProfile.name + " with ID: " +userProfile.id)
												if(i == fbUserCount){                          
													buildNetwork()
												}							
											}
											case Failure(ex) => {
												println(ex)
											}
									}
				}
	} 

	private def buildNetwork(){

		for(i <- 0 until userBuffer.size){

			var friend = Friend(userBuffer(i))

					val pipeline: HttpRequest => Future[Friend] = sendReceive ~> unmarshal[Friend]
							val f: Future[Friend] = pipeline(Post("http://localhost:8000/buildNetwork", friend))
							f.onComplete{
							case Success(result) =>{
								println("Built Friend Network successfully for User Id: " + result.id)
								if(i == userBuffer.size - 1)
								{
									for(i <- 0 until userBuffer.size)
										userMap(userBuffer(i)) ! PublishPublicKeyToServer
										for(i <- 0 until userBuffer.size){
											simulatorSystem.scheduler.schedule(15 seconds, 5 seconds, userMap(userBuffer(i)), RetrieveFriendsPosts(userMap))                      
										}

								}
							}
							case Failure(ex) =>{
								println("Failure while building Network" +ex)								
							}

					}
		}
	}

	private def createPage(){
		for(i <- 0 until 200){

			var pageId: String = "page" + i
					pageBuffer += pageId
					val pipeline: HttpRequest => Future[List[String]] = sendReceive ~> unmarshal[List[String]]
							val f: Future[List[String]] = pipeline(Post("http://localhost:8000/createPage",pageId))
							f.onComplete{
							case Success(result) =>{
								println("Page with page Id " + pageId +" created succcesffuly")
							}
							case Failure(result) =>{
								println(result)
							}
					}
		}
	}


	private def retrieveGenericProfileList(): Vector[BaseProfile] = {

			var baseProfileList = Vector[BaseProfile]()

					baseProfileList = baseProfileList :+ new BaseProfile("Chandler Bing", "Chicago", "California", "352-670-8765", "chandlerBing@fb.com")
			baseProfileList = baseProfileList :+ new BaseProfile("Joey Tribbiani", "New York", "California", "352-456-3456", "joeyTribbian@fb.com")
			baseProfileList = baseProfileList :+ new BaseProfile("Phoebe Buffay", "Florida", "California", "354-567-6444", "phoebeBuffay@fb.com")
			baseProfileList = baseProfileList :+ new BaseProfile("Ross Geller", "Colarado", "California", "342-345-2224", "rossGeller@fb.com")
			baseProfileList = baseProfileList :+ new BaseProfile("Monica Geller", "Colarado", "California", "354-223-5566", "monicaGeller@fb.com")
			baseProfileList = baseProfileList :+ new BaseProfile("Rachel Greene", "Ohio", "California", "345-566-3445", "rachelGreene@fb.com")
			baseProfileList = baseProfileList :+ new BaseProfile("Tyrion Lannister", "Casterly Rock", "Westeros", "435-566-3456", "tyrionLannister@fb.com")
			baseProfileList = baseProfileList :+ new BaseProfile("Barney Stinson", "Boston", "New York", "455-344-2445", "barneyStinson@fb.com")


			baseProfileList
	}

}

class Client(id: String) extends Actor {
	import FacebookProtocol._

	implicit val simulatorSystem = ActorSystem("Client")
	var friends = new ArrayBuffer[String]()
	var friendsMap = Map[String, ActorRef]()
	var userId:String = id

	val AESCipher : Cipher = Cipher.getInstance("AES/ECB/PKCS5PADDING")
	val RSACipher : Cipher = Cipher.getInstance("RSA/ECB/PKCS1PADDING")
	val keyGeneratorInstance: KeyPairGenerator = KeyPairGenerator.getInstance("RSA")
	keyGeneratorInstance.initialize(2048)
	val keyPair: KeyPair = keyGeneratorInstance.generateKeyPair()
	val privateKey: PrivateKey = keyPair.getPrivate
	val publicKey: PublicKey = keyPair.getPublic
	val secretKeyGenerator: KeyGenerator = KeyGenerator.getInstance("AES")
	secretKeyGenerator.init(128)
	var secretKey: SecretKey = secretKeyGenerator.generateKey()
	val publicKeyString: String = Base64.getUrlEncoder().encodeToString(publicKey.getEncoded)


	def receive = {

	case PostMessageToFriend => {

		val pipeline: HttpRequest => Future[FriendToPost] = sendReceive ~> unmarshal[FriendToPost]
				val p: Future[FriendToPost] = pipeline(Get("http://localhost:8000/getFriendToPost/" + userId))
				p.onComplete {
				case Success(result) =>{
					println("Retrieved friend for user " +userId +" and friend is" +result.id)
					if(!result.publicKey.equalsIgnoreCase("") && !result.publicKey.isEmpty()
							&& !result.id.equalsIgnoreCase("") && !result.id.isEmpty()){    
						println("Posting to friend with Id: " + result.id)

						friends += result.id
						var messagePost: String = generateRandomMessage
						secretKey = secretKeyGenerator.generateKey()
						val encryptedMessagePost: String = encrypt(messagePost, secretKey)
						var decryptBytes: Array[Byte] = decrypt(encryptedMessagePost, secretKey)
						var decryptedMessageString: String = new String(decryptBytes, "utf-8")

						var publicKeyBytes: Array[Byte] = Base64.getUrlDecoder.decode(result.publicKey)
						var keySpec: X509EncodedKeySpec = new X509EncodedKeySpec(publicKeyBytes)
						val keyFactory = KeyFactory.getInstance("RSA")
						val publickey: PublicKey = keyFactory.generatePublic(keySpec)
						val keyEncyptedRSAPublic: Array[Byte] = encryptPublicKey(secretKey,publickey)
						val enSecret: String = Base64.getUrlEncoder().encodeToString(keyEncyptedRSAPublic)
						var timeStamp: String = System.currentTimeMillis().toString()

						val unencryptedMessagePost: UnencryptedMessagePost = UnencryptedMessagePost(userId, result.id, messagePost, timeStamp)
						println(writePretty(unencryptedMessagePost))
						var postMessage: MessagePost = MessagePost(userId, result.id, encryptedMessagePost,enSecret, timeStamp)

						val pipeline: HttpRequest => Future[MessagePost] = sendReceive ~> unmarshal[MessagePost]
								val future: Future[MessagePost] = pipeline(Put("http://localhost:8000/postMessage", postMessage))
								future.onComplete { 
								case Success(result) =>{
									println("Message Posted successfully from " + result.fromUser +" to " + result.toUser)
								}
								case Failure(result) =>{
									println("Failed to post message with exception ", result)
								}
						}

					}
				}
				case Failure(exception) =>{
					println("Exception while requesting friends public key for posting message", exception)
				}
		}

	}

	case FriendsPost(id: String, message: String, key: Array[Byte]) => {

		var aesKey: SecretKey = decryptPrivateKey(key)
				if(aesKey != null){
					var messageByte: Array[Byte] = decrypt(message, aesKey)
							var messageEncrypted: String = new String(messageByte, "utf-8")

					println(" Message retrieved from friends profile - Decrypted - " + messageEncrypted)
				}			

	}

	case RespondWithPosts(id: String, key: String, actorRef: ActorRef) => {

		val decoder = Base64.getUrlDecoder
				val byteKey: Array[Byte] = decoder.decode(key)

				val keySpec: X509EncodedKeySpec = new X509EncodedKeySpec(byteKey)
				val keyFactory: KeyFactory = KeyFactory.getInstance("RSA")
				val publickey: PublicKey = keyFactory.generatePublic(keySpec)

				val pipeline: HttpRequest => Future[List[MessagePost]] = sendReceive ~> unmarshal[List[MessagePost]]
						val future: Future[List[MessagePost]] = pipeline(Get("http://localhost:8000/" + userId + "/getPost"))
						future.onComplete { 

						case Success(result) => {

							if(!result.isEmpty){
								println("Latest post retrieved from user " +userId)
								println(writePretty(result(0)))
								var byteKey: Array[Byte] = Base64.getUrlDecoder.decode(result(0).key)
								var secretKey: SecretKey = decryptPrivateKey(byteKey)
								if(secretKey != null){
									var encryptPublic: Array[Byte] = encryptPublicKey(secretKey, publickey)
											var keyString: String = Base64.getUrlEncoder.encodeToString(encryptPublic)
											actorRef ! FriendsPost(userId, result(0).message, encryptPublic)
								}

							}						

						}case Failure(result) =>{

						}
				}
	}

	case RetrieveFriendsPosts(userMap: Map[String, ActorRef]) => {
		if(!friends.isEmpty){
			var randomUserIndex: Int = Random.nextInt(friends.size)
					var randomFriend: String = friends(randomUserIndex)
					println("Retrieving Posts for friend " + randomFriend)
					userMap(randomFriend) ! RespondWithPosts(userId, publicKeyString, self)
		}

	}

	case PublishPublicKeyToServer => {

		val pipeline: HttpRequest => Future[ReturnJsonResponse] = sendReceive ~> unmarshal[ReturnJsonResponse]
				val p: Future[ReturnJsonResponse] = pipeline(Put("http://localhost:8000/" + userId + "/addPublicKey/" + publicKeyString))
				p.onComplete { 
				case Success(result) => {
					println(result.response)
					//         simulatorSystem.scheduler.schedule(7 seconds, 5 seconds, self, PostMessageToFriend) 
					self ! PostMessageToFriend
				}
				case Failure(exception) => {
					println("Exception while publishing public key to server" + exception)    
				}
		}
	}

	case Authenticate => {

		val pipeline: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]
				val future: Future[String] = pipeline(Get("http://localhost:8000/authenticate/" + userId))
				future.onComplete {
				case Success(result) =>
				{
					self ! AuthorizeUser(result)
				}
				case Failure(result) =>
				println("Error while authentication of user " + userId)
		}
	}

	case AuthorizeUser(randomId: String) => {

		val pipeline: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]
				var postRequest:Authorize = Authorize(generateAuthID(randomId))
				val future: Future[String] = pipeline(Post("http://locahost:8000/authorize/", postRequest))
				future.onComplete {
				case Success(result) =>
				{
					println("User authorized")
				}

				case Failure(result) =>
				{
					println("User Authorization failed")
				}
				}
	}

	case _ =>
	println("Undefined action for the client")
	}

	private def encryptPublicKey(secretKey: SecretKey, publicKey: PublicKey): Array[Byte] = {
		RSACipher.init(Cipher.ENCRYPT_MODE, publicKey)
		RSACipher.doFinal(secretKey.getEncoded)
	}

	private def decryptPrivateKey(secretKey: Array[Byte]): SecretKey = {
		var key: SecretKeySpec = null
				try{
					RSACipher.init(Cipher.DECRYPT_MODE, privateKey)
					val byteKey: Array[Byte] = RSACipher.doFinal(secretKey)
					key = new SecretKeySpec(byteKey, "AES")

				}catch{
				case e:Exception =>
				}
	key
	}

	private def encrypt(message:String,secKey: SecretKey): String = {
		var decoder : Base64.Decoder = Base64.getUrlDecoder()
				var arrayBytes : Array[Byte] = message.getBytes("utf-8")
				AESCipher.init(Cipher.ENCRYPT_MODE,secKey)
				var encryptedByte : Array[Byte] = AESCipher.doFinal(arrayBytes)
				var encoder : Base64.Encoder = Base64.getUrlEncoder
				var encyptedMessage : String = encoder.encodeToString(encryptedByte)
				var unencodedMessage : Array[Byte] = decoder.decode(encyptedMessage)
				return encyptedMessage

	}

	private def decrypt(encryptedMessage:String,secKey: SecretKey): Array[Byte] = {
		var decoder : Base64.Decoder = Base64.getUrlDecoder()
				var encyptedMessageInBytes : Array[Byte] = decoder.decode(encryptedMessage)
				AESCipher.init(Cipher.DECRYPT_MODE,secKey)
				var decryptedByte : Array[Byte] = AESCipher.doFinal(encyptedMessageInBytes)
				return decryptedByte

	}

	private def generateRandomMessage : String ={    
		var randomMessage: String = "Hello how are you?"
				randomMessage
	}
  
  private def generateAuthID(randomId: String): String = {

    var authId: String = "AUTH_TOKEN"
        authId
  }

}

class PostBot(userList: ArrayBuffer[String], userMap: Map[String, ActorRef],pageList: ArrayBuffer[String]) extends Actor {
	import FacebookProtocol._

	var users: ArrayBuffer[String] = userList
	var pages: ArrayBuffer[String] = pageList
	import scala.concurrent.ExecutionContext.Implicits.global

	def receive = {
	case StartPosting => {
		for (i <- 0 until 20) {
			triggerMessagePostCapsule()
			Thread.sleep(5000)
		}
	}

	case PagePosting => {
		for(i <- 0 until 20)
			triggerPagePostCapsule
			Thread.sleep(5000)
	}
}

def triggerMessagePostCapsule() = {
	for (j <- 0 until 200) {
		var user = users(Random.nextInt(users.length))
				var actorRef: ActorRef = userMap(user)
				actorRef ! InitiatePostMessage
				var time = String.valueOf(System.currentTimeMillis())
				var messagePost =  MessagePost(user, " ", time,"sds", "Sample Message")
				val pipeline: HttpRequest => Future[MessagePost] = sendReceive ~> unmarshal[MessagePost]
						val f1: Future[MessagePost] = pipeline(Put("http://127.0.0.1:8000/postMessage", messagePost))
						f1.onComplete {
						case Success(result) => {
							println("Message posted successfully from " + result.fromUser + " to user " + result.toUser)
							println(writePretty(result))
						}
						case Failure(ex1) => {
							println(ex1)
						}
				}
	}
}

def triggerPagePostCapsule() = {

	for(j <- 0 until 200){

		var pageId = pageList(Random.nextInt(pageList.size))
				var time = String.valueOf(System.currentTimeMillis())
				var pagePost = PagePost(pageId,"Sample Page Post", time)
				val pipeline: HttpRequest => Future[PagePost] = sendReceive ~> unmarshal[PagePost]
						val f1: Future[PagePost] = pipeline(Put("http://127.0.0.1:8000/postToPage", pagePost))
						f1.onComplete { 
						case Success(result) =>{
							println("Message post to page " + pageId + " Successfully")
							println(writePretty(pagePost))
						}
						case Failure(result) =>{
							println(result)
						}
				}

	}
}
}

/*
 *  The below Attribute Co ordinator was arrived at as part of group discussion : With group strength equal to 4. : Citation
 *  Citation: AttributeCoordinator and Attribute: The basis of the idea has been discussed and arrived at
 *
 */
class AttributeCoordinator() {

	var fCount: Double = 0
			var attributeGroup  = new ArrayBuffer[Attribute]
					var mCount: Double = 0

					attributeGroup += new Attribute(13, 19, 0.16, 15)
			attributeGroup += new Attribute(19, 29, 0.39, 30)
			attributeGroup += new Attribute(29, 41, 0.23, 20)
			attributeGroup += new Attribute(41, 55, 0.13, 22)
			attributeGroup += new Attribute(55, 65, 0.06, 27)
			attributeGroup += new Attribute(65, 100, 0.03, 30)



			def returnAgeFriendAttributes(userCount: Int) = {
				var currentAttribute : Attribute = null
						var isMinor = true
						var rn = 0
						var minorAttributes = new ArrayBuffer[Attribute]

								for(Attribute <- attributeGroup){
									if(Attribute.count == 0 || ((Attribute.count / userCount) < Attribute.ratio ) || userCount == 0 ){
										minorAttributes += Attribute
									}
								}
			if(minorAttributes.length > 0){
				currentAttribute = minorAttributes(Random.nextInt(minorAttributes.length-1))
			}
			else{
				currentAttribute = attributeGroup(Random.nextInt(attributeGroup.length-1))
			}
			currentAttribute.count += 1

					((currentAttribute.start + Random.nextInt(currentAttribute.end - currentAttribute.start)), currentAttribute.getListSize())
			}  
}


class Attribute (attributeStart: Int, attributeEnd: Int, attributeratio: Double, attributeLimit : Int){
	var threshold = 2 * attributeLimit
			var count = 0
			var average  = 0.0
			var start = attributeStart
			var end = attributeEnd
			var ratio = attributeratio
			var limit = attributeLimit

			def getListSize():Int = {
					var tempVar = 0
							if(average < limit){
								tempVar = (Math.ceil(average)).toInt + Random.nextInt(threshold-((Math.ceil(average)).toInt))
							}
							else{
								tempVar = Random.nextInt((Math.ceil(average)).toInt)
							}
					average = ((average*(count-1).toDouble + tempVar.toDouble)/count.toDouble)
							return tempVar
			}

}