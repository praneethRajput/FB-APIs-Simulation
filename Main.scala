
import akka.actor._
import akka.io.IO
import akka.pattern.ask
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import spray.can.Http

import scala.concurrent.duration._

object Main extends App {
	val config = ConfigFactory.load()

			implicit val actorSystem = ActorSystem("facebook-rest-service")
			implicit val executionContext = actorSystem.dispatcher
			implicit val timeout = Timeout(10 seconds)

			val service = actorSystem.actorOf(Props[SprayFBInterface], "SprayFBInterface")
			IO(Http).ask(Http.Bind(service, interface = "localhost", port = 8000))
			.mapTo[Http.Event]
					.map {
					case Http.Bound(address) =>
					println("Facebook REST Interface at $address")
	}

}

