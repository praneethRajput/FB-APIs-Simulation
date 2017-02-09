

/**
 * @author Praneeth Rajput
 */
import org.json4s.DefaultFormats
import org.json4s.Formats
import org.json4s.JsonAST.JArray
import org.json4s.Serializer
import org.json4s.reflect.TypeInfo

import spray.httpx.Json4sSupport

trait JsonSupport extends Json4sSupport {

	implicit def json4sFormats: Formats = DefaultFormats
 

}
  