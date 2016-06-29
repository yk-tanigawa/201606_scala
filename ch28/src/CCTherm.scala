abstract class CCTherm {
  val description: String
  val yearMade: Int
  val dateObtained: String
  val bookPrice: Int
  val purchasePrice: Int
  val condition: Int
  override def toString = description

  def toXML = { <cctherm>
    <description>{description}</description>
    <yearMade>{yearMade}</yearMade>
    <dateObtained>{dateObtained}</dateObtained>
    <bookPrice>{bookPrice}</bookPrice>
    <purchasePrice>{purchasePrice}</purchasePrice>
    <condition>{condition}</condition>
    </cctherm>
  }

}

object CCTherm {
  def fromXML(node: scala.xml.Node): CCTherm =
    new CCTherm {
      val description   = (node \ "description").text
      val yearMade      = (node \ "yearMade").text.toInt
      val dateObtained  = (node \ "dateObtained").text
      val bookPrice     = (node \ "bookPrice").text.toInt
      val purchasePrice = (node \ "purchasePrice").text.toInt
      val condition     = (node \ "condition").text.toInt
    }
}

object CCThermTest extends App {
  val therm = new CCTherm {
     val description = "hot dog #5"
     val yearMade = 1952
     val dateObtained = "March 14, 2006"
     val bookPrice = 2199
     val purchasePrice = 500
     val condition = 9
  }
  val fileName = "../data/therm1.xml"
  scala.xml.XML.save(fileName, therm.toXML)
  println(CCTherm.fromXML(scala.xml.XML.loadFile(fileName)))
}
