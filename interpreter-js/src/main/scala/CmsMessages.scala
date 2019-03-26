package ltbs.uniform.prototype

import ltbs.uniform.UniformMessages

object CmsMessages {
  def fromText(input: String): UniformMessages[String] = {
    val rawLines = input
      .replace("\\\n"," ")
      .split("\n")      
      .map{_.replace("#.*","").split("=",2).toList}
      .collect {
        case (k::v::Nil) => (k, v.replace("''","'"))
      }

    val groupedAndSorted =
      rawLines.toList
        .groupBy(_._1.replaceAll("[.][0-9]*$",""))
        .mapValues{_.sortBy(_._1).map{_._2}}

    UniformMessages.fromMapWithSubstitutions(groupedAndSorted)
  }

}
