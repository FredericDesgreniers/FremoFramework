package me.frde.fremo

import com.azure.cosmos.models.{CosmosItemRequestOptions, CosmosQueryRequestOptions}
import com.azure.cosmos.{ConsistencyLevel, CosmosClientBuilder}
import com.fasterxml.jackson.annotation.JsonProperty

import java.io.FileReader
import java.util.{Collections, Properties}
import java.util.stream.Collectors
import java.io.File
import java.util

object CosmoDB:
  def loadProperty(name: String) = {
//    println(util.Arrays.toString(System.getenv().keySet().toArray))
    val envVariable = System.getenv(name)
    if envVariable != null then
      println(s"Found ${name} in environment variable: ${envVariable}")
      envVariable
    else
      val properties = new Properties()
      val file = new File("secrets.properties")
      if file.exists() then
        properties.load(new FileReader(file))
        val property = properties.getProperty(name)
        println(s"Property ${name} found in file: ${property}")
        property
      else
        throw new RuntimeException(s"Could not find property ${name} in env variables and no settings file is present")
  }

  val COSMO_SECRET = loadProperty("cosmoSecret");

  val COSMO_HOST = loadProperty("cosmoHost");

  case class Test(@JsonProperty val id: String, @JsonProperty val name: String) {
  }

  val preferredRegions = new java.util.ArrayList[String]
  preferredRegions.add("West US")

  val client = new CosmosClientBuilder().endpoint(COSMO_HOST).key(COSMO_SECRET).preferredRegions(preferredRegions).userAgentSuffix("CosmosDBJavaQuickstart").consistencyLevel(ConsistencyLevel.EVENTUAL).buildClient
  client.createDatabaseIfNotExists("fremo")
  val db = client.getDatabase("fremo")
  db.createContainerIfNotExists("entityStore", "/entityStore")
  val container = db.getContainer("entityStore")

  def put(map: java.util.Map[String, Object]) =
    container.createItem(map)

  def getByRef(ref: String) =
    container.queryItems(s"select * from c where c.id=${ref}", new CosmosQueryRequestOptions(), classOf[java.util.Map[String, Object]]).stream().collect(Collectors.toList)

  def getByProperty(typ: String, name: String, value: String): java.util.List[java.util.Map[String, Object]] = {
    val query = s"select * from c where c.type='${typ}' and c[\"${name}\"][\"value\"]='${value}'"
    println(query)
    container.queryItems(query, new CosmosQueryRequestOptions(), classOf[java.util.Map[String, Object]]).stream().collect(Collectors.toList())
  }

  def getByKey(key: String, typ: String): java.util.List[java.util.Map[String, Object]]=
    val surface = Fremo.currentScenario.get.surface
    val vt = surface.vtEpoch
    val tt = surface.ttEpoch
    val query = s"select * from c where c.key='${key}' and c.type='${typ}' and c.tt <= ${tt} and c.vt <= ${vt} order by c.tt, c.vt ASC"
    println(query)
    container.queryItems(query, new CosmosQueryRequestOptions(), classOf[java.util.Map[String, Object]]).stream().collect(Collectors.toList)

  def close = client.close()

  def main(args: Array[String]): Unit =
    val options = new CosmosItemRequestOptions
    container.createItem(Test("1", "test"), options)
    container.createItem("")
