package iostreams

object XMLCode extends App {
  val drawXML =
    <drawing>
      <drawable type="transform" transType="translate" value1="0" value2="0">
        <drawable type="rectangle" x="0" y="0" width="200" height="400">
          <color red="0.0" green="0.0" blue="1.0" opacity="1.0"/>
        </drawable>
        <drawable type="text" x="100" y="100">
          <color red="0.0" green="0.0" blue="0.0" opacity="1.0"/>
          <text>This is the text that is drawn.</text>
        </drawable>
      </drawable>
    </drawing>

  val name = "Ginny"
  val age = 16
  val description = "Red hair and second hand textbooks."
  val xmlRecord = <person name={name} age={age.toString}>{description}</person>
  
  val costs = List(8,5,6,9,10)
  val costXML = <costs>{costs.map(c => <cost>{c}</cost>)}</costs>
  
  drawXML \ "drawable"
  drawXML \ "@x"
  drawXML \\ "@x"
  drawXML \ "drawable" \ "drawable"
  (drawXML \ "drawable" \ "drawable")(0) \ "@x"
  (drawXML \ "drawable" \ "drawable").filter(n => (n \ "@type").text=="rectangle") \ "@x"
  (drawXML \ "drawable" \ "drawable").map(n => ((n \ "@x").text, (n \ "@y").text, (n \ "@type").text))
}