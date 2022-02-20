package me.frde.fremo

object Html {

}

trait HtmlElement:
  def generate(): String

trait HtmlElementContainer(tag: String, body: HtmlElement*) extends HtmlElement:
  override def generate(): String = s"<${tag}>${body.map(_.generate()).mkString("")}</${tag}>"

case class Body(body: HtmlElement*) extends HtmlElementContainer("body", body: _*)

case class Header(body: HtmlElement*) extends HtmlElementContainer("header", body: _*)

case class Div(body: HtmlElement*) extends HtmlElementContainer("div", body: _*)

case class Text(body: String) extends HtmlElement:
  override def generate(): String = body

object HtmlTest:
  def main() =
    println(
      Body {
        Header {
          Div {
            Text("this is a test")
          }
        }
      }.generate()
    )