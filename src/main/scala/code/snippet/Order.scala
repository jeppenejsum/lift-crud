package code 
package snippet 

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.sitemap._
import code.lib._
import Helpers._


case class Order(id: String)
case class LineItem(order: Order, id: String)

object Orders {
  def parse(id: String): Box[Order] = {
    println("Parse order "+id)
    if (id == "notfound" || id == "new") Empty else Full(Order(id))
  }
  def encode(o: Order): String = o.id;
  
  def menus: List[Menu] = List(createOp, editOp, listOp, lineItemsOp, createLineItemOp)

  // This works :-)
  def listOp = Menu.i("listorder") / "orders" >> Loc.Template(() => <span>List orders</span>)

  // To make this work, parse needs to return Empty when called with new
  def createOp = Menu.i("neworder") / "orders" / "new"

  def editOp = Menu.param[Order]("editorder", "editorder", parse _, encode _) / "orders" / * >> Loc.ValueTemplate({o:Box[Order] => <lift:OrderSnippets.edit/>}) 

  def lineItemsOp = Menu.param[Order]("lineitems", "lineitems", parse _, encode _) / "orders" / * / "lineitems" >> Loc.ValueTemplate({o:Box[Order] => <lift:OrderSnippets.lineItems/>}) 

  def createLineItemOp = Menu.param[Order]("newlineitem", "newlineitem", parse _, encode _) / "orders" / * / "lineitems" / "new"  >> Loc.ValueTemplate({o:Box[Order] => <span>New line item for order {o.open_!.id}</span>})
}

object LineItems {
  def parse(ids: List[String]) = {
    println("Parse line "+ids)
    val order = Order(ids.head)
    val li = LineItem(order, ids.drop(1).head)
    if (ids(1) == "notfound") Empty else Full((order, li))
  }

  def encode(item: (Order,LineItem)): List[String] = {
    val (order,li) = item
    List(order.id, li.id)
  }

  def menus: List[Menu] = List(editOp);

  // Parse never seems to be called
  def editOp = Menu.params[(Order, LineItem)]("editlineitem", "editlineitem", parse _, encode _) / "orders" / * / "lineitems" / * >> Loc.ValueTemplate({o:Box[(Order,LineItem)] => <lift:LineItemSnippets.edit/>}) 
}

class OrderSnippets(order: Order) {
  def edit(in: NodeSeq): NodeSeq = <span>Edit order with id {order.id} Create new line item <a href={Orders.createLineItemOp.calcHref(order)}>here</a></span>

  def lineItems(in: NodeSeq): NodeSeq = <span>Showing line items for order with id {order.id}</span>
}

class LineItemSnippets(parms: (Order,LineItem)) {
  val (order, lineItem) = parms

  def edit(in: NodeSeq): NodeSeq = <span>Edit line item order:{order.id}, item:{lineItem.id}</span>
}
