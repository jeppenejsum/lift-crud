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
    if (id == "notfound") Empty else Full(Order(id))
  }
  def encode(o: Order): String = o.id;
  
  def menus: List[Menu] = List(editOp, listOp, createOp, lineItemsOp)

  // This works :-)
  def listOp = Menu.i("listorder") / "orders" >> Loc.Template(() => <span>List orders</span>)

  // This cannot find the snippet (my guess is because it's a Loc[Unit] and the snippet only works for Loc[Order])
  def createOp = Menu.i("neworder") / "orders" / "new"  >> Loc.CalcValue(() => Full(Order("newly created"))) >> Loc.Template(() => <lift:OrderSnippet.edit/>)

  // Parse never seems be called
  def editOp = Menu.param[Order]("editorder", "editorder", parse _, encode _) / "orders" / * >> Loc.ValueTemplate({o:Box[Order] => <lift:OrderSnippet.edit/>}) 

  // Never seems to call parse
  def lineItemsOp = Menu.param[Order]("lineitems", "lineitems", parse _, encode _) / "orders" / * / "lineitems" >> Loc.ValueTemplate({o:Box[Order] => <lift:OrderSnippet.lineItems/>}) 
}

object LineItems {
  def parse(ids: List[String]) = {
    println("Parse line "+ids)
    val order = Order(ids.head)
    val li = LineItem(order, ids.drop(1).head)
    Full((order, li))
  }

  def encode(item: (Order,LineItem)): List[String] = {
    val (order,li) = item
    List(order.id, li.id)
  }

  def menus: List[Menu] = List(/*createOp, */editOp);

  // Cannot figure out correct type signature here: 
  // Loc probably needs to be (Order, LineItem), but only Order can be looked up, LineItem will be created 
  //def createOp = Menu.param[Order]("newlineitem") / "orders" / * / "lineitems" / "new"  >> Loc.CalcValue(() => Full(....) >> Loc.Template(() => <lift:LineItemSnippets.edit/>)

  // Parse never seems to be called
  def editOp = Menu.params[(Order, LineItem)]("editlineitem", "editlineitem", parse _, encode _) / "orders" / * / "lineitems" / * >> Loc.ValueTemplate({o:Box[(Order,LineItem)] => <lift:LineItemSnippets.edit/>}) 
}

class OrderSnippets(order: Order) {
  def edit(in: NodeSeq): NodeSeq = <span>Edit order with id {order.id}</span>

  def lineItems(in: NodeSeq): NodeSeq = <span>Showing line items for order with id {order.id}</span>
}

class LineItemSnippets(parms: (Order,LineItem)) {
  val (order, lineItem) = parms

  def edit(in: NodeSeq): NodeSeq = <span>Edit line item order:{order.id}, item:{lineItem.id}</span>
}
