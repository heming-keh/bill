package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.data.format.Formats._

import play.api.data.validation._
import play.api.data.validation.Constraints._

import play.api.libs.json._

import models.{ User, Bill, Settlement }

import java.util.{ Calendar, Date }

object Application extends Controller with Secured {

  val loginForm = Form(
    tuple("username" -> text, "password" -> text).
      verifying("用户名或密码错误", result => result match {
        case (username, password) =>
          User.authenticate(username, password).isDefined
      }))

  val passwdForm = Form(
    tuple(
      "username" -> text,
      "password" -> text,
      "newpasswd" -> text,
      "newpasswdr" -> text).verifying("旧密码错误或新密码不匹配", result => result match {
        case (username, password, newpasswd, newpasswdr) =>
          User.authenticate(username, password).isDefined && !newpasswd.isEmpty && newpasswd.equals(newpasswdr)
      }))

  val billForm = Form(
    tuple("name" -> text, "pay" -> of[Double]).verifying(
      "账单错误", result => result match {
        case (name, pay) => !name.isEmpty() && pay >= 0
      }))

  // IsAuthenticated接受一个函数作为参数，这个函数根据用户名和请求生成Action
  def index = IsAuthenticated { username =>
    implicit request =>
      User.findByName(username).map { user =>
        val payers = User.findAll.map(_.name)
        val settlements = -1 :: Settlement.findAll.map(_.id.get).toList.reverse
        Ok(views.html.index(payers, settlements))
      }.getOrElse(Forbidden)
  }

  /**
   * 登录页面
   */
  def login = Action { implicit request =>
    Ok(views.html.login(loginForm))
  }

  def authenticate = Action { implicit request =>
    loginForm.bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.login(formWithErrors)),
      user => Redirect(routes.Application.index).withSession("username" -> user._1))
  }

  /**
   * 登出
   */
  def logout = Action {
    Redirect(routes.Application.login).withNewSession
  }

  /**
   * 修改密码页面
   */
  def passwd = IsAuthenticated { username =>
    implicit request =>
      User.findByName(username).map { user =>
        Ok(views.html.passwd(passwdForm.bind(
          // 将当前用户的名称绑定到修改密码的表单，不绑定上面的FORM无法验证
          Map[String, String]("username" -> user.name))))
      }.getOrElse(Forbidden)
  }

  def changepasswd = Action { implicit request =>
    passwdForm.bindFromRequest.fold(
      fromWithErrors => BadRequest(views.html.passwd(fromWithErrors)),
      passwd => {
        User.update(User(passwd._1, passwd._3))
        Redirect(routes.Application.index).withNewSession
      })
  }

  def settlement = IsAuthenticated { username =>
    implicit request =>
      User.findByName(username).map { user =>
        Bill.last match {
          // 无账可结
          case None => Redirect(routes.Application.index)
          // 结账
          case Some(b) =>
            Settlement.last match {
              case None =>
                val settlement = Settlement.add(b.id.get)
                play.api.Logger.info(settlement.toString)
              case Some(s) =>
                // 最后一个记账的编号要比settlement.bill大，否则不需要结账
                if (s.bill < b.id.get) {
                  val settlement = Settlement.add(b.id.get)
                  play.api.Logger.info(settlement.toString)
                }
            }
            Redirect(routes.Application.index)
        }
      }.getOrElse(Forbidden)
  }

  /**
   * 添加账单
   */
  def addBill = IsAuthenticated { username =>
    implicit request =>
      User.findByName(username).map { user =>
        billForm.bindFromRequest.fold(
          formWithErrors => {
            // TODO 很奇怪这里怎么获取不到错误信息
            val errs = formWithErrors.globalError.map { err => err.message }
            Logger.info(errs.getOrElse("none"))
            val json = Json.toJson(errs.getOrElse("none"))

            BadRequest("账单错误")
          },
          bill => {
            // 添加账单
            val name = bill._1
            val pay = bill._2
            // val username = request.session.get("username")
            Bill.create(Bill(anorm.NotAssigned, name, pay, user))
            Ok("添加完成")
          })
      }.getOrElse(Forbidden)
  }

  /**
   * 搜索
   */

  case class Searcher(settlement: Int, payers: List[String], showDeleted: Boolean)
  val searchForm = Form(mapping(
    "settlement" -> number,
    "payers" -> list(text),
    "show_deleted" -> boolean)(Searcher.apply)(Searcher.unapply))

  def search = IsAuthenticated { username =>
    implicit request =>
      User.findByName(username).map { user =>
        searchForm.bindFromRequest.fold(fromWithErrors => {
          BadRequest("查询错误")
        },
          searcher => searcher match {
            case Searcher(settlement, payers, showDeleted) =>
              payers.foreach(x => play.api.Logger.info(x))
              play.api.Logger.info(searcher.toString)
              val bills = Bill.find(settlement)  // 这里的bills是包含已经删除的
              val billsJson = bills.filter(b => payers.contains(b.payer.name)).filter(b => (!b.deleted) || (b.deleted && showDeleted)) map {
                case Bill(id, name, price, billuser, date, deleted) =>
                  Json.toJson(Map("id" -> id.get.toString, "name" -> name, "price" -> price.toString, "user" -> billuser.name,
                    "date" -> (new java.text.SimpleDateFormat("yyyy-MM-dd")).format(date), "deleted" -> deleted.toString,
                    "deleteable" -> billuser.name.equals(user.name).toString));
              }

              val billsNoDeleted = bills.filter(!_.deleted)
              val totalPay = billsNoDeleted.map(_.pay).sum
              val everybodyShouldPay = totalPay / User.findAll.size
              val balanceInfo = f"总开销$totalPay%03.2f元。" :: (User.findAll map {
                case User(payer: String, _) => {
                  val pay = billsNoDeleted.filter(_.payer.name.equals(payer)).map(_.pay).sum
                  val debt = everybodyShouldPay - pay
                  val payStr = f"已花费$pay%03.2f元"
                  val debtStr = if (debt > 0) {
                    "还应再支付%03.2f元给其他室友" format (debt)
                  } else {
                    "还应再从其他室友那里收取%03.2f元" format (debt.abs)
                  }
                  s"${payer}${payStr}，${debtStr}。"
                }
              } toList)

              Ok(Json.toJson(Map("bills" -> Json.toJson(billsJson), "balance" -> Json.toJson(balanceInfo))))
          })

      }.getOrElse(Forbidden)
  }

  /**
   * 删除
   */
  def delete(id: Long) = Action {
    implicit request =>
      request.session.get("username") match {
        case None => BadRequest("需要登录")
        case Some(username) =>
          if (Bill.delete(id, username)) {
            Ok("删除成功")
          } else {
            BadRequest("无权限删除或账单不存在")
          }
      }
  }

}

/**
 * Provide security features
 */
trait Secured {

  /**
   * Retrieve the connected user email.
   */
  private def username(request: RequestHeader) = request.session.get("username")

  /**
   * Redirect to login if the user in not authorized.
   */
  private def onUnauthorized(request: RequestHeader) = Results.Redirect(routes.Application.login)

  /**
   * Action for authenticated users.
   *
   * username -- function used to retrieve the user name from the request header - the default is to read from session cookie.
   * onUnauthorized -- function used to generate alternative result if the user is not authenticated - the default is a simple 401 page
   */
  def IsAuthenticated(f: => String => Request[AnyContent] => Result) =
    Security.Authenticated(username, onUnauthorized) { user =>
      Action(request => f(user)(request))
    }
}