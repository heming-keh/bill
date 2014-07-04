package models

import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._

import java.util.{ Date, Calendar }

case class Bill(id: Pk[Long], name: String, pay: Double,
  payer: User, date: Date = new Date,
  deleted: Boolean = false)

object Bill {
  def simple = {
    get[Pk[Long]]("bill.id") ~ get[String]("bill.name") ~
      get[Double]("bill.pay") ~
      get[String]("bill.payer") ~ get[Date]("bill.date") ~
      get[Boolean]("bill.deleted") map {
        case id ~ name ~ pay ~ payer ~ date ~ deleted =>
          Bill(id, name, pay, User.findByName(payer).get, date, deleted)
      }
  }

  def findAll: Seq[Bill] = {
    DB.withConnection { implicit connection =>
      SQL("select * from bill").as(Bill.simple *)
    }
  }

  def last: Option[Bill] = {
    DB.withConnection { implicit connection =>
      SQL("select * from bill order by id desc limit 1").as(Bill.simple.singleOpt)
    }
  }

  def find(settlementID: Int): Seq[Bill] = {
    DB.withConnection { implicit connection =>
      if (settlementID < 0) {
        Settlement.last match {
          // 查询待结账的账单，同时账本中没有结账记录
          case None => SQL("select * from bill").as(Bill.simple *)
          // 查询待结账的账单，同时几本中有结账记录
          case Some(settlement) =>
            SQL("select * from bill where id > {id}").
              on('id -> settlement.bill).as(Bill.simple *)
        }
      } else {
        Settlement.findById(settlementID) match {
          // 没有找到ID对应的结账记录
          case None => Seq()
          // 找到ID对应的结账记录
          case Some(settlement) => {
            Settlement.previous(settlementID) match {
              // 当前结账记录之前没有其它的结账记录
              case None =>
                SQL("select * from bill where id <= {id}").on('id -> settlement.bill).as(Bill.simple *)
              // 当前结账记录之前有其它的结账记录
              case Some(previousSettlement) =>
                SQL("select * from bill where id > {previousID} and id <= {id}").
                  on('previousID -> previousSettlement.bill, 'id -> settlement.bill	).as(Bill.simple *)
            }
          }
        }
      }
    }
  }

  
  def create(bill: Bill): Bill = {
    DB.withConnection { implicit connection =>
      // 这种生成ID的方法很麻烦，使用SEQUENCE有什么优势吗，相比AUTO_INCREMENT
      val id: Long = bill.id.getOrElse {
        SQL("select next value for bill_id_seq").as(scalar[Long].single)
      }

      SQL("insert into bill (id, name, pay, payer, date, deleted) values ({id}, {name}, {pay}, {payer}, {date}, {deleted})").
        on('id -> id, 'name -> bill.name, 'pay -> bill.pay, 'payer -> bill.payer.name, 'date -> bill.date,
          'deleted -> bill.deleted).executeInsert()
      bill
    }
  }

  def delete(id: Long, payer: String): Boolean = {
    DB.withConnection { implicit connection =>
      SQL("update bill set deleted = {deleted} where payer = {payer} and id = {id}").on('deleted -> true, 'payer -> payer, 'id -> id).executeUpdate != 0
    }
  }
}