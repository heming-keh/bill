package models

import play.api.db._
// Implicitly import the current running application in the context.
import play.api.Play.current

import anorm._
import anorm.SqlParser._

import java.util.Date

case class Settlement(id: Pk[Int], bill: Long)

object Settlement {
  // maps a database row values to the User case class
  def simple = {
    get[Pk[Int]]("settlement.id") ~ get[Long]("settlement.bill") map {
      case id ~ bill => Settlement(id, bill)
    }
  }

  def findAll: Seq[Settlement] = {
    DB.withConnection { implicit connection =>
      SQL("select * from settlement").as(Settlement.simple *)
    }
  }

  def last: Option[Settlement] = {
    DB.withConnection { implicit connection =>
      SQL("select * from settlement order by bill desc limit 1").
        as(Settlement.simple.singleOpt)
    }
  }

  def findById(id: Int): Option[Settlement] = {
    DB.withConnection { implicit connection =>
      SQL("select * from settlement where id = {id} limit 1").
        on('id -> id).
        as(Settlement.simple.singleOpt)
    }
  }

  // 找出前一个Settlement
  def previous(id: Int): Option[Settlement] = {
    DB.withConnection { implicit connection =>
      SQL("select * from settlement where id < {id} order by bill desc limit 1").
        on('id -> id).
        as(Settlement.simple.singleOpt)
    }
  }
  
  def add(bill: Long): Option[Settlement] = {
    DB.withConnection { implicit connection =>
      SQL("insert into settlement (bill) values ({bill})").
        on('bill -> bill).executeUpdate
      SQL("select * from settlement where bill = {bill}").
        on('bill -> bill).as(Settlement.simple.singleOpt)
    }    
  }

  def create(settlement: Settlement): Option[Settlement] = {
    DB.withConnection { implicit connection =>
      SQL("insert into settlement values ({bill})").
        on('bill -> settlement.bill).executeUpdate
      SQL("select * from settlement where bill = {bill}").
        on('bill -> settlement.bill).as(Settlement.simple.singleOpt)
    }
  }

}
