package models

import play.api.db._
// Implicitly import the current running application in the context.
import play.api.Play.current

import anorm._
import anorm.SqlParser._

case class User(name: String, password: String)

object User {
  // maps a database row values to the User case class
  def simple = {
    get[String]("user.name") ~ get[String]("user.password") map {
      case name ~ password => User(name, password)
    }
  }

  def findByName(name: String): Option[User] = {
    DB.withConnection { implicit connection =>
      SQL("select * from user where name = {name}").
        on('name -> name).
        as(User.simple.singleOpt)
    }
  }

  def findAll: Seq[User] = {
    DB.withConnection { implicit connection =>
      SQL("select * from user").as(User.simple *)
    }
  }

  def authenticate(name: String, password: String): Option[User] = {
    DB.withConnection { implicit connection =>
      SQL("select * from user where name = {name} and password = {password}").
        on('name -> name, 'password -> password).
        as(User.simple.singleOpt)
    }
  }

  def create(user: User): User = {
    DB.withConnection { implicit connection =>
      SQL("insert into user values ({name}, {password})").
        on('name -> user.name, 'password -> user.password).executeUpdate
      user
    }
  }

  // 修改密码
  def update(user: User): User = {
    DB.withConnection { implicit connection =>
      SQL("update user set password = {password} where name = {name}").
        on('password -> user.password, 'name -> user.name).executeUpdate
      user
    }
  }
}