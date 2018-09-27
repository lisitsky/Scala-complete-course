package lectures.functions

import java.lang.StackWalker

import scala.util.Random

/**
  * Эта задача имитирует авторизацию в интернет банке.
  * Авторизоваться можно 2-я способами. Предоставив карту или логин/пароль
  * Вам дан список зарегистрированных банковских карт и
  * AuthenticationData.registeredCards
  * и список зарегистрированных логинов/паролей
  * AuthenticationData.registeredLoginAndPassword
  *
  * Ваша задача, получая на вход приложения список тестовых юзеров
  * AuthenticationData.testUsers
  * Оставить в этом списке только тех пользователей, чьи учетные данные
  * совпадают с одними из зарегистрированных в системе
  *
  * Пользователи бывают 3-х видов
  * AnonymousUser - пользователь, который не указал своих учетных данных
  * CardUser - пользователь, который предоствил данные карты
  * LPUser - пользователь, предоставивший логин и пароль
  *
  * Для решения задачи раскомметируйте код в теле объекта Authentication
  * Реализуйте методы authByCard и authByLP, заменив
  * знаки ??? на подходящие выражения.
  *
  * Что-либо еще, кроме знаков ???, заменять нельзя
  */
object Authentication extends App {

  import AuthenticationData._

// val authByCard: PartialFunction[CardCredentials, Boolean] = registeredCards.contains _
 val authByCard: PartialFunction[CardCredentials, Boolean] = {
   case cc: CardCredentials if registeredCards.contains(cc) => true
 }

// val authByLP: PartialFunction[???, ???] = ???
  val authByLP: PartialFunction[LPCredentials, Boolean] = {
    case lpc: LPCredentials if registeredLoginAndPassword.contains(lpc) => true
  }
// val authByLP: PartialFunction[LPCredentials, Boolean] = registeredLoginAndPassword.contains _

  val authenticated: List[Option[User]] = for (user <- testUsers) yield {
        val authBySmth: PartialFunction[User, User] = {
            case u: CardUser if authByCard.lift(u.credentials).contains(true) => u
            case u: LPUser if authByLP.lift(u.credentials).contains(true) => u
          }

        authBySmth.lift(user)

  }

 authenticated.flatten foreach println

}
