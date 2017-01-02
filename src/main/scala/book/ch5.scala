package example.book.ch5
import java.time.ZonedDateTime
import scala.util.{Try, Success, Failure}

import cats._
import cats.instances.all._
import cats.syntax.all._
import cats.data.{Validated, ValidatedNel, Kleisli, NonEmptyList}

case class Account(no: String, name: String, balance: Balance, openingDate: ZonedDateTime, closingDate: Option[ZonedDateTime] = None)
case class Balance(amount: Amount)
case class Amount(amount: BigDecimal)

trait Customer

trait Repository[A, IdType] {
  def query(id: IdType): Validated[Exception, Option[A]]
  def store(a: A): Validated[Exception, A]
}

// https://github.com/debasishg/frdomain/blob/master/src/main/scala/frdomain/ch3/repository/AccountRepository.scala
trait AccountRepository extends Repository[Account, String] {
  def query(id: String): Validated[Exception, Option[Account]]
  def store(a: Account): Validated[Exception, Account]
  def balance(no: String): Validated[Exception, Balance] = query(no) match {
    case Validated.Valid(Some(a)) => Validated.Valid(a.balance)
    case Validated.Valid(None) => Validated.Invalid(new Exception(s"No account exists with no $no"))
    case Validated.Invalid(ex) => Validated.Invalid(ex)
  }
  def query(openedOn: ZonedDateTime): Validated[Exception, Seq[Account]]
}

trait ValidType {
  type Valid[A] = ValidatedNel[String, A]
}

trait AccountService[Account, Amount, Balance] extends ValidType {
  type AccountOperation[A] = Kleisli[Valid, AccountRepository, A]

  def open(no: String, closeDate: Option[ZonedDateTime]): AccountOperation[Account]
  def close(no: String, closeDate: Option[ZonedDateTime]): AccountOperation[Account]
  def debit(no: String, amount: Amount): AccountOperation[Account]
  def credit(no: String, amount: Amount): AccountOperation[Account]
  def balance(no: String): AccountOperation[Balance]

  def transfer(fromNo: String, toNo: String, amount: Amount): AccountOperation[(Account, Account)] =
    (debit(fromNo, amount) |@| credit(fromNo, amount)).map(Tuple2.apply)
}

class AccountServiceInterpreter extends AccountService[Account, Amount, Balance] {
  def open(no: String, closeDate: Option[ZonedDateTime]): AccountOperation[Account] = Kleisli { repository =>
    repository.query(no) match {
      case Validated.Valid(Some(account)) => NonEmptyList.of(s"Already existing account with no $no").invalid[Account]
      case Validated.Valid(None) => ??? // run open new account
      case Validated.Invalid(e) => ??? // convert exception to NonEmptyList[String]
    }
  }

  def close(no: String, closeDate: Option[ZonedDateTime]): AccountOperation[Account] = Kleisli { repository =>
    repository.query(no) match {
      case Validated.Valid(None) => NonEmptyList.of(s"Account $no does not exist.").invalid[Account]
      case Validated.Valid(Some(account)) => ??? // run close account
      case Validated.Invalid(e) => ??? // convert exception to NonEmptyList[String]
    }
  }

  def debit(no: String, amount: Amount): AccountOperation[Account] = ???
  def credit(no: String, amount: Amount): AccountOperation[Account] = ???

  def balance(no: String): AccountOperation[Balance] = Kleisli { repository =>
    repository.balance(no) match {
      case v@ Validated.Valid(b) => v
      case i@ Validated.Invalid(e) => ??? // convert to NonEmptyList[String]
    }
  }
}

object AccountService extends AccountServiceInterpreter

trait InterestCalculation[Account, Amount] extends ValidType {
  def computeInterest: Kleisli[Valid, Account, Amount]
}

trait TaxCalculation[Amount] extends ValidType {
  def computeTax: Kleisli[Valid, Amount, Amount]
}

trait InterestPostingService[Account, Amount]
    extends InterestCalculation[Account, Amount]
    with TaxCalculation[Amount]

class InterestPostingServiceInterpreter extends InterestPostingService[Account, Amount] {
  def computeInterest = ???
  def computeTax = ???
}

object InterestPostingService extends InterestPostingServiceInterpreter

object ExampleClientApplication extends ValidType {
  import AccountService._
  import InterestPostingService._

  def postTransactions(a: Account, creditAmount: Amount, debitAmount: Amount): Kleisli[Valid, AccountRepository, Amount] =
    (credit(a.no, creditAmount) |@| debit(a.no, debitAmount)).map { case (_, a) => a.balance.amount }
}

trait Show[T] {
  def shows(t: T): Try[String]
}

trait ShowProtocol {
  implicit val showAccount: Show[Account]
  implicit val showCustomer: Show[Customer]
}

trait DomainShowProtocol extends ShowProtocol {
  implicit val showAccount: Show[Account] = new Show[Account] {
    def shows(a: Account) = Success(a.toString)
  }
  implicit val showCustomer: Show[Customer] = new Show[Customer] {
    def shows(a: Customer) = Success(a.toString)
  }
}

object DomainShowProtocol extends DomainShowProtocol

object Reporting {
  def report[T: Show](as: Seq[T]) = as.map(implicitly[Show[T]].shows(_))
}

// some free monads
// (a free monad is a way of deriving a monad--getting a "free" monad--from another type which is already an (endo)functor)

import cats.free.Free

// define basic algebra for account repo application
sealed trait AccountRepoF[+A]
case class Query(no: String) extends AccountRepoF[Account]
case class Store(account: Account) extends AccountRepoF[Unit]
case class Delete(no: String) extends AccountRepoF[Unit]

trait AccountRepoType {
  type AccountRepo[A] = Free[AccountRepoF, A] // get the monad
}

trait AccountRepository2 extends AccountRepoType {

  // put the algebra's types into the monad (the implementation of the algebra is in the interpreter)
  def store(account: Account): AccountRepo[Unit] = Free.liftF(Store(account))
  def query(no: String): AccountRepo[Account] = Free.liftF(Query(no))
  def delete(no: String): AccountRepo[Unit] = Free.liftF(Delete(no))

  // and then use the (free) monad to compose more exciting operations

  def update(no: String, f: Account => Account): AccountRepo[Unit] = for {
    a <- query(no)
    _ <- store(f(a))
  } yield ()

  def updateBalance(no: String, amount: Amount, f: (Account, Amount) => Account): AccountRepo[Unit] = for {
    a <- query(no)
    _ <- store(f(a, amount))
  } yield ()

  def open(no: String, name: String, balance: Balance, openingDate: ZonedDateTime): AccountRepo[Account]= for {
    _ <- store(Account(no, name, balance, openingDate))
    a <- query(no)
  } yield a

  def close(no: String): AccountRepo[Account] = for {
    _ <- update(no, _.copy(closingDate = Some(ZonedDateTime.now)))
    a <- query(no)
  } yield a
}

trait AccountRepositoryInterpreter[F[_]] extends AccountRepoType {
  def apply[A](action: AccountRepo[A]): F[A]
}

object AccountRepositoryExampleInterpreter extends AccountRepositoryInterpreter[Id] { // book uses scalaz Task
  val step: AccountRepoF ~> Id = new (AccountRepoF ~> Id) {
    override def apply[A](fa: AccountRepoF[A]): Id[A] = fa match {
      case Query(no) => ??? // get from db
      case Store(account) => ??? // store in db
      case Delete(no) => ??? // del from db
    }
  }

  def apply[A](action: AccountRepo[A]): Id[A] = action.foldMap(step)
}

import cats.data.StateT

object AccountRepositoryState {
  type AccountMap = Map[String, Account]

  type Errors = NonEmptyList[String]
  type Valid[A] = Either[Errors, A] // Use Either. Validated isn't a monad, so AccountState wouldn't be one either.

  type AccountState[A] = StateT[Valid, AccountMap, A]
}

object AccountRepositoryAccountStateInterpreter extends AccountRepositoryInterpreter[AccountRepositoryState.AccountState] {
  import AccountRepositoryState._

  val step: AccountRepoF ~> AccountState = new (AccountRepoF ~> AccountState) {
    override def apply[A](fa: AccountRepoF[A]): AccountState[A] = fa match {
      case Query(no) => StateT { map =>
        map.get(no) match {
          case Some(a) => Right((map, a))
          case None => Left(NonEmptyList.of(s"Account $no does not exist."))
        }
      }

      case Store(account) => StateT { map =>
        map.get(account.no) match {
          case Some(a) => Left(NonEmptyList.of(s"Account $account.no already exists."))
          case None => Right((map + (account.no -> account), ()))
        }
      }

      case Delete(no) => StateT { map =>
        map.get(no) match {
          case Some(a) => Right((map - no, ()))
          case None => Left(NonEmptyList.of(s"Account $no does not exist."))
        }
      }
    }
  }

  def apply[A](action: AccountRepo[A]): AccountState[A] = action.foldMap(step)
}