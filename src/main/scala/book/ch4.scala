package example.book.ch4
import java.time.Instant
import cats._
import cats.instances.all._
import cats.syntax.all._

package MyExamples {
  trait Monoid[T] {
    def empty: T // the book uses `zero`
    def combine(x: T, y: T): T // the book uses `op`
  }

  trait Foldable[F[_]] {
    def foldl[A,B](as: F[A], z: B, f: (B,A) => B): B
    def foldMap[A,B](as: F[A])(f: A => B)(implicit m: Monoid[B]): B =
      foldl(as, m.empty, (b: B, a: A) => m.combine(b, f(a)))
  }

  trait FunctorPattens {
    def mapList[A, B](f: A => B): List[B]
    def mapOption[A, B](f: A => B): Option[B]
    // etc
  }
  trait Functor[F[_]] {
    def map[A, B](a: F[A])(f: A => B): F[B]
  }

  trait Applicative[F[_]] extends Functor[F] {
    def unit[A](a: => A): F[A]
    def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B]

    def apply2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] = ap(fb)(map(fa)(f.curried))
    // apply3, etc

    def lift2[A,B,C](f: (A,B) => C): (F[A], F[B]) => F[C] = apply2(_, _)(f)
    // lift3, etc
  }

  trait Monad[F[_]] extends Applicative[F] {
    def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]
  }

  case class Kleisli[M[_], A, B](run: A => M[B]) {
    def andThen[C](k: Kleisli[M,B,C])(implicit m: Monad[M]) = Kleisli[M,A,C] { a =>
      m.flatMap(run(a))(k.run)
    }

    def compose[C](k: Kleisli[M,C,A])(implicit m: Monad[M]): Kleisli[M,C,B] = k andThen this
  }

  object instances {
    implicit def IntegersMonoid = new Monoid[Int] {
      def empty = 0
      def combine(x: Int, y: Int) = x + y
    }

    implicit def ListFunctor: Functor[List] = new Functor[List] {
      def map[A, B](a: List[A])(f: A => B): List[B] = a.map(f)
    }

    implicit def OptionFunctor: Functor[Option] = new Functor[Option] {
      def map[A, B](a: Option[A])(f: A => B): Option[B] = a.map(f)
    }

    implicit def ListApply: Applicative[List] = new Applicative[List] {
      def map[A, B](a: List[A])(f: A => B): List[B] = a.map(f)
      def unit[A](a: => A): List[A] = List(a)
      def ap[A,B](as: => List[A])(fs: => List[A => B]): List[B] = for {
        a <- as
        f <- fs
      } yield f(a)
    }
  }

  object syntax {
    def |+|[T: Monoid](x: T, y: T): T = implicitly[Monoid[T]].combine(x, y)
  }
}

sealed trait TransactionType
case object DR extends TransactionType
case object CR extends TransactionType

sealed trait Currency
case object USD extends Currency
case object JPY extends Currency
case object AUD extends Currency
case object INR extends Currency

object Money {
  implicit val MoneyAdditionMonoid = new Monoid[Money] {
    val m = implicitly[Monoid[Map[Currency, BigDecimal]]]
    def empty = Money(Monoid[Map[Currency, BigDecimal]].empty)
    def combine(x: Money, y: Money) = Money(m.combine(x.m, y.m))
  }
}

case class Money(m: Map[Currency, BigDecimal]) {
  def toBaseCurrency: BigDecimal = ???
}

case class Transaction(
  txid: String,
  accountNo: String,
  date: Instant,
  amount: Money,
  txnType: TransactionType,
  status: Boolean
)

case class Balance(b: Money)

// the algebra
trait Analytics[Transaction, Balance, Money] {
  def maxDebitOnDay(txns: List[Transaction])(implicit m: Monoid[Money]): Money
  def sumBalances(bs: List[Balance])(implicit m: Monoid[Money]): Money
}

object Analytics extends Analytics[Transaction, Balance, Money] {
  import Money._

  def mapReduce[F[_], A, B](as: F[A])(f: A => B)(implicit fd: Foldable[F], m: Monoid[B]) =
    fd.foldMap(as)(f)

  def maxDebitOnDay(txns: List[Transaction])(implicit m: Monoid[Money]): Money =
    mapReduce(txns.filter(_.txnType == DR))(valueOf)

  def sumBalances(bs: List[Balance])(implicit m: Monoid[Money]): Money =
    mapReduce(bs)(creditBalance)

  private def valueOf(txn: Transaction): Money = txn.amount
  private def creditBalance(b: Balance): Money = b.b
}

import cats.data.Kleisli

trait Trading[Account, Market, Order, ClientOrder, Execution, Trade] {

  // without kleisli
  // def clientOrders: ClientOrder => List[Order]
  // def execute(m: Market, a: Account): Order => List[Execution]
  // def allocate(as: List[Account]): Execution => List[Trade]

  // def tradeGeneration(market: Market, broker: Account, clientAccounts: List[Account]): ClientOrder => List[Trade] =
  //   clientOrder => for {
  //     order <- clientOrders(clientOrder)
  //     execution <- execute(market, broker)(order)
  //     trade <- allocate(clientAccounts)(execution)
  //   } yield trade

  def clientOrders: Kleisli[List, ClientOrder, Order]
  def execute(m: Market, a: Account): Kleisli[List, Order, Execution]
  def allocate(as: List[Account]): Kleisli[List, Execution, Trade]

  def tradeGeneration(market: Market, broker: Account, clientAccounts: List[Account]): Kleisli[List, ClientOrder, Trade] =
    clientOrders andThen execute(market, broker) andThen allocate(clientAccounts)
}

case class LoanApplication[Loans](
  date: Instant,
  name: String,
  purpose: String,
  repayIn: Int,
  actualRepaymentYears: Option[Int] = None,
  startDate: Option[Instant] = None,
  loanNo: Option[String] = None,
  emi: Option[BigDecimal] = none
)

object LoanApplication {
  trait Applied
  trait Approved
  trait Enriched

  type LoanApplied = LoanApplication[Applied]
  type LoanApproved = LoanApplication[Approved]
  type LoanEnriched = LoanApplication[Enriched]

  def applyLoan(name: String, purpose: String, repayIn: Int, date: Instant = Instant.now): LoanApplied =
    LoanApplication[Applied](date, name, purpose, repayIn)

  def approve = Kleisli[Option, LoanApplied, LoanApproved] { loan =>
    loan.copy[Approved](
      loanNo = mkLoanNo().some,
      actualRepaymentYears = 15.some,
      startDate = Instant.now.some
    ).some
  }

  def enrich = Kleisli[Option, LoanApproved, LoanEnriched] { loan =>
    val emi = for {
      y <- loan.actualRepaymentYears
      s <- loan.startDate
    } yield calculateEMI(y, s)
    loan.copy[Enriched](emi = emi).some
  }

  private def mkLoanNo(): String = ???
  private def calculateEMI(tenure: Int, start: Instant): BigDecimal = ???
}
