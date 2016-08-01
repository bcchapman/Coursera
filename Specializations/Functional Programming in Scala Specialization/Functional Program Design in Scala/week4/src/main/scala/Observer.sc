trait Subscriber {
  def handler(pub: Publisher)
}

trait Publisher {
  private var subscribers: Set[Subscriber] = Set()

  def subscribe(subscriber: Subscriber): Unit =
    subscribers += subscriber

  def unsubscribe(subscriber: Subscriber): Unit =
    subscribers -= subscriber

  def publish(): Unit =
    subscribers.foreach(_.handler(this))

}

class BankAccount extends Publisher {
  private var balance = 0

  def deposit(amount: Int): Unit = {
    if(amount > 0) {
      balance = balance + amount
      publish()
    }
  }

  def withdraw(amount: Int): Int =
    if(0 < amount && amount <= balance) {
      balance = balance - amount
      publish()
      balance
    } else throw new Error("insufficient funds")

  def currentBalance = balance
}

class Consolidator(observed: List[BankAccount]) extends Subscriber {
  observed.foreach(_.subscribe(this))

  private var total: Int = _
  compute()

  private def compute() = {
    total = observed.map(_.currentBalance).sum
  }

  def handler(pub: Publisher) = compute()

  def totalBalance = total
}

val b1 = new BankAccount()
val b2 = new BankAccount()

val c = new Consolidator(List(b1,b2))
c.totalBalance
b1 deposit(150)
c.totalBalance
b2 deposit(75)
c.totalBalance


