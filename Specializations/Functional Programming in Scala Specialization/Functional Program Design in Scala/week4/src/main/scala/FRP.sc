import calculator.{Signal, Var}

class BankAccount {
  val balance = Var(0)

  def deposit(amount: Int): Unit = {
    if(amount > 0) {
      val b = balance()
      balance() = b + amount
    }
  }

  def withdraw(amount: Int): Int =
    if(0 < amount && amount <= balance()) {
      val b = balance()
      balance() = b - amount
      balance()
    } else throw new Error("insufficient funds")

  def currentBalance = balance
}

def consolidated(accts: List[BankAccount]): Signal[Int] =
  Signal(accts.map(_.balance()).sum)


val b1 = new BankAccount()
val b2 = new BankAccount()

val c = consolidated(List(b1,b2))
c()
b1 deposit 20
c()
b2 deposit 30
c()

val xchange = Signal(246.00)
val inDollar = Signal(c() * xchange())
inDollar()
b1 withdraw 10
inDollar()

