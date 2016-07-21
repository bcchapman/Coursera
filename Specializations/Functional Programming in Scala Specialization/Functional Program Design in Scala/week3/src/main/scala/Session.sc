import java.util.Random

import bank.BankAccount

val acct = new BankAccount
acct deposit 50
acct withdraw 20
acct withdraw 20

try {
  acct withdraw 15
}
catch {
  case e: Error => println(e)
}

def power (x: Double, exp: Int): Double = {
  var r = 1.0
  var i = exp
  while (i > 0) { r = r * x; i = i-1 }
  r
}

power (2, 4)

def WHILE(condition: => Boolean)(command: => Unit): Unit =
  if(condition) {
    command
    WHILE(condition)(command)
  }
  else ()

def power2 (x: Double, exp: Int): Double = {
  var r = 1.0
  var i = exp
  WHILE (i > 0) { r = r * x; i = i-1 }
  r
}

power2 (2,4)

class REPEAT(command: => Unit) {
  def UNTIL(condition: => Boolean): Unit = {
    command

    if (condition) ()
    else UNTIL (condition)
  }
}

object REPEAT {
  def apply(command: => Unit) = new REPEAT(command)
}

def randomBool = new Random().nextBoolean()
REPEAT {
  println("Hello")
} UNTIL (randomBool)

for(i <- 1 until 3) { println (i + " ")}

for(i <- 1 until 3; j <- "abc") { println (i + " " + j)}

(1 until 3) foreach (i => "abc" foreach ( j=> println(i + " " + j)))

