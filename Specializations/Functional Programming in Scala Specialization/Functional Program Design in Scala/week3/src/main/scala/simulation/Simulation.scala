package simulation

trait Simulation {
  type Action = () => Unit

  private type Agenda = List[Event]
  private var agenda: Agenda = List()
  private var curTime = 0

  case class Event(time: Int, action: Action)

  def currentTime: Int = curTime

  def afterDelay(delay: Int)(block: => Unit): Unit = {
    val item = Event(currentTime + delay, () => block)
    agenda = insert(agenda, item)
  }

  private def insert(ag: List[Event], item: Event): List[Event] = ag match {
    case first :: rest if first.time <= item.time =>
      first :: insert(rest, item)
    case _ =>
      item :: ag
  }

  def run(): Unit = {
    afterDelay(0) {
      println(s"*** simulation started, time = $currentTime ***")
    }
    loop()
  }

  private def loop(): Unit = agenda match {
    case first:: rest =>
      agenda = rest
      curTime = first.time
      first.action()
      loop()
    case Nil =>
  }
}
