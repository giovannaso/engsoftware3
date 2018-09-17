import akka.actor._
import akka.routing.RoundRobinRouter
import scala.concurrent.duration._
import scala.io._

sealed trait Message

case object Calculate extends Message

case class Work(start: Int, number: Int) extends Message
case class Result(valor: Double) extends Message
case class Show(conta: Double, duration: Duration) extends Message

class Worker extends Actor{
    
    def conta(start: Int, number: Int): Double = {
        case Sortear(num) =>
        val r = scala.util.Random(200)
        val cont = 0
        if (r == num)
        {
            cont++
            println("Jogador $cont acertou!")
        }
    }
        sender ! Contador(cont)
    }
    }
    
    override def receive: Receive = {
        case Work(start,fim) => sender ! Result(conta(start,fim))
    }
}

class Master(workers: Int, msgs: Int, listener: ActorRef) extends Actor{
    var conta: Double = _
    var resultados: Int = _
    val start: Long = System.currentTimeMillis // COMECO
 
    val worker = context.actorOf(Props[Worker].withRouter(RoundRobinRouter(workers)), "worker")
      
    override def receive: Receive = {
        case Calculate => {
            for(i<- 0 until msgs)
                worker ! Work(i*msgs,msgs)
        }
        
        case Result(res) => {
            conta += res 
            resultados += 1
            if(resultados == msgs){
                listener ! Show(conta, (System.currentTimeMillis - start).millis)
                context.stop(self)
            }
        }
    }
}

class Listener extends Actor{
    def receive: Receive = {
      case Show(res, duration) ⇒
        println("Resultado: \t\t%s\n\tTempo: \t%s".format(res, duration))
        context.system.shutdown()
    }
}

object Teste{
    def main(args: Array[String]): Unit = {
        val system = ActorSystem("MainSystem")
        println("Digite os trabalhadores ")
        val workers = StdIn.readInt()
        println("Digite os pedacos ")
        val p = StdIn.readInt()
        val listener = system.actorOf(Props[Listener], "listener")
        val masterActor = system.actorOf(Props(new Master(workers, p, listener)),"masterActor")
        
        masterActor ! Calculate
    }
}