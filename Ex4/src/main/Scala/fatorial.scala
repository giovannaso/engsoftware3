/* (Fatorial) Implemente o cálculo de um fatorial passado
por uma mensagem usando o esquema de mestre/escravo. Lembre-se
que 0! = 1! = 1 */

import akka.actor._
import akka.routing.RoundRobinRouter
import scala.concurrent.duration._
import scala.io._

sealed trait Message

case class Start(numero: Int) extends Message
case class Fatorial(valor: Int) extends Message


class Mestre(worker: ActorRef) extends Actor{
    def receive = {
    case Start(num) =>
    actb ! Ping(0)
    
    sender ! Ping(valor+1)
    }
}

class Worker extends Actor{
 
     def receive = {
        case Fatorial(valor) =>
        if(valor <2000){
            println("Pong")
            sender ! Pong(valor+1)
        }
        else
            context.system.shutdown()
    }
}

object Main{
    def main(args: Array[String]): Unit = {
        val system = ActorSystem("MasterSystem")
        val mestre = system.actorOf(Props[Mestre],"Mestre")
        val worker = system.actorOf(Props[Worker],"Worker")
        mestre ! "Start(5)"
    }
}


