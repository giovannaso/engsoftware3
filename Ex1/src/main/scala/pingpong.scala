/* (Ping-Pong) Há três mensagens Start (sem campos),
Ping e Pong (com um campo inteiro). Um AtorA, ao qual possui um
atributo actB do tipo ActorRef indicando um ator AtorB ao qual ele
pode se comunicar, pode receber um Start de modo a começar a cadeia
e mandar um Ping(0) para um AtorB. O AtorB responde com um Pong
incrementando o valor do campo. Com o campo inteiro chegando em
2000 o processo para. Implemente esse Ping-Pong usando o Akka. */

import akka.actor._
import akka.routing.RoundRobinRouter
import scala.io._

sealed trait Message

case class Start() extends Message
case class Pong(valor: Int) extends Message
case class Ping(valor: Int) extends Message


class AtorA(valor: Int, actb: ActorRef) extends Actor{
    def receive: Receive = {
        case Start => {
        println("Ping")
        actb ! Ping(1)
        }
        case Pong(valor) => {
        println("Ping")
        if (valor<2000){
        sender ! Ping(valor+1)
        }
          else {
            println("Fim do jogo")
            context.stop(self)
          }
        }
    }
}
class AtorB(valor: Int) extends Actor{
 
     def receive: Receive = {
        case Ping(valor) => {
            println("Pong")
            if(valor<2000){
            sender() ! Pong(valor+1)
            }
            else {
            println("Fim do jogo")
            context.stop(self)
          }
        } 
     }
}

object Main{
    def main(args: Array[String]): Unit = {
        val system = ActorSystem("MasterSystem")
        val actb = system.actorOf(Props(new AtorB(0)),"AtorB")
        val acta = system.actorOf(Props(new AtorA(0,actb)),"AtorA")
        acta ! Start
    }
}

