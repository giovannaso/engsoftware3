/* (Jogo de advinhação - Mestre/Escravo) Quer-se fazer
um jogo, de forma concorrente, de advinhação. Trinta e dois jogadores
devem sortear números de 1 a 200 e a "máquina"possui o número
correto. Indique o(s) vencedore(s). */

import akka.actor._
import akka.routing.RoundRobinRouter
import scala.concurrent.duration._
import scala.io._

sealed trait Message

case class Start() extends Message
case class Sortear(num: Int) extends Message
case class Contador(valor: Int) extends Message


class Mestre(worker: ActorRef) extends Actor{
    def receive = {
    case Start() =>
    num = scala.util.Random(200)
    worker ! Sortear(num)
    case Contador(cont) = {
        if (cont == 0){
            println("Ninguem acertou")
        }
        else
        context.system.shutdown()
    }
}

class Worker extends Actor{
 
     def receive = {
        case Sortear(num) =>
        for(i<-1 to 32)
    {
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

object Main{
    def main(args: Array[String]): Unit = {
        val system = ActorSystem("MasterSystem")
        val mestre = system.actorOf(Props[Mestre],"Mestre")
        val worker = system.actorOf(Props[Worker],"Worker")
        mestre ! "Start"
    }
}



