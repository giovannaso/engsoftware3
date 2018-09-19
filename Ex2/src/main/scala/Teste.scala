/* (Jogo de advinhação - Mestre/Escravo) Quer-se fazer
um jogo, de forma concorrente, de advinhação. Trinta e dois jogadores
devem sortear números de 1 a 200 e a "máquina"possui o número
correto. Indique o(s) vencedore(s). */

import akka.actor._
import akka.routing.RoundRobinRouter
import scala.io._

sealed trait Message

case class Guess(n: Int) extends Message
case class Resultado(valor: Int) extends Message


class Worker extends Actor{
 
     def receive: Receive = {
        case Guess(num) => {
        for(i<- 0 until 32){
        val r = scala.util.Random
        val n = r.nextInt(10)
        if (n == num){
        println("Jogador" + (i+1) + "acertou!")
      //  c = 1
        }
        }
        sender ! Resultado(0)
    }
}
}

class Master(num:Int, listener: ActorRef) extends Actor{
 
   // var c = 0
    val Worker = context.actorOf(Props[Worker].withRouter(RoundRobinRouter(32)), "Worker")
      
    override def receive: Receive = {
        case Guess => {
            val r = scala.util.Random
            val n = r.nextInt(10)
            println("O numero é:  "+  n)
               Worker ! Guess(n)
        }
        case Resultado(num) => {
                listener ! Resultado(0)
                context.stop(self)
            }
        }
}

class listener(num: Int) extends Actor{
 
     def receive: Receive = {
        case Resultado(num) => {
            if(num==0){
                println("Ninguem Acertou!")
            }
            else{
                println(num + " acertaram")
            }
        }
     }
}

object Main{
    def main(args: Array[String]): Unit = {
        val system = ActorSystem("MasterSystem")
        val listener = system.actorOf(Props(new listener(0)),"listener")
        val masterActor = system.actorOf(Props(new Master(0,listener)),"masterActor")
        masterActor ! Guess
    }
}