/* (Fatorial) Implemente o cálculo de um fatorial passado
por uma mensagem usando o esquema de mestre/escravo. Lembre-se
que 0! = 1! = 1 */

import akka.actor._
import akka.routing.RoundRobinRouter
import scala.concurrent.duration._
import scala.io._

sealed trait Message

case object Calcular extends Message
case class Conta(inic: Int, num: Int) extends Message
case class Resultado(valor: Int) extends Message


class Worker extends Actor {
        def Conta(num: Int, fat: Int): Int = {
            var fatorial = 0
            fatorial = fat - num
        return fatorial
    }
    override def receive: Receive = {
        case Conta(num,fat) => sender() ! Resultado(Conta(num,fat))
    }
}

class Master(num:Int, listener: ActorRef) extends Actor{
var total: Int = 1
var result: Int = 0

val Worker = context.actorOf(Props[Worker].withRouter(RoundRobinRouter(num)), "Worker")
 
    override def receive: Receive = {
        if(num<=1)
        {
            case Calcular =>{
                listener ! Resultado(num)
            }
        }
        else
        {
            case Calcular =>{
                for(i<-0 until num - 1){
                Worker ! Conta(i,num)    
                }
            }
            case Resultado(num) =>
            {
                total *= num
                result += 1
                
                if (result == num - 1){
                    listener ! Resultado(total)
                }
                
            }
        }
}
}

class listener(num: Int) extends Actor{
     def receive: Receive = {
        case Resultado(num) => {
            println("O numero é: " + num)
     }
}
}

object Main{
    def main(args: Array[String]): Unit = {
        
        println("Insira o numero: ")
        val num = StdIn.readInt()
        
        val system = ActorSystem("MasterSystem")
        
        val listener = system.actorOf(Props(new listener(0)),"listener")
        val masterActor = system.actorOf(Props(new Master(num,listener)),"masterActor")
        
        masterActor ! Calcular
    }
}