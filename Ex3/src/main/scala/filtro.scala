/* (Dutos e Filtros) DataSource, que possui um atributo
prox de tipo ActorRef indicando o próximo filtro, é um ator que recebe
uma mensagem Start (case class com um campo String). Em seu
recebimento, prox deve enviar uma mensagem Message (case class de
mesma característica). Os filtros LowerCase, UpperCase, FilterVowels
(atores de mesma característica que DataSource) e Duplicate devem
receber a mensagem Message e modificar seu conteudo de acordo, ou
seja, deixar minúsculo, maiúsculo, eliminar vogais e duplicar respectivamente
e enviar a mensagem modificada a um próximo ator prox. Se
não houver próximo ator (null for encontrado) o processo deve parar
imediatamente em qualquer ator. Monte os filtros no teste */

import com.pipes._
import com.filters._


sealed trait Message
case class Start(msg: String) extends Message
case class Message(msg: String) extends Message


class DataSource(prox: ActorRef){
    def Start(msg: String): String = {
        println("Mensagem original: " msg)
        return ()
    }
}

class LowerCase extends DataSource{
    override def Message(msg: String): String ={
        val m = msg.toLowerCase
        println("LowerCase: " + m)
        return ()
        
    }
}

class UpperCase extends DataSource{
    override def Message(msg: String): String ={
         val m = msg.toUpperCase
        println("UpperCase: " + m)
        return 
    }
}

class FilterVowels extends DataSource{
    override def Message(msg: String): String ={
        val m = msg.replaceAll("[aeiouAEIOU]", "")
        println("FilterVowels: " + m)
        return
    }
}

class Duplicate extends DataSource{
    override def Message(msg: String): String ={
        println("Duplicate: " + msg + msg)
        return
    }
}


class Pipes{
    private var lista: Array[DataSource] = new Array[DataSource](0)
    
    def adicionar(filtro: DataSource): Unit = {
        lista = filtro +: lista
    }
    
    def fluxo(entrada: String): Unit = {
        var out = entrada
        for(filtro <- lista){
            out = filtro.Message(Message)
        }
       // println(out)
    }
}

object Main {
    def main(args: Array[String]): Unit = {
        var p = new Pipes()
        p.adicionar(new LowerCase())
        p.adicionar(new UpperCase())
        p.adicionar(new FilterVowels())
        p.adicionar(new Duplicate())
        
        DataSource ! Start("AlO MunDo")
    }
}
