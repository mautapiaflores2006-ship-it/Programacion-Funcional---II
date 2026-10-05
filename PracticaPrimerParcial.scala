import scala.annotation.tailrec
object PracticaPrimerParcial {

    //Ejercicio 1
    def decimalABinario (num:Int):String = {
        if num <= 1 then  s"${num%2}"
        else s"${decimalABinario(num/2)}" + s"${num%2}"
    }

    //Ejercicio 2
    def digitoMayor (num:Int):Int = {
        def mayor (a:Int, b:Int):Int = {
            if a > b then a else b
        }
        if num < 10 then num
        else mayor(digitoMayor(num/10)%10,num%10)
    }

    //Ejercicio 3
    def contarDigitosPares (num:Int):Int = {
        if num <10 && num%2 == 1 then 0
        else if num<10 && num%2 == 0 then 1
        else (if (num%10)%2==0 then 1 else 0) + contarDigitosPares(num/10)
    }

    //Ejercicio 4
    def contarDigito(num:Int, dig:Int):Int = {
        if num < 10 && num == dig then 1
        else if num <10 && num != dig then 0
        else (if num%10 == dig then 1 else 0) + contarDigito(num/10, dig)
    }

    //Ejercicio 5
    def extraerPares(num:Int):String = {
        if num<10 && num%2 == 1 then ""
        else if num<10 && num%2 == 0 then s"$num"
        else s"${extraerPares(num/10)}" + (if (num%10)% 2 == 0 then s"${num%10}" else "" )
    }

    //Ejercicio 6
    @tailrec
    def sumarDigitosPares (num:Int, acum:Int = 0): Int = {
        if num < 10 && num%2 == 0 then acum + num
        else if num < 10 && num%2 == 1 then acum
        else sumarDigitosPares(num/10, acum + (if ((num%10)%2==0) then num%10 else 0))
    }

    //Ejercicio 7
    @tailrec
    def contarMayusculas(p:String, acum:Int = 0):Int = {
        if p.isEmpty() then acum
        else contarMayusculas(p.tail, acum + (if (p(0).isUpper) then 1 else 0))
    }

    //Ejercicio 8
    @tailrec
    def contarCambiosParidad(num:Int, acum:Int = 0):Int = {
        if num < 10 then acum
        else contarCambiosParidad(num/10, acum + (if(((num/10)%10)%2 != (num%10)%2) then 1 else 0))
    }

    //Ejercicio 9
def segundoMayorDigito(num: Int, acum: Int = -1): Int = {
    @tailrec
  def max(n: Int, p: Int, s: Int): Int = {
    if n == 0 then s
    else {
      val d = n % 10
      if d > p then max(n / 10, d, p)
      else if d < p && d > s then max(n / 10, p, d)
      else max(n / 10, p, s)
    }
  }

  if num < 10 then acum
  else max(num, -1, -1)
}

    //Ejercicio 10
    def rachaMaxima(p: String, acum: Int = 0): Int = {
        @tailrec
        def aux(a: String, dact: Int = 1, dmax: Int = 1): Int = {
            if a.length <= 1 then 
            if dact > dmax then dact else dmax
            else {
            val v1 = a.head
            val v2 = a(1) 
            
            if v1 == v2 then {
                aux(a.tail, dact + 1, dmax)
            } else {
                val nuevoMax = if dact > dmax then dact else dmax
                aux(a.tail, 1, nuevoMax)
            }
            }
        }
        if p.isEmpty then acum
        else aux(p)
}

    def main (args: Array[String]):Unit = {
        //println(decimalABinario(19))
        //println(dmayor(58328400))
        //println(par(583246))
       //println(aparicion(525235,5))
       //println(extraerPares(583246))
       //println(sumarDigitosPares(583246))
       //println(contarMayusculas("HolaMundoScala"))
       // println(contarCambiosParidad(52841))
       //println(segundoMayorDigito(53829))
       println(rachaMaxima("aaabbccccdaa"))
       println(rachaMaxima("aabbbbbcc"))
       println(rachaMaxima("abcd"))
       println(rachaMaxima("aaaaaa"))
       println(rachaMaxima("aabbbaaaa"))
       println(rachaMaxima(""))
       println(rachaMaxima("aaAAaa"))

    }
}