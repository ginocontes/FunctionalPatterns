package com.ginocontes

object Basics {

  def factorial(n: Int) : Int = {
    def go(n: Int, acc: Int): Int = {
      if ( n <= 0) acc
      else go(n-1, acc*n)
    }
    go(n, 1)
  }

  //exercise2.1 recursive fibonacci with local tail optimisation

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, previous_two: Int, previous: Int): Int = {
      if(n == 0) 0
      else if(n ==1) previous_two + previous
      else go(n-1,previous,  previous_two  + previous)
    }
    go(n, 1, 0)
  }
    //exercise 2.2 implement is Sorted
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def go(n: Int): Boolean = {
      if(as.size == 1 || n==as.size) true
      else if (!ordered(as(n-1), as(n))) false
      else go(n+1)
    }
    go(1)
  }

  // exercise 2.3
  def curry[A,B,C](f: (A,B) => C): A=>(B=>C) = a => b => f(a, b)

  // exercise 2.4
  def uncurry[A,B,C](f: A => B => C): (A,B) => C = (a,b) => f(a)(b)

  // exercise 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C = a  => f(g(a))


  def main(args: Array[String]) = {
    println("Hello world")
    println(fib(10))
    val x = Array(1)
    println(x.size)
    println(isSorted(Array(1,3,2, 4,5), (x:Int, y:Int) => x < y))
  }
}
