package example

import shapeless.{HList, ::, HNil, Coproduct, :+:, CNil, Inl, Inr}
import shapeless.Generic

object Hello extends App {
  class MyProduct[T, H] {}

  println("hello")

  case class IceCream(name: String, num: Int, inCone: Boolean)

  val product1 = "Sunday" :: 1 :: false :: HNil
  val product2 = "Wed" :: 2 :: true :: HNil
  val products = product1.zip(product2)

  val products2 = ("Sunday" :: "Wed" :: HNil) :: (1 :: 2 :: HNil) :: (false :: true :: HNil) :: HNil

  println(product1.toString)
  println(products.toString)
  println(products2.toString)

  val iceCreamGen = Generic[IceCream]

  val iceCreamCase = IceCream("a", 2, true)

  println(iceCreamGen.from(product1))
  println(iceCreamGen.to(iceCreamGen.from(product1)))

  println("-------- Coproduct --------")

  case class Red()
  case class Amber()
  case class Green()
  type Light = Red :+: Amber :+: Green :+: CNil

  val red: Light = Inl(Red())
  val amber: Light = Inr(Inl(Amber()))
  val green: Light = Inr(Inr(Inl(Green())))

  println(red)
  println(amber)
  println(green)

  sealed trait Shape
  final case class Rectangle(width: Double, height: Double) extends Shape
  final case class Circle(radius: Double) extends Shape

  val gen = Generic[Shape]

  val rectGen = gen.to(Rectangle(1, 2))
}
