package example

import shapeless.{HList, ::, HNil, Generic}

trait Fixtures {

  // example class
  case class Employee(name: String, number: Int, manager: Boolean)
  case class IceCream(name: String, numCherries: Int, cone: Boolean)

  val hList = List("Bill" :: 1 :: false :: HNil, "Mark" :: 2 :: true :: HNil)
  val hList2 = List(("Bill", "Gates") :: 1 :: false :: HNil, ("Mark", "Twin") :: 2 :: true :: HNil)

  // mannually define employee encoder by employee it to HList
  val employeeList: List[Employee] = hList.map(Generic[Employee].from)
  val iceCreamList: List[IceCream] = hList.map(Generic[IceCream].from)

  // helper method to write a list of A as csv
  def writeCsv[A: CsvEncoder](values: List[A]): String =
    values.map(value => CsvEncoder[A].encode(value).mkString(", ")).mkString("\n")

  // csv encoder type class
  trait CsvEncoder[A] { def encode(value: A): List[String] }
  object CsvEncoder {
    // Summoner
    def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] = enc

    // Constructor
    def instance[A](func: A => List[String]): CsvEncoder[A] = new CsvEncoder[A] {
      def encode(value: A): List[String] = func(value)
    }
  }

}
