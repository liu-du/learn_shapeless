package example

import shapeless.{HList, ::, HNil, Generic}

object TypeClassInstanceDerivation extends App with Fixtures {

  // base cases, encoders for primitive types: String, int and boolean
  implicit val stringEncoder: CsvEncoder[String] = CsvEncoder.instance(str => List(str))
  implicit val intEncoder: CsvEncoder[Int] = CsvEncoder.instance(int => List(int.toString))
  implicit val booleanEncoder: CsvEncoder[Boolean] = CsvEncoder.instance(boolean => List(if (boolean) "yes" else "no"))

  // implicit val for HNil
  implicit val hNilEncoder: CsvEncoder[HNil] = CsvEncoder.instance(_ => Nil)
  // implicit resolution rule for HList
  implicit def hListEncoder[T: CsvEncoder, H <: HList: CsvEncoder]: CsvEncoder[T :: H] = CsvEncoder.instance {
    case a :: as => CsvEncoder[T].encode(a) ++ CsvEncoder[H].encode(as)
  }

  // implicit resolution rule for Tuple2
  implicit def pairEncoder[A: CsvEncoder, B: CsvEncoder]: CsvEncoder[(A, B)] = CsvEncoder.instance {
    case (a, b) => CsvEncoder[A].encode(a).zip(CsvEncoder[B].encode(b)).map { case (a, b) => s"$a:$b" }
  }

  // automatic derivation for arbitrary HList and pairs
  println(writeCsv(hList))
  println(writeCsv(hList2))
  println(writeCsv(hList.zip(hList)))

  // mannually define employee encoder by employee it to HList
  implicit val employeeEncoder: CsvEncoder[Employee] = {
    val gen = Generic[Employee]
    CsvEncoder.instance(employee => CsvEncoder[gen.Repr].encode(gen.to(employee)))
  }
  println(writeCsv(employeeList))
  println(writeCsv(employeeList.zip(employeeList)))

  // final cut, use type refinement to define automatic type class derivation for any case class!
  implicit def genericEncoder[A, R](implicit gen: Generic[A] { type Repr = R }, enc: CsvEncoder[R]): CsvEncoder[A] =
    CsvEncoder.instance(a => CsvEncoder[R].encode(gen.to(a)))

  println(writeCsv(iceCreamList))

  println(
    // compiler expands writeCsv(iceCreamList) to the following
    writeCsv(iceCreamList)(
      genericEncoder(
        Generic[IceCream],
        hListEncoder(stringEncoder, hListEncoder(intEncoder, hListEncoder(booleanEncoder, hNilEncoder)))
      )
    )
  )

}
