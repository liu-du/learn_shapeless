package example

import shapeless.{Coproduct, :+:, CNil, Inl, Inr}
import shapeless.HNil
import shapeless.HList
import shapeless.::
import shapeless.Generic

object TypeClassInstanceDerivationForCoproduct extends App {

  trait AlignedCsvEncoder[T] {
    val width: Int
    def encode(t: T): List[String]
  }

  object AlignedCsvEncoder {
    // summoner
    def apply[T](implicit enc: AlignedCsvEncoder[T]) = enc

    // constructor
    def pure[T](w: Int)(func: T => List[String]) = new AlignedCsvEncoder[T] {
      val width: Int = w
      def encode(t: T): List[String] = func(t)
    }
  }

  // implicit resolution rule for coproducts
  implicit val cNilEncoder: AlignedCsvEncoder[CNil] = AlignedCsvEncoder.pure(0)(_ => ???)
  implicit def coproductEncoder[H, T <: Coproduct](
      implicit
      lEnc: AlignedCsvEncoder[H],
      rEnc: AlignedCsvEncoder[T]
  ): AlignedCsvEncoder[H :+: T] =
    AlignedCsvEncoder.pure(lEnc.width + rEnc.width) {
      case Inl(left)  => lEnc.encode(left) ++ List.fill(rEnc.width)("null")
      case Inr(right) => List.fill(lEnc.width)("null") ++ rEnc.encode(right)
    }

  // implicit resolution rule for products
  implicit val hNilEncoder: AlignedCsvEncoder[HNil] = AlignedCsvEncoder.pure(0)(_ => Nil)
  implicit def hListEncoder[H, T <: HList](
      implicit hEnc: AlignedCsvEncoder[H],
      tEnc: AlignedCsvEncoder[T]
  ): AlignedCsvEncoder[H :: T] = {
    AlignedCsvEncoder.pure(hEnc.width + tEnc.width)(
      x => hEnc.encode(x.head) ++ tEnc.encode(x.tail)
    )
  }

  implicit def optionEncoder[T](implicit enc: AlignedCsvEncoder[T]): AlignedCsvEncoder[Option[T]] =
    AlignedCsvEncoder.pure(enc.width) {
      case None    => List.fill(enc.width)("null")
      case Some(t) => enc.encode(t)
    }

  // (Int :+: Null :+: CNil) represent a nullable integer field
  type Record = (Int :+: String :+: CNil) :: Boolean :: HNil

  val a: List[Option[Record]] = List(
    Some(Inl(1) :: true :: HNil),
    Some(Inr(Inl("two")) :: false :: HNil),
    None
  )

  // base cases, encoders for primitive types: String, int and boolean
  implicit val stringEncoder: AlignedCsvEncoder[String] = AlignedCsvEncoder.pure(1)(str => List(str))
  implicit val intEncoder: AlignedCsvEncoder[Int] = AlignedCsvEncoder.pure(1)(int => List(int.toString))
  implicit val booleanEncoder: AlignedCsvEncoder[Boolean] =
    AlignedCsvEncoder.pure(1)(boolean => List(if (boolean) "yes" else "no"))
  implicit val doubleEncoder: AlignedCsvEncoder[Double] = AlignedCsvEncoder.pure(1)(double => List(double.toString))

  def writeAlignedCsv[A: AlignedCsvEncoder](values: List[A]): String =
    values.map(value => AlignedCsvEncoder[A].encode(value).mkString(", ")).mkString("\n")

  println(writeAlignedCsv(a))

  // Shape
  sealed trait Shape
  final case class Rectangle(width: Double, height: Double) extends Shape
  final case class Circle(radius: Double) extends Shape

  val b: List[Option[Shape]] = List(
    Some(Rectangle(3d, 4d)),
    None,
    Some(Circle(5d))
  )

  implicit def genericEncoder[T, R](implicit gen: Generic[T] { type Repr = R }, csvEncoder: AlignedCsvEncoder[R])
      : AlignedCsvEncoder[T] =
    AlignedCsvEncoder.pure(csvEncoder.width)(x => csvEncoder.encode(gen.to(x)))

  println(writeAlignedCsv(b))
}
