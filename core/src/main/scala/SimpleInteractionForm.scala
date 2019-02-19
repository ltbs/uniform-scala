package ltbs.uniform

import cats.implicits._

trait SimpleInteractionForm[IN,TELL,ASK,OUT] {
  def render(key: String, tell: TELL, existing: Option[Encoded], data: IN, errors: ErrorTree): OUT
  def render(key: String, tell: TELL, existing: Option[Encoded], data: IN): OUT =
    render(key, tell, existing, data, Tree.empty)
  def receiveInput(data: IN): Encoded
  def decodeInput(data: IN): Either[ErrorTree,ASK] = decode(receiveInput(data))
  def encode(in: ASK): Encoded
  def decode(out: Encoded): Either[ErrorTree,ASK]
  
  def transform[ASKB](f: ASK => Either[ErrorTree,ASKB])(g: ASKB => ASK) = {
    val fa = this
    new SimpleInteractionForm[IN,TELL,ASKB,OUT] {
      def decode(out: Encoded): Either[ErrorTree,ASKB] = fa.decode(out).flatMap(f)
      def receiveInput(data: IN): Encoded = fa.receiveInput(data)
      def encode(in: ASKB): Encoded = fa.encode(g(in))
      def render(key: String, tell: TELL, existing: Option[Encoded], data: IN, errors: ErrorTree): OUT =
        fa.render(key,tell, existing,data, errors)
    }
  }

  def validating(f: ASK => Either[ErrorTree,ASK]): SimpleInteractionForm[IN,TELL,ASK,OUT] =
    transform(f)(identity)
}
