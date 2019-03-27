package ltbs.uniform

import cats.implicits._

trait SimpleInteractionForm[IN,TELL,ASK,OUT] {
  def render(key: String, tell: TELL, existing: Option[Encoded], data: IN, messages: UniformMessages[OUT], errors: ErrorTree): OUT
  def render(key: String, tell: TELL, existing: Option[Encoded], data: IN, messages: UniformMessages[OUT]): OUT =
    render(key, tell, existing, data, messages, Tree.empty)
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
      def render(key: String, tell: TELL, existing: Option[Encoded], data: IN, messages: UniformMessages[OUT], errors: ErrorTree): OUT =
        fa.render(key,tell, existing,data, messages, errors)
    }
  }

  def transformOut[OUT2](f: OUT => OUT2)(g: OUT2 => OUT): SimpleInteractionForm[IN,TELL,ASK,OUT2] = {
    val original = this
    new SimpleInteractionForm[IN,TELL,ASK,OUT2] {
      def decode(out: Encoded): Either[ErrorTree,ASK] = original.decode(out)
      def receiveInput(data: IN): Encoded = original.receiveInput(data)
      def encode(in: ASK): Encoded = original.encode(in)
      def render(key: String, tell: TELL, existing: Option[Encoded], data: IN, messages: UniformMessages[OUT2], errors: ErrorTree): OUT2 =
        f(original.render(key, tell, existing, data, messages.map(g), errors))
    }
  }

  def transformIn[IN2](g: IN2 => IN): SimpleInteractionForm[IN2,TELL,ASK,OUT] = {
    val original = this
    new SimpleInteractionForm[IN2,TELL,ASK,OUT] {
      def decode(out: Encoded): Either[ErrorTree,ASK] = original.decode(out)
      def receiveInput(data: IN2): Encoded = original.receiveInput(g(data))
      def encode(in: ASK): Encoded = original.encode(in)
      def render(key: String, tell: TELL, existing: Option[Encoded], data: IN2, messages: UniformMessages[OUT], errors: ErrorTree): OUT =
        original.render(key, tell, existing, g(data), messages, errors)
    }
  }

  def validating(f: ASK => Either[ErrorTree,ASK]): SimpleInteractionForm[IN,TELL,ASK,OUT] =
    transform(f)(identity)
}
