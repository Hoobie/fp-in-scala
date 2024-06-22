package chapter8

trait Prop { self =>
  def check: Boolean
  def &&(that: Prop): Prop =
    new Prop {
      def check = this.check && that.check
    }
}
