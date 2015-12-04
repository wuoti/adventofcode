import java.security.MessageDigest

val input = "bgvyzdsv"
val digest = MessageDigest.getInstance("MD5")
                     
def md5(s: String) = digest.digest(s.getBytes).map("%02x".format(_)).mkString

def foo(prefix: String) = Stream.from(1)
  .zipWithIndex
  .map{ case (v, i) => (v, i + 1) }
  .map{ case (v, i) => (md5(input + v), i) }
  .filter(_._1.startsWith(prefix))
  .head

val (_, secret1) = foo("00000")
val (_, secret2) = foo("000000")

println(s"The first secret key is $secret1")
println(s"The second secret key is $secret2")