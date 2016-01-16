/*
 * Copyright 2016 David Crosson
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package bullyboy

trait Sha1 {
  def makehash(pass: Array[Byte]): Array[Byte]
}

trait Sha1Native extends Sha1 {
  import java.security.MessageDigest
  private val md = MessageDigest.getInstance("SHA-1")
  def makehash(pass: Pass): Hash = md.digest(pass)
}

trait Sha1Apache extends Sha1 {
  import org.apache.commons.codec.digest.DigestUtils
  def makehash(pass: Pass): Hash = DigestUtils.sha1(pass)
}

trait Sha1SaphirLib extends Sha1 {
  import java.security.MessageDigest
  import fr.cryptohash.JCAProvider
  private val md = MessageDigest.getInstance("SHA-1", new JCAProvider)
  def makehash(pass: Pass): Hash = md.digest(pass)
}

trait Utils {
  def toHex(bytes: Hash) = bytes.map { b => f"$b%02x" }.mkString
  def now() = System.currentTimeMillis
}

case class Alphabet(chars: Array[Byte]) {
  val size = chars.size
  def head = chars.head
  def apply(index: Int) = chars(index)
  def combs(passwordSize: Int) = BigInt(size).pow(passwordSize)
  def info(passwordSize: Int) = {
    println(s"Alphabet contains $size item which means ${combs(passwordSize)} combinations")
  }
}

object Alphabet {
  def apply(chars: Iterable[Char]): Alphabet = {
    Alphabet(chars.toArray.map(_.toByte))
  }
}

trait Progress {
  protected def progressMade(alphabet: Alphabet, passwordSize: Int) {}
}

trait StdoutProgress extends Progress with Utils {
  val checkStep = 1000000L
  private var count = 0L
  private var prevtime = 0L
  private var prevcount = 0L

  override protected def progressMade(alphabet: Alphabet, passwordSize: Int) {
    synchronized {
      count += 1
      if (count % checkStep == 0L) {
        if (prevtime == 0) {
          prevtime = now
          prevcount = count
        } else {
          val curtime = now
          if (curtime - prevtime > 10000) { // at least every 10s
            val duration = curtime - prevtime
            val justdonecount = count - prevcount
            prevtime = curtime
            prevcount = count
            val remaintime = (alphabet.combs(passwordSize) - count) * duration / justdonecount / 1000 / 3600
            val rate = justdonecount * 1000L / duration
            println(s"$count total tested, +$justdonecount added in $duration ms - $rate/s - $remaintime hours remaining")
          }
        }
      }
    }
  }
}

trait PasswordGenerator {
  val alphabet: Alphabet
  def passwordGenerator(passlen: Int): Iterator[Pass] = {
    var tmp = Array.fill(passlen)(0)
    def inc(): Boolean = {
      def incworker(pos: Int): Boolean = {
        if (pos == passlen) false else {
          if (tmp(pos) < alphabet.size - 1) {
            tmp(pos) = tmp(pos) + 1
            true
          } else {
            tmp(pos) = 0
            incworker(pos + 1)
          }
        }
      }
      incworker(0)
    }
    new Iterator[Pass] {
      private var status: Boolean = true
      def hasNext: Boolean = status
      def next(): Pass = {
        val newone = tmp.map(i => alphabet(i))
        status = inc()
        newone
      }
    }
  }

}

trait Brutalizer extends Progress {
  val codec = "US-ASCII"
  val alphabet: Alphabet
  val passwordSize:Int
  def makehash(password: Pass): Hash

  def testhash(hash2test: Pass, refhash: Hash) = {
    progressMade(alphabet, passwordSize)
    if (hash2test.size != refhash.size) false
    else {
      var i = hash2test.size - 1
      while (i >= 0 && (hash2test(i) == refhash(i))) i -= 1
      i < 0
    }
  }

  def testpassword(password2test: Pass, refhash: Hash) = {
    val hash2test = makehash(password2test)
    testhash(hash2test,refhash)
  }
}

abstract class ClassicBrutalizer(val alphabet: Alphabet, val passwordSize: Int) extends Brutalizer {
  def brutalize(inhash: Hash): Option[String] = {
    val curpass = Array.fill(passwordSize)(alphabet.head)
    def worker(pos: Int): Option[String] = {
      if (pos == passwordSize) {
        if (testpassword(curpass, inhash)) {
          val found = new String(curpass, codec)
          println(s"Found : $found")
          Some(found)
        } else None
      } else {
        var result: Option[String] = None
        for { c <- alphabet.chars if !result.isDefined } {
          curpass(pos) = c
          result = worker(pos + 1)
        }
        result
      }
    }
    worker(0)
  }
}

abstract class ParallelBrutalizer(val alphabet: Alphabet, val passwordSize: Int) extends Brutalizer with PasswordGenerator {

  def brutalize(inhash: Hash): Option[String] = {
    passwordGenerator(passwordSize)
      .grouped(1000)
      .map(_.par.find(password => testpassword(password, inhash)).map(b => new String(b)))
      .toStream
      .filter(_.isDefined)
      .head
  }
}

abstract class StreamedBrutalizer(val alphabet: Alphabet, val passwordSize: Int) extends Brutalizer with PasswordGenerator {
  import scala.concurrent.ExecutionContext.Implicits.global
  import akka.actor.{ ActorSystem, Actor, Props }
  import akka.stream.{ActorMaterializer}
  import akka.stream.scaladsl._
  import akka.stream._
  import scala.concurrent.{Future,Await}
  import scala.concurrent.duration._
  
  implicit val system = ActorSystem("reactive-tweets")
  implicit val materializer = ActorMaterializer()

  def brutalize(inhash: Hash): Option[String] = {
    val passwords:Source[Pass,Unit] = Source.fromIterator(() =>passwordGenerator(passwordSize))
    //val out = Sink.headOption[Option[String]]
    val out = Sink.foreach[Option[String]](println)
    
    val passflow = RunnableGraph.fromGraph(GraphDSL.create() { implicit builder =>
      
      val finder = Flow[Pass].mapAsyncUnordered(256)(x => Future{if (testpassword(x,inhash)) Some(x) else None })
      val cleanup = Flow[Option[Pass]].filter(_.isDefined)
      val tostr = Flow[Option[Pass]].map(_.map(new String(_)))
      import GraphDSL.Implicits._
      
      passwords ~> finder ~> cleanup ~> tostr ~> out 
      
      ClosedShape
    })
    
    passflow.run()
    
    None
  }
}

object Brute extends Utils {

  def main(args: Array[String]) {
    val passlen = 8
    val alphabet = Alphabet(('0' to '9') ++ ('a' to 'z') ++ ('A' to 'Z'))
    alphabet.info(passlen)

    //val brutalizer = new ClassicBrutalizer(alphabet, passlen) with Sha1Native with StdoutProgress
    //val brutalizer = new ParallelBrutalizer(alphabet,passlen) with Sha1Apache with StdoutProgress
    val brutalizer = new StreamedBrutalizer(alphabet,passlen) with Sha1Apache with StdoutProgress

    val inhash = brutalizer.makehash("trucMuc1".getBytes(brutalizer.codec))
    println("bruteforcing sha1=" + toHex(inhash))

    brutalizer.brutalize(inhash)
  }
}
