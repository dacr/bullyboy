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

import scala.concurrent.ExecutionContextExecutor

trait Descriptor {
  val name:String
  val desc:String
}

trait Sha1 extends Descriptor {
  def makehash(pass: Pass): Pass
  def hashit(pass: Pass): Tuple2[Pass, Hash] = pass -> makehash(pass)
}

class Sha1Nop extends Sha1 {
  val name = "Sha1Nop"
  val desc = "Do nothing, pass it returns as the hash"
  def makehash(pass: Pass): Hash = pass
}

class Sha1Native() extends Sha1 {
  val name = "Sha1Native"
  val desc = "Default JVM Sha1 implementation - not thread safe"
  // default digester is not thread safe
  import java.security.MessageDigest
  private val md = MessageDigest.getInstance("SHA-1")
  def makehash(pass: Pass): Hash = md.digest(pass)
}

class Sha1NativeThreadLocal() extends Sha1 {
  val name = "Sha1NativeThreadLocal"
  val desc = "Default JVM Sha1 implementation - thread safety through ThreadLocal variables"
  // default digester is not thread safe
  import java.security.MessageDigest
  private val threadLocal = new ThreadLocal[MessageDigest]()
  def makehash(pass: Pass): Hash = {
    val md = threadLocal.get() match {
      case null =>
        val md = MessageDigest.getInstance("SHA-1")
        threadLocal.set(md)
        md
      case md => md
    }
    md.digest(pass)
  }
}

class Sha1NativePooled() extends Sha1 {
  val name = "Sha1NativePooled"
  val desc = "Default JVM Sha1 implementation - thread safety through digester objects pool"
  // default digester is not thread safe, so using pooling
  import java.security.MessageDigest
  import io.github.andrebeat.pool._
  private def md() = MessageDigest.getInstance("SHA-1")
  private val pool = Pool[MessageDigest](16, md)
  def makehash(pass: Pass): Hash = pool.acquire.use(md => md.digest(pass))
}

class Sha1Apache() extends Sha1 {
  val name = "Sha1Apache"
  val desc = "Apache Sha1 implementation coming from commons-codec library"
  import org.apache.commons.codec.digest.DigestUtils
  def makehash(pass: Pass): Hash = DigestUtils.sha1(pass)
}

class Sha1Saphir() extends Sha1 {
  val name = "Sha1Saphir"
  val desc = "Saphir Sha1 implementation coming from saphir-hash-jca library - not thread safe"
  // saphir digester is not thread safe
  import java.security.MessageDigest
  import fr.cryptohash.JCAProvider
  private val md = MessageDigest.getInstance("SHA-1", new JCAProvider)
  def makehash(pass: Pass): Hash = md.digest(pass)
}

class Sha1SaphirPooled() extends Sha1 {
  val name = "Sha1SaphirPooled"
  val desc = "Saphir Sha1 implementation coming from saphir-hash-jca library - thread safety through digester objects pool"
  // saphir digester is not thread safe, so using pooling
  import java.security.MessageDigest
  import fr.cryptohash.JCAProvider
  import io.github.andrebeat.pool._
  private def md() = MessageDigest.getInstance("SHA-1", new JCAProvider)
  private val pool = Pool[MessageDigest](16, md)
  def makehash(pass: Pass): Hash = pool.acquire.use(md => md.digest(pass))
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

trait Progress extends Descriptor {
  def progressMade(context: GoalContext)
}

class StdoutProgress extends Progress with Utils {
  val name = "StdoutProgress"
  val desc = "Basic progress followup which synchronization issue"

  val checkStep = 1000000L
  private var count = 0L
  private var prevtime = 0L
  private var prevcount = 0L

  def progressMade(context: GoalContext) {
    synchronized {
      count += 1
      if (count % checkStep == 0L) {
        if (prevtime == 0) {
          prevtime = now
          prevcount = count
        } else {
          val curtime = now
          if (curtime - prevtime > 5000) { // at least every 10s
            val duration = curtime - prevtime
            val justdonecount = count - prevcount
            prevtime = curtime
            prevcount = count
            val remaintime = (context.combs() - count) * duration / justdonecount / 1000 / 3600
            val remaindays = remaintime / 24
            val rate = justdonecount * 1000L / duration
            println(s"$count total tested, +$justdonecount added in $duration ms - $rate/s - $remaintime hours remaining ($remaindays days)")
          }
        }
      }
    }
  }
}

class StdoutAtomicProgress() extends Progress with Utils {
  val name = "StdoutAtomicProgress"
  val desc = "Basic progress followup using JVM atomic operation"
  import java.util.concurrent.atomic._

  val checkStep = 1000000L
  private val counter = new AtomicLong(0)
  private var prevtime = 0L
  private var prevcount = 0L

  def progressMade(context: GoalContext) {
    val count = counter.addAndGet(1)
    if (count % checkStep == 0L) synchronized {
      if (prevtime == 0) {
        prevtime = now
        prevcount = count
      } else {
        val curtime = now
        if (curtime - prevtime > 5000) { // at least every 10s
          val duration = curtime - prevtime
          val justdonecount = count - prevcount
          prevtime = curtime
          prevcount = count
          val remaintime = (context.combs() - count) * duration / justdonecount / 1000 / 3600
          val remaindays = remaintime / 24
          val rate = justdonecount * 1000L / duration
          println(s"$count total tested, +$justdonecount added in $duration ms - $rate/s - $remaintime hours remaining ($remaindays days)")
        }
      }
    }
  }
}

class PasswordGenerator(context: GoalContext) extends Descriptor {
  val name = "PasswordGenerator"
  val desc = "Iterator based password generator "
  def generator(): Iterator[Pass] = {
    var tmp = Array.fill(context.passwordSize)(0)
    def inc(): Boolean = {
      @annotation.tailrec
      def incworker(pos: Int): Boolean = {
        if (pos == context.passwordSize) false else {
          if (tmp(pos) < context.alphabet.size - 1) {
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
        //val newone = tmp.map(i => context.alphabet(i)) // Slower
        val newone = Array.ofDim[Byte](context.passwordSize)
        for(i <- 0 until context.passwordSize) newone(i)=context.alphabet(tmp(i))
//        var i=0
//        while(i < context.passwordSize) {
//          newone(i)=context.alphabet(tmp(i))
//          i+=1
//        }
        status = inc()
        newone
      }
    }
  }
}

case class GoalContext(
  passwordSize: Int,
  alphabet: Alphabet) {
  def combs() = alphabet.combs(passwordSize)
}

trait Brutalizer extends Descriptor {
  val codec = "US-ASCII"
  val context: GoalContext
  val impls: Implementations

  def testhash(hash2test: Pass, refhash: Hash) = {
    impls.progress.progressMade(context)
    if (hash2test.size != refhash.size) false
    else {
      var i = hash2test.size - 1
      while (i >= 0 && (hash2test(i) == refhash(i))) i -= 1
      i < 0
    }
  }

  def testpassword(password2test: Pass, refhash: Hash): Boolean = {
    val hash2test = impls.sha1.makehash(password2test)
    testhash(hash2test, refhash)
  }
}

class ClassicBrutalizer(val context: GoalContext, val impls: Implementations) extends Brutalizer {
  val name = "ClassicBrutalizer"
  val desc = "Default monothreaded brutalizer implementations with inline password generator"
  def brutalize(inhash: Hash): Option[String] = {
    val curpass = Array.fill(context.passwordSize)(context.alphabet.head)
    def worker(pos: Int): Option[String] = {
      if (pos == context.passwordSize) {
        if (testpassword(curpass, inhash)) {
          val found = new String(curpass, codec)
          println(s"Found : $found")
          Some(found)
        } else None
      } else {
        var result: Option[String] = None
        for { c <- context.alphabet.chars if !result.isDefined } {
          curpass(pos) = c
          result = worker(pos + 1)
        }
        result
      }
    }
    worker(0)
  }
}


class ShorterClassicBrutalizer(val context: GoalContext, val impls: Implementations) extends Brutalizer {
  val name = "ShorterClassicBrutalizer"
  val desc = "Monothreaded brutalizer implementation, it uses the provided password generator"
  def brutalize(inhash: Hash): Option[String] = {
    impls.generator.generator().find(p => testpassword(p, inhash)).map(p=> new String(p))
  }
}


class ParallelBrutalizer(val context: GoalContext, val impls: Implementations) extends Brutalizer {
  val name = "ParallelBrutalizer"
  val desc = "Multithreaded brutalizer implementation"

  import scala.collection.parallel._
  import scala.concurrent._

  //val tasksupport =  new ExecutionContextTaskSupport(ExecutionContext.fromExecutor(impls.executor))
  val tasksupport =  new ThreadPoolTaskSupport()
  
  def parallelize(inhash: Hash)(seq:Seq[Pass]) = {
    val pc = seq.par
    pc.tasksupport = tasksupport
    pc.map(impls.sha1.hashit).find { case (p, h) => testhash(h, inhash) }.map { case (p, h) => new String(p) }
  }
  
  def brutalize(inhash: Hash): Option[String] = {
    impls.generator.generator()
      .grouped(100000)
      .map(parallelize(inhash))
      .toStream
      .filter(_.isDefined)
      .head
  }
}

class ParallelIteraBrutalizer(val context: GoalContext, val impls: Implementations) extends Brutalizer {
  val name = "ParallelIteraBrutalizer"
  val desc = "Multithreaded brutalizer implementation - parallelization over iterator is provided by itera library"
  
  import com.timgroup.iterata.ParIterator.Implicits._

  // TODO : How to setup a custom tasksupport with iterata library ?
  
  implicit lazy val executor = impls.executor
  def brutalize(inhash: Hash): Option[String] = {
    val result = impls.generator.generator()
      .par(100000)
      .map(impls.sha1.hashit)
      .filter { case (p, h) => testhash(h, inhash) }
      .map { case (p, h) => new String(p) }
    if (result.hasNext) Some(result.next) else None
  }
}

class StreamedBrutalizer(val context: GoalContext, val impls: Implementations) extends Brutalizer {
  val name = "Streamed Brutalizer"
  val desc = "Akka streamed based implementation"
  import akka.actor.{ ActorSystem, Actor, Props }
  import akka.stream.{ ActorMaterializer }
  import akka.stream.scaladsl._
  import akka.stream._
  import scala.concurrent.{ Future, Await }
  import scala.concurrent.duration._

  implicit lazy val executor = impls.executor
  implicit val system = ActorSystem("bully-boy")
  implicit val materializer = ActorMaterializer()

  def brutalize(inhash: Hash): Option[String] = {
    val passwords: Source[Pass, Unit] = Source.fromIterator(() => impls.generator.generator())
    //val out = Sink.headOption[Option[String]]
    val out = Sink.foreach[Option[String]](println)

    val passflow = RunnableGraph.fromGraph(GraphDSL.create() { implicit builder =>

      //val finder = Flow[Pass].mapAsyncUnordered(256)(x => Future{if (testpassword(x,inhash)) Some(x) else None })
      val finder = Flow[Pass].map(x => if (testpassword(x, inhash)) Some(x) else None)
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

case class Implementations(
  sha1: Sha1,
  generator: PasswordGenerator,
  progress: Progress,
  executor: ExecutionContextExecutor) extends Descriptor {
  val name = sha1.name+" - "+generator.name+" - "+progress.name+" - "
  val desc = ""
}
  
  
  
object Implementations {
  def defaultExecutor():ExecutionContextExecutor = {
    scala.concurrent.ExecutionContext.Implicits.global
  }
  def fixedPoolExecutor():ExecutionContextExecutor = {
    val cores = Runtime.getRuntime().availableProcessors()
    fixedPoolExecutor(cores)
  }
  def fixedPoolExecutor(n: Int):ExecutionContextExecutor = {
    import scala.concurrent._
    import java.util.concurrent._
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(n))
  }
}

object Brute extends Utils {

  def main(args: Array[String]) {
    // Of course it only makes sense when no salt has been added to the password
    val passlen = 8
    val alphabet = Alphabet(('0' to '9') ++ ('a' to 'z') ++ ('A' to 'Z'))
    alphabet.info(passlen)

    val context = GoalContext(passlen, alphabet)

    //val sha1 = new Sha1Nop()
    val sha1 = new Sha1NativeThreadLocal()
    val generator = new PasswordGenerator(context)
    val progress = new StdoutAtomicProgress()
    val executor = Implementations.fixedPoolExecutor()

    val impls = new Implementations(sha1, generator, progress, executor)

    val brutalizer = new ParallelBrutalizer(context, impls)
    //val brutalizer = new ClassicBrutalizer(context, impls)
    //val brutalizer = new ShorterClassicBrutalizer(context, impls)

    val testpassword = "trucMuc1"
    val inhash = sha1.makehash(testpassword.getBytes(brutalizer.codec))
    println("Bruteforcing sha1=" + toHex(inhash))
    println("Using "+brutalizer.name+" with "+impls.name)

    brutalizer.brutalize(inhash)
  }
}
