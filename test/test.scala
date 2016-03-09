import cbt._
import cbt.paths._
import scala.collection.immutable.Seq
import scala.util.Try

// micro framework
object Main{

  def testGraph(implicit logger: Logger) = {
    println("Start Graph Testing.")

    // Sinks 1
    {
      val g = Graph(Set(1, 2, 3), Map(3 -> Set(2), 2 -> Set(1)))
      assert(g.sinks == Set(1))
    }

    // Sinks 2
    {
      val g = Graph(Set(1, 2, 3), Map())
      assert(g.sinks == Set(1, 2, 3))
    }

    // Sinks 3
    {
      val g = Graph(Set(1, 2, 3, 4), Map(2 -> Set(1), 3 -> Set(1), 4 -> Set(2, 3)))
      assert(g.sinks == Set(1))
    }

    // BFS 1
    {
      val g = Graph(Set(1, 2, 3), Map())
      assert(Graph.breadthFirstSearch(g).toList == List(Set(1,2,3)))
    }

    // BFS 2
    {
      val g = Graph(Set(1, 2, 3, 4), Map(2 -> Set(1), 3 -> Set(1), 4 -> Set(2, 3)))
      assert(Graph.breadthFirstSearch(g).toList == List(Set(1), Set(2,3), Set(4)))
    }

    // BFS 3
    {
      val g = Graph(Set(1, 2, 3, 4), Map(2 -> Set(1), 3 -> Set(2), 4 -> Set(3)))
      assert(Graph.breadthFirstSearch(g).toSet == Set(Set(1), Set(2), Set(3), Set(4)))

    }

    println("Finished Graph Testing.")
  }

  def assert(condition: Boolean, msg: String = "")(implicit logger: Logger): Try[Unit] = {
    scala.util.Try{
      Predef.assert(condition, "["++msg++"]")
    }.map{ _ =>
      print(".")
    }.recover{
      case e: AssertionError =>
        println("FAILED")
        e.printStackTrace
    }
  }

  def main(args: Array[String]): Unit = {
    val init = new Init(args)
    implicit val logger: Logger = init.logger

    // Run graph tests
    testGraph
    
    var successes = 0
    var failures = 0
    def assert(condition: Boolean, msg: String = "")(implicit logger: Logger) =
      Main.assert(condition, msg)
        .map(_ => successes += 1)
        .recover { case _ => failures += 1 }

    def runCbt(path: String, args: Seq[String])(implicit logger: Logger): Result = {
      import java.io._
      val allArgs: Seq[String] = ((cbtHome.string ++ "/cbt") +: "direct" +: (args ++ init.propsRaw))
      logger.test(allArgs.toString)
      val pb = new ProcessBuilder( allArgs :_* )
      pb.directory(cbtHome ++ ("/test/" ++ path))
      val p = pb.inheritIO.start
      p.waitFor
      val berr = new BufferedReader(new InputStreamReader(p.getErrorStream));
      val bout = new BufferedReader(new InputStreamReader(p.getInputStream));
      p.waitFor
      import collection.JavaConversions._
      val err = Stream.continually(berr.readLine()).takeWhile(_ != null).mkString("\n")
      val out = Stream.continually(bout.readLine()).takeWhile(_ != null).mkString("\n")
      Result(out, err, p.exitValue == 0)
    }
    case class Result(out: String, err: String, exit0: Boolean)
    def assertSuccess(res: Result)(implicit logger: Logger) = {
      assert(res.exit0, res.toString)
    }

    // tests
    def usage(path: String)(implicit logger: Logger) = {
      val usageString = "Methods provided by CBT"
      val res = runCbt(path, Seq())
      logger.test(res.toString)
      assertSuccess(res)
      assert(res.out == "", "usage " + path +" "+ res.toString)
      assert(res.err contains usageString, "usage " + path +" "+res.toString)
    }
    def compile(path: String)(implicit logger: Logger) = {
      val res = runCbt(path, Seq("compile"))
      assertSuccess(res)
      // assert(res.err == "", res.err) // FIXME: enable this
    }

    logger.test( "Running tests " ++ args.toList.toString )

    //usage("nothing")
    compile("nothing")
    //usage("multi-build")
    compile("multi-build")
    

    {
      val noContext = Context(cbtHome ++ "/test/nothing", Seq(), logger)
      val b = new Build(noContext){
        override def dependencies = Seq(
          MavenDependency("net.incongru.watchservice","barbary-watchservice","1.0")(logger),
          MavenDependency("net.incongru.watchservice","barbary-watchservice","1.0")(logger)
        )
      }
      val cp = b.classpath
      assert(cp.strings.distinct == cp.strings, "duplicates in classpath: " ++ cp.string)
    }

    System.err.println(" DONE!")
    System.err.println( successes.toString ++ " succeeded, "++ failures.toString ++ " failed" )
    if(failures > 0) System.exit(1) else System.exit(0)
  }
}
