package syntax

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.meta.Source
import scala.meta.Tree
import scala.meta.Type
import scala.meta.Type.Compound
import scala.meta._
import scala.reflect.ClassTag
import scala.util.control.NonFatal

import java.util.concurrent.CopyOnWriteArrayList
import java.util.concurrent.atomic.AtomicInteger
trait Show[T] {
  def print(e: T): String
  def lineNumber(e: T): Int
}
object Show {
  implicit def treeShow[T <: Tree]: Show[T] = new Show[T] {
    override def print(e: T): String = e.syntax
    override def lineNumber(e: T): Int = Experiment.linenumber(e)
  }
  def instance[T](f: T => String, g: T => Int): Show[T] = new Show[T] {
    override def print(e: T): String = f(e)
    override def lineNumber(e: T): Int = g(e)
  }
  implicit val compoundShow = new Show[Type.Compound] {
    override def print(e: Type.Compound): String =
      s"""
         |```scala
         |${e.syntax}
         |```""".stripMargin

    override def lineNumber(e: Compound): Int = Experiment.linenumber(e)
  }
}

object Experiment {

  // two statements
  List(1)
  map(_ +)

  // one statement
  1
  + 2

  def collectAnalysis[T](analysis: PartialFunction[Tree, T]): Tree => Seq[T] = {
    ast =>
      ast.collect(analysis)
  }

  def runAnalysis[T: ClassTag](size: Int)(
      analysis: Tree => Seq[T]): mutable.Buffer[(ScalaFile, T)] = {
    val results = new CopyOnWriteArrayList[(ScalaFile, T)]
    val counter = new AtomicInteger()
    val errors = new AtomicInteger()
    ScalaFile.getAll.take(size).toVector.par.foreach { file =>
      val n = counter.incrementAndGet()
      if (n % 1000 == 0) {
        println(n)
      }
      try {
        file.jFile.parse[Source] match {
          case Parsed.Success(ast) =>
            analysis(ast).foreach { t =>
              results.add(file -> t)
            }
          case _ =>
        }
      } catch {
        case e: org.scalameta.invariants.InvariantFailedException => // scala.meta error
        case e: java.nio.charset.MalformedInputException => // scala.meta error
        case e: java.util.NoSuchElementException => // scala.meta error
        case NonFatal(e) =>
          e.printStackTrace()
          println(e.getClass.getSimpleName)
          val i = errors.incrementAndGet()
          if (i > 100) {
            ???
          }
      }
    }
    results.asScala
  }

  def projectAnalysis: Tree => Seq[Type.Project] = { ast =>
    val builder = Seq.newBuilder[Type.Project]
    def loop(tree: Tree)(gamma: Set[String]): Unit = {
      tree match {
        case t: Type.Project if gamma.contains(t.qual.syntax) =>
          builder += t
        case _ =>
      }
      val newBindings: Seq[String] = tree match {
        case HasTypeParams(tparams) => tparams
        case HasTypeStats(tparams) => tparams
        case _ => Nil
      }
      val newGamma = gamma ++ newBindings
      tree.children.foreach(loop(_)(newGamma))
    }
    loop(ast)(Set.empty[String])
    builder.result()
  }

  def linenumber(t: Tree): Int =
    t.tokens.headOption.map(_.pos.start.line + 1).getOrElse(0)

  def prettyPrint[T](buf: Traversable[(ScalaFile, T)])(
      implicit ev: Show[T]): String = {
    buf.map {
      case (file, t) =>
        val url = file.githubUrlAtLine(ev.lineNumber(t))
        s"$url: ${ev.print(t)}"
    }.mkString("\n")
  }

  def runTypeCompounds(): Unit = {
    val compounds = runAnalysis(30000)(collectAnalysis {
      case t: Type.Compound
          if t.refinement.exists(x => x.is[Decl.Var] || x.is[Defn.Var]) =>
        t
    })
    println("Total: " + compounds.length)
    println(prettyPrint(compounds))
  }

  def runTypeProjections(): Unit = {
    val typProjections = runAnalysis(100000)(projectAnalysis)
    println("Total: " + typProjections.length)
    println(prettyPrint(typProjections))
  }

  @tailrec
  final def parents(tree: Tree,
                    accum: Seq[Tree] = Seq.empty[Tree]): Seq[Tree] = {
    tree.parent match {
      case Some(parent) => parents(parent, parent +: accum)
      case _ => accum
    }
  }

  def runInfixNewline(): Unit = {

    /**
      * Creates lookup table from token offset to its closest scala.meta tree.
      */
    def getOwners(tree: Tree): Map[Token, Tree] = {
      val result = Map.newBuilder[Token, Tree]
      def loop(x: Tree): Unit = {
        x.tokens.foreach { tok =>
          result += tok -> x
        }
        x.children.foreach(loop)
      }
      loop(tree)
      result.result()
    }

    def isSymbolic(nme: String) =
      !nme.exists(x => Character.isLetterOrDigit(x))

    val infixOwners = runAnalysis[InfixExperiment](100000) { t =>
      val owners = getOwners(t)
      val statementsStarts = getStatementStarts(t).keys.toSet

      def isCandidate(ft: FormatToken, tok: Token): Boolean =
        ft.newlines > 0 &&
          isSymbolic(tok.syntax) &&
          !owners(tok).parent.exists(_.is[Pat])
      val fts = FormatToken.formatTokens(t.tokens)
      val result = fts.collect {
        // lines ending in infix operator
        case ft @ FormatToken(tok: Token.Ident, _, _)
            if isCandidate(ft, tok) =>
          InfixExperiment(tok, owners(tok), startOfLine = false)
        case ft @ FormatToken(_, tok: Token.Ident, _)
            if isCandidate(ft, tok) =>
          InfixExperiment(tok, owners(tok), startOfLine = true)
      }
      result.filter(x => statementsStarts(x.tok))
    }
//    infixOwners.groupBy(_._2.getClass.getName).mapValues(_.length).foreach {
//      case (a, b) => println(s"$a: $b")
//    }
    val (start, end) = infixOwners.partition(_._2.startOfLine)
//    println(prettyPrint(start))
    var questions = 0
    start
      .groupBy(_._2.ownerName)
      .mapValues(x => x -> x.length)
      .toSeq
      .sortBy(_._2._1.length)
      .foreach {
        case (a, (c, b)) =>
          println(s"$a: $b")
          c.foreach {
            case (x, y) =>
              println(x.githubUrlAtLine(linenumber(y.owner)))
              if (y.tok.syntax == "???") questions += 1
//              if (y.ownerName.endsWith("Apply") ||
//                  y.ownerName.contains("Unary")) {
//              }
          }
      }
    println("???: " + questions)
    println("SOL: " + start.length)
    println("SOL infix: " + start.count(_._2.ownerName.contains("ApplyInfix")))
    println("EOL infix: " + end.count(_._2.ownerName.contains("ApplyInfix")))
    println("Total: " + infixOwners.length)
  }

  def main(args: Array[String]): Unit = {
    println("Experiment!!!")
    runInfixNewline()
//    runTypeProjections()
//    runTypeCompounds()
  }

  def getStatementStarts(tree: Tree): Map[Token, Tree] = {
    import Token._
    import scala.reflect.classTag
    import scala.reflect.ClassTag
    def getEnumStatements(enums: Seq[Enumerator]): Seq[Enumerator] = {
      val ret = Seq.newBuilder[Enumerator]
      enums.zipWithIndex.foreach {
        case (x, 0) => x
        case (enum: Enumerator.Guard, i) =>
          // Only guard that follows another guards starts a statement.
          if (enums(i - 1).is[Enumerator.Guard]) {
            ret += enum
          }
        case (x, _) => ret += x
      }
      ret.result()
    }

    def extractStatementsIfAny(tree: Tree): Seq[Tree] = tree match {
      case b: Term.Block => b.stats
      case t: Pkg => t.stats
      // TODO(olafur) would be nice to have an abstract "For" superclass.
      case t: Term.For => getEnumStatements(t.enums)
      case t: Term.ForYield => getEnumStatements(t.enums)
      case t: Term.Match => t.cases
      case t: Term.PartialFunction => t.cases
      case t: Term.TryWithCases => t.catchp
      case t: Type.Compound => t.refinement
      case t: scala.meta.Source => t.stats
      case t: Template if t.stats.isDefined => t.stats.get
      case t: Case if t.body.tokens.nonEmpty => Seq(t.body)
      case _ => Seq.empty[Tree]
    }

    val ret = Map.newBuilder[Token, Tree]
    ret.sizeHint(tree.tokens.length)

    def addAll(trees: Seq[Tree]): Unit = {
      trees.foreach { t =>
        ret += t.tokens.head -> t
      }
    }

    def addDefn[T: ClassTag](mods: Seq[Mod], tree: Tree): Unit = {
      // Each @annotation gets a separate line
      val annotations = mods.filter(_.is[Mod.Annot])
      addAll(annotations)
      val firstNonAnnotation: Token = mods.collectFirst {
        case x if !x.is[Mod.Annot] =>
          // Non-annotation modifier, for example `sealed`/`abstract`
          x.tokens.head
      }.getOrElse {
        // No non-annotation modifier exists, fallback to keyword like `object`
        tree.tokens.find(x => classTag[T].runtimeClass.isInstance(x)) match {
          case Some(x) => x
          case None => ???
        }
      }
      ret += firstNonAnnotation -> tree
    }

    def loop(x: Tree): Unit = {
      x match {
        case t: Defn.Class => addDefn[KwClass](t.mods, t)
        case t: Defn.Def => addDefn[KwDef](t.mods, t)
        case t: Decl.Def => addDefn[KwDef](t.mods, t)
        case t: Ctor.Secondary => addDefn[KwDef](t.mods, t)
        case t: Defn.Object => addDefn[KwObject](t.mods, t)
        case t: Defn.Trait => addDefn[KwTrait](t.mods, t)
        case t: Defn.Type => addDefn[KwType](t.mods, t)
        case t: Decl.Type => addDefn[KwType](t.mods, t)
        case t: Defn.Val => addDefn[KwVal](t.mods, t)
        case t: Decl.Val => addDefn[KwVal](t.mods, t)
        case t: Defn.Var => addDefn[KwVar](t.mods, t)
        case t: Decl.Var => addDefn[KwVar](t.mods, t)
        case t => // Nothing
          addAll(extractStatementsIfAny(t))
      }
      x.children.foreach(loop)
    }
    loop(tree)
    ret.result()
  }

}

object HasTypeStats {
  def unapply(arg: Tree): Option[Seq[String]] = {
    val stats: Option[Seq[Stat]] = arg match {
      case t: Template if t.stats.nonEmpty => Some(t.stats.get)
      case t: Term.Block => Some(t.stats)
      case _ => None
    }
    stats.map(_.collect {
      case t: Decl.Type => t.name.syntax
      case t: Defn.Type => t.name.syntax
    })
  }

}

case class FormatToken(left: Token, right: Token, between: Vector[Token]) {

  override def toString = s"${left.syntax}∙${right.syntax}"

  def newlines: Int = between.count(_.is[Token.LF])

  def inside(range: Set[Range]): Boolean = {
    if (range.isEmpty) true
    else range.exists(_.contains(right.pos.end.line))
  }

  val leftHasNewline: Boolean = left.syntax.contains('\n')
}

object FormatToken {

  /**
    * Convert scala.meta Tokens to FormatTokens.
    *
    * Since tokens might be very large, we try to allocate as
    * little memory as possible.
    */
  def formatTokens(tokens: Tokens): Array[FormatToken] = {
    var left = tokens.head
    val result = Array.newBuilder[FormatToken]
    val whitespace = Vector.newBuilder[Token]
    tokens.toArray.foreach {
      case t @ Whitespace() => whitespace += t
      case right =>
        val tok = FormatToken(left, right, whitespace.result)
        result += tok
        left = right
        whitespace.clear()
    }
    result.result
  }
}

object Whitespace {
  def unapply(token: Token): Boolean = {
    import Token._
    token.is[Space] ||
    token.is[Tab] ||
    token.is[CR] ||
    token.is[LF] ||
    token.is[FF]
  }
}

object HasTypeParams {
  def unapply(arg: Tree): Option[Seq[String]] = {
    val res = arg match {
      case t: Defn.Def => Some(t.tparams)
      case t: Defn.Trait => Some(t.tparams)
      case t: Defn.Class => Some(t.tparams)
      case _ => None
    }
    res.map(_.map(_.name.syntax))
  }
}

case class InfixExperiment(tok: Token, owner: Tree, startOfLine: Boolean) {
  def ownerName: String =
    owner.parent.get.getClass.getName
      .stripPrefix("scala.meta.")
      .replace("$", ".")
      .replaceFirst("\\.[a-zA-Z]*$", "")
}

object InfixExperiment {
  implicit val infixExperimentShow: Show[InfixExperiment] =
    Show.instance[InfixExperiment](
      x => s"${x.tok.syntax}: ${x.ownerName}",
      x => Experiment.linenumber(x.owner)
    )
}
