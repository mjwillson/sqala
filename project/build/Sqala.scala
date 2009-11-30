import sbt._

class SqalaProject(info: ProjectInfo) extends DefaultProject(info) {
  override def consoleInit = "import com.yumptious.sqala.examples._\nimport com.yumptious.sqala.expr.column.Implicits._"
}