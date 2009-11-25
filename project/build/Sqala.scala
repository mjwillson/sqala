import sbt._

class SqalaProject(info: ProjectInfo) extends DefaultProject(info) {
  override def consoleInit = "import com.yumptious.sqala._\nimport com.yumptious.sqala.Implicits._"
}