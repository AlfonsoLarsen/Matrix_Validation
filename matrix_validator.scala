import scala.util.matching.Regex

case class MatrixData(num_rows: Int, num_cols: Int, data: List[List[String]]) {
  override def toString: String = {
    s"Number of rows: $num_rows\n" +
    s"Number of columns: $num_cols\n" +
    s"Data:\n${data.map(_.mkString(", ")).mkString("\n")}"
  }
}

def validateMatrix(matrix: List[List[String]]): MatrixData = {
  val pattern = new Regex("^[A-Z][A-Za-z]{0,10}(_[A-Za-z]{0,10})*$")

  if (!matrix.flatten.forall(_.isInstanceOf[String])) {
    throw new IllegalArgumentException("All elements in the matrix must be strings")
  }

  if (matrix.transpose.exists(column => column.distinct.length != column.length)) {
    throw new IllegalArgumentException("There cannot be repeated elements in each column")
  }

  if (matrix.flatten.exists(element => element == null || element.trim.isEmpty)) {
    throw new IllegalArgumentException("There cannot be null or empty elements")
  }

  if (matrix.flatten.exists(element => pattern.findFirstIn(element).isEmpty)) {
    throw new IllegalArgumentException("Elements must be strings of maximum 12 characters that start with uppercase letters and only contain letters and underscores")
  }

  val num_rows = matrix.length
  val num_cols = matrix.head.length
  MatrixData(num_rows, num_cols, matrix)
}

// Example usage

val matrix = List(
  List("Apple", "Banana", "Cherry"),
  List("Polka", "El_der_ey", "Fig")
)

println(validateMatrix(matrix))
