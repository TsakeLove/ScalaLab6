package streams

/**
 * This component implements a parser to define terrains from a
 * graphical ASCII representation.
 *
 * When mixing in that component, a level can be defined by
 * defining the field `level` in the following form:
 *
 *   val level =
 *     """------
 *       |--ST--
 *       |--oo--
 *       |--oo--
 *       |------""".stripMargin
 *
 * - The `-` character denotes parts which are outside the terrain
 * - `o` denotes fields which are part of the terrain
 * - `S` denotes the start position of the block (which is also considered
     inside the terrain)
 * - `T` denotes the final position of the block (which is also considered
     inside the terrain)
 *
 * In this example, the first and last lines could be omitted, and
 * also the columns that consist of `-` characters only.
 */
trait StringParserTerrain extends GameDef {

  /**
   * A ASCII representation of the terrain. This field should remain
   * abstract here.
   */
  val level: String

  /**
   * This method returns terrain function that represents the terrain
   * in `levelVector`. The vector contains parsed version of the `level`
   * string. For example, the following level
   *
   *   val level =
   *     """ST
   *       |oo
   *       |oo""".stripMargin
   *
   * is represented as
   *
   *   Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o'))
   *
   * The resulting function should return `true` if the position `pos` is
   * a valid position (not a '-' character) inside the terrain described
   * by `levelVector`.
   */
    //Результирующая функция должна вернуть `true`, если позиция pos (не символ '-') внутри описываемой местности
  def terrainFunction(levelVector: Vector[Vector[Char]]): Pos => Boolean = (p: Pos) => {
    if (p.col < 0 || p.row < 0) false//не минусовые
    else {
      if (p.row >= levelVector.size) false // не вышли за пределы кол
      else {
        if (p.col >= levelVector.apply(p.row).size) false // тк как 2 мерный не вышли за пределы вложеного ров
        else {
          levelVector.apply(p.row).apply(p.col) == 'o' ||
            levelVector.apply(p.row).apply(p.col) == 'S' || levelVector.apply(p.row).apply(p.col) == 'T'
        }
      }
    }
  }
  /**
   * This function should return the position of character `c` in the
   * terrain described by `levelVector`. You can assume that the `c`
   * appears exactly once in the terrain.
   *
   * Hint: you can use the functions `indexWhere` and / or `indexOf` of the
   * `Vector` class
   */
    //Эта функция должна возвращать позицию символа `c`
  def findChar(c: Char, levelVector: Vector[Vector[Char]]): Pos = {
    val positions = for { // по масиву
      i <- 0 until levelVector.size
      l <- 0 until levelVector.apply(i).size
      if (levelVector.apply(i).apply(l) == c) // если эл равен тому что передали
    } yield Pos(i, l)//пос и координаты
    positions.head
  }
  private lazy val vector: Vector[Vector[Char]] =
    Vector(level.split("\r?\n").map(str => Vector(str: _*)).toIndexedSeq: _*)

  lazy val terrain: Terrain = terrainFunction(vector)
  lazy val startPos: Pos = findChar('S', vector)
  lazy val goal: Pos = findChar('T', vector)

}
