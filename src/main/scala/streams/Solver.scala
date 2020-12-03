package streams

/**
 * This component implements the solver for the Bloxorz game
 */
trait Solver extends GameDef {

  /**
   * Returns `true` if the block `b` is at the final position
   */
  // проверяем добрались  ли мы к цели
  def done(b: Block): Boolean = b.isStanding && b.b1.col == goal.col && b.b1.row == goal.row

  /**
   * This function takes two arguments: the current block `b` and
   * a list of moves `history` that was required to reach the
   * position of `b`.
   *
   * The `head` element of the `history` list is the latest move
   * that was executed, i.e. the last move that was performed for
   * the block to end up at position `b`.
   *
   * The function returns a lazy list of pairs: the first element of
   * the each pair is a neighboring block, and the second element
   * is the augmented history of moves required to reach this block.
   *
   * It should only return valid neighbors, i.e. block positions
   * that are inside the terrain.
   */
    //возвращаем ленивый список соседних блоков
  def neighborsWithHistory(b: Block, history: List[Move]): LazyList[(Block, List[Move])] = {
    b.legalNeighbors.map(el => (el._1, List(el._2) ::: history)).to(LazyList)
  }

  /**
   * This function returns the list of neighbors without the block
   * positions that have already been explored. We will use it to
   * make sure that we don't explore circular paths.
   */
    //newNeighborsOnly
  def newNeighborsOnly(neighbors: LazyList[(Block, List[Move])],
                       explored: Set[Block]): LazyList[(Block, List[Move])] = {
    neighbors.filter(el1 => !explored.exists(el2 => (el2.b1.col == el1._1.b1.col
      && el2.b1.row == el1._1.b1.row && el2.b2.col == el1._1.b2.col && el2.b2.row == el1._1.b2.row)))
  }

  /**
   * The function `from` returns the lazy list of all possible paths
   * that can be followed, starting at the `head` of the `initial`
   * lazy list.
   *
   * The blocks in the lazy list `initial` are sorted by ascending path
   * length: the block positions with the shortest paths (length of
   * move list) are at the head of the lazy list.
   *
   * The parameter `explored` is a set of block positions that have
   * been visited before, on the path to any of the blocks in the
   * lazy list `initial`. When search reaches a block that has already
   * been explored before, that position should not be included a
   * second time to avoid cycles.
   *
   * The resulting lazy list should be sorted by ascending path length,
   * i.e. the block positions that can be reached with the fewest
   * amount of moves should appear first in the lazy list.
   *
   * Note: the solution should not look at or compare the lengths
   * of different paths - the implementation should naturally
   * construct the correctly sorted lazy list.
   */
  /*Функция from возвращает ленивый список всех возможных путей.
  * за которым можно следовать, начиная с `head` слова` initial`
  * ленивый список.
    *
  * Блоки в ленивом списке `initial` отсортированы по возрастанию
  * длина: позиции блока с кратчайшими путями (длина
  * список перемещений) находятся во главе ленивого списка.
    *
  * Параметр `explored` - это набор позиций блока, которые имеют
    * были посещены ранее, на пути к любому из блоков в
  * ленивый список `начальный`. Когда поиск достигает блока, который уже
    * были исследованы ранее, эту позицию не следует включать в
    * второй раз, чтобы избежать циклов.
    *
  * Полученный ленивый список следует отсортировать по возрастанию длины пути,
  * то есть позиции блока, которые могут быть достигнуты с наименьшим количеством
  * количество ходов должно появиться первым в ленивом списке.
    *
  * Примечание: решение не должно смотреть или сравнивать длины.
  * разных путей - реализация должна естественно
  * построить правильно отсортированный ленивый список.*/
    //возвращает ленивый список всех возможных путей.
  def from(initial: LazyList[(Block, List[Move])],
           explored: Set[Block]): LazyList[(Block, List[Move])] = {
    if (initial.isEmpty) LazyList.empty
    else {
      val s = for {
        path <- initial
        next <- newNeighborsOnly(neighborsWithHistory(path._1, path._2), explored)
      } yield next
      initial ++ from(s, explored ++ (s.map(el => el._1)))
    }
  }
  /**
   * The lazy list of all paths that begin at the starting block.
   */
    //Ленивый список всех путей, начинающихся с начального блока.
  lazy val pathsFromStart: LazyList[(Block, List[Move])] =
    from(LazyList((startBlock, List())), Set())

  /**
   * Returns a lazy list of all possible pairs of the goal block along
   * with the history how it was reached.
   */
    //Возвращает ленивый список всех возможных пар целевого блока вдоль
  lazy val pathsToGoal: LazyList[(Block, List[Move])] = {
    pathsFromStart.filter(el => done(el._1))
  }

  /**
   * The (or one of the) shortest sequence(s) of moves to reach the
   * goal. If the goal cannot be reached, the empty list is returned.
   *
   * Note: the `head` element of the returned list should represent
   * the first move that the player should perform from the starting
   * position.
   */
  lazy val solution: List[Move] = {
    if (pathsToGoal.isEmpty) List() // цель не может быть достигнута
    else pathsToGoal.minBy(path => path._2.length)._2.reverse // возвращаем список последовательности ходов для достижения цели
  }
}
