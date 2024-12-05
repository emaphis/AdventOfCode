
def part1(input: String): Int =
  totalXMAS(parse(input))

type Grid = IArray[IArray[Char]]

def parse(input: String): Grid =
  IArray.from(
    input.linesIterator.map(IArray.from)
  )

case class Dir(dy: Int, dx: Int)

val dirs = IArray(
  Dir(dy = -1, dx = 0), // up
  Dir(dy = 0, dx = 1), // right
  Dir(dy = 1, dx = 0), // down
  Dir(dy = 0, dx = -1), // left
  Dir(dy = -1, dx = 1), // up-right
  Dir(dy = 1, dx = 1), // down-right
  Dir(dy = 1, dx = -1), // down-left
  Dir(dy = -1, dx = -1) // up-left
)

def boundCheck(x: Int, y: Int, grid: Grid): Boolean =
  x >= 0 && x < grid.length && y >= 0 && y < grid(0).length

def scanner(x: Int, y: Int, dir: Dir, grid: Grid): Iterator[Char] =
  Iterator.unfold((y, x)): (y, x) =>
    Option.when(boundCheck(x, y, grid))(grid(y)(x) -> (y + dir.dy, x + dir.dx))

def scanString(target: String)(x: Int, y: Int, dir: Dir, grid: Grid): Boolean =
  scanner(x, y, dir, grid).take(target.length).corresponds(target)(_ == _)

val scanXMAS = scanString("XMAS")

def totalXMAS(grid: Grid): Int =
  Iterator
    .tabulate(grid.size, grid.size): (y, x) =>
      dirs.count(dir => scanXMAS(x, y, dir, grid))
    .flatten
    .sum

