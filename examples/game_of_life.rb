require "bundler/setup"
require "speculation"
require "speculation/gen"
require "speculation/test"

S = Speculation
STest = S::Test
Gen = S::Gen

S.def :"gol/coordinate", S.with_gen(Integer) { S.gen(S.int_in(-5..5)) }
S.def :"gol/cell", S.tuple(:"gol/coordinate", :"gol/coordinate")
S.def :"gol/world", S.coll_of(:"gol/cell", :kind => Set, :gen_max => 20)

def self.neighbours(cell)
  cell_x, cell_y = cell
  block = (-1..1).flat_map { |x| (-1..1).map { |y| [cell_x - x, cell_y - y] } }.to_set
  block - Set[cell]
end

def self.cell_distance(cell_a, cell_b)
  [
    (cell_a[0] - cell_b[0]).abs,
    (cell_a[1] - cell_b[1]).abs
  ]
end

S.fdef method(:neighbours),
  :args => S.cat(:cell => :"gol/cell"),
  :ret  => S.coll_of(:"gol/cell", :count => 8, :kind => Set),
  :fn   => ->(fn) {
    cell = fn[:args][:cell]
    neighbours = fn[:ret]
    neighbours.all? { |neighbour| [[1, 0], [0, 1], [1, 1]].include?(cell_distance(cell, neighbour)) }
  }

S.exercise_fn(method(:neighbours))
STest.instrument(method(:neighbours))
# STest.summarize_results STest.check(method(:neighbours))

def self.alive_neighbours(world, cell)
  neighbours(cell).intersection(world)
end

def self.alive?(world, cell)
  world.include?(cell)
end

def self.tick(world)
  world.
    flat_map { |cell| neighbours(cell).to_a }.
    select { |cell|
      alive_neighbour_count = alive_neighbours(world, cell).count

      if alive?(world, cell)
        (2..3).cover?(alive_neighbour_count)
      else
        alive_neighbour_count == 3
      end
    }.
    to_set
end

S.fdef method(:tick),
  :args => S.cat(:world => :"gol/world"),
  :ret  => :"gol/world"

S.exercise_fn(method(:tick))
STest.instrument(method(:tick))
# STest.check(method(:tick))

def self.simulation(world)
  Enumerator.new do |yielder|
    loop do
      yielder << world
      world = tick(world)
    end
  end
end

def self.serialize_world(world)
  return [[]] if world.empty?

  min_y, max_y = world.minmax_by(&:last).map(&:last)
  min_x, max_x = world.minmax_by(&:first).map(&:first)

  (min_y.pred..max_y.next).map { |y| (min_x.pred..max_x.next).map { |x| world.include?([x, y]) } }
end

S.fdef method(:serialize_world),
  :args => S.cat(:world => :"gol/world"),
  :ret  => S.coll_of(S.coll_of(:"Speculation/boolean")),
  :fn   => ->(fn) { fn[:ret].flatten.count(&:itself) == fn[:args][:world].count }

def self.print_world(world, out)
  world.each do |line|
    line.each do |cell|
      if cell
        out << "\u2588"
      else
        out << "\u2591"
      end
    end

    out << "\n"
  end

  nil
end

init = Gen.generate(S.gen(:"gol/world"))
worlds = simulation(init).lazy.take_while { |world| !world.empty? }

worlds.first(10).each do |world|
  print "\033[2J"
  puts world.sort.to_s
  print_world(serialize_world(world), STDOUT)
  sleep 0.5
end

# STest.summarize_results STest.check
