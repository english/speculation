require 'test_helper'
require 'speculation/core'
require 'speculation/gen'

class SpeculationGenTest < Minitest::Test
  using Speculation.namespaced_symbols(self)

  S = Speculation::Core
  Gen = Speculation::Gen

  def test_generate
    assert_kind_of Integer, Gen.generate(S.gen(Integer))

    S.def(:string.ns, String)
    assert_kind_of String, Gen.generate(S.gen(:string.ns))

    S.def(:even.ns, S.and(Integer, -> (x) { x.even? }))
    val = Gen.generate(S.gen(:even.ns))
    assert_kind_of Integer, val
    assert val.even?

    S.def(:foo.ns, S.or(neg_int: S.and(Integer, -> (x) { x.negative? }), str: String))
    val = Gen.generate(S.gen(:foo.ns))
    assert (val.is_a?(Integer) && val.negative?) || val.is_a?(String)

    S.def(:x.ns, String)
    S.def(:y.ns, Integer)
    S.def(:z.ns, Integer)
    S.def(:hash.ns, S.keys(req: [:x.ns, :y.ns], opt: [:z.ns]))

    val = Gen.generate(S.gen(:hash.ns))

    assert val.keys.include?(:x.ns)
    assert val.keys.include?(:y.ns)
    assert val[:x.ns].is_a?(String)
    assert val[:y.ns].is_a?(Integer)

    S.def(:hash.ns, S.keys(req_un: [:x.ns, :y.ns], opt_un: [:z.ns]))

    val = Gen.generate(S.gen(:hash.ns))

    assert val.keys.include?(:x)
    assert val.keys.include?(:y)
    assert val[:x].is_a?(String)
    assert val[:y].is_a?(Integer)

    assert val[:z].is_a?(Integer) if val.key?(:z)
  end
end
