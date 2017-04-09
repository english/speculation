# frozen_string_literal: true

# Speculation version of
# http://blog.cognitect.com/blog/2016/10/5/interactive-development-with-clojurespec by David
# Chelimsky.

require "bundler/setup"
require "speculation"
require "speculation/gen"
require "speculation/test"

S = Speculation
Gen = S::Gen
STest = S::Test

extend Speculation::NamespacedSymbols

peg = Set[:y, :g, :r, :c, :w, :b]

S.def ns(:code), S.coll_of(peg, :min_count => 4, :max_count => 6)

def score; end

S.fdef method(:score),
       :args => S.cat(:secret => ns(:code), :guess => ns(:code))

S.exercise S.get_spec(method(:score)).args

# [[[[:c, :g, :y, :w, :g, :y], [:y, :y, :c, :r, :y, :y]], {:secret=>[:c, :g, :y, :w, :g, :y], :guess=>[:y, :y, :c, :r, :y, :y]}],
#  [[[:b, :b, :y, :y], [:b, :r, :y, :y, :y]], {:secret=>[:b, :b, :y, :y], :guess=>[:b, :r, :y, :y, :y]}],
#  [[[:y, :y, :w, :g, :r, :w], [:r, :w, :c, :b, :r, :y]], {:secret=>[:y, :y, :w, :g, :r, :w], :guess=>[:r, :w, :c, :b, :r, :y]}],
#  ...

S.fdef method(:score),
       :args => S.and(S.cat(:secret => ns(:code), :guess => ns(:code)),
                      ->(args) { args[:secret].count == args[:guess].count })

S.exercise S.get_spec(method(:score)).args

# [[[[:b, :w, :b, :c, :b], [:y, :r, :y, :y, :b]], {:secret=>[:b, :w, :b, :c, :b], :guess=>[:y, :r, :y, :y, :b]}],
#  [[[:c, :y, :w, :c], [:c, :r, :c, :c]], {:secret=>[:c, :y, :w, :c], :guess=>[:c, :r, :c, :c]}],
#  [[[:g, :g, :y, :g], [:b, :y, :w, :b]], {:secret=>[:g, :g, :y, :g], :guess=>[:b, :y, :w, :b]}],
#  ...

S.def ns(:exact_matches), ns(S, :natural_integer)
S.def ns(:loose_matches), ns(S, :natural_integer)

S.fdef method(:score),
       :args => S.and(S.cat(:secret => ns(:code), :guess => ns(:code)),
                      ->(args) { args[:secret].count == args[:guess].count }),
       :ret  => S.keys(:req => [ns(:exact_matches), ns(:loose_matches)])

S.exercise S.get_spec(method(:score)).ret
# [[{:"main/exact_matches"=>301501626008109845, :"main/loose_matches"=>1592567845535536138},
#   {:"main/exact_matches"=>301501626008109845, :"main/loose_matches"=>1592567845535536138}],
#  [{:"main/exact_matches"=>705260057025726755, :"main/loose_matches"=>4979282811122408}, {:"main/exact_matches"=>705260057025726755, :"main/loose_matches"=>4979282811122408}],
#  [{:"main/exact_matches"=>905565787875512744, :"main/loose_matches"=>965211463791348650},
# ...

S.fdef method(:score),
       :args => S.and(S.cat(:secret => ns(:code), :guess => ns(:code)),
                      ->(args) { args[:secret].count == args[:guess].count }),
       :ret  => S.keys(:req => [ns(:exact_matches), ns(:loose_matches)]),
       :fn   => ->(fn) {
         sum_matches = fn[:ret].values.reduce(&:+)
         sum_matches.between?(0, fn[:args][:secret].count)
       }

def self.score(secret, guess)
  { ns(:exact_matches) => 0,
    ns(:loose_matches) => 0 }
end

STest.check method(:score)
# [{:spec=>Speculation::FSpec(main.score), :"Speculation::Test/ret"=>{:num_tests=>1000, :result=>true}, :method=>#<Method: main.score>}]

def self.score(secret, guess)
  { ns(:exact_matches) => 4,
    ns(:loose_matches) => 3 }
end

# STest.check method(:score)

# [{:spec=>Speculation::FSpec(main.score),
#   :"Speculation::Test/ret"=>
#    {:fail=>[[:y, :b, :r, :r, :b], [:y, :w, :g, :r, :g]],
#     :block=>nil,
#     :num_tests=>1,
#     :result=>
#      #<Speculation::Error: {:"Speculation/problems"=>
#   [{:path=>[:fn],
#     :val=>
#      {:args=>{:secret=>[:y, :b, :r, :r, :b], :guess=>[:y, :w, :g, :r, :g]},
#       :block=>nil,
#       :ret=>{:"main/exact_matches"=>4, :"main/loose_matches"=>3}},
#     :via=>[],

def self.score(secret, guess)
  { ns(:exact_matches) => secret.zip(guess).count { |(a, b)| a.equal?(b) },
    ns(:loose_matches) => 0 }
end

S.exercise_fn method(:score)

# [[[[:y, :b, :b, :b, :b, :c], [:y, :y, :g, :r, :y, :b]], nil, {:"Object/exact_matches"=>1, :"Object/loose_matches"=>0}],
#  [[[:b, :c, :w, :w, :w, :g], [:r, :w, :c, :c, :b, :w]], nil, {:"Object/exact_matches"=>0, :"Object/loose_matches"=>0}],
#  [[[:c, :c, :g, :w, :r], [:g, :g, :g, :c, :w]], nil, {:"Object/exact_matches"=>1, :"Object/loose_matches"=>0}],
#  [[[:b, :y, :g, :y, :b, :y], [:g, :w, :b, :y, :r, :c]], nil, {:"Object/exact_matches"=>1, :"Object/loose_matches"=>0}],

STest.check method(:score)

# [{:spec=>Speculation::FSpec(main.score), :"Speculation::Test/ret"=>{:num_tests=>1000, :result=>true}, :method=>#<Method: main.score>}]

def self.score(secret, guess)
  { ns(:exact_matches) => exact_matches(secret, guess),
    ns(:loose_matches) => 0 }
end

def self.exact_matches(secret, guess)
  secret.zip(guess).count { |(a, b)| a.equal?(b) }
end

S.def ns(:secret_and_guess), S.and(S.cat(:secret => ns(:code), :guess => ns(:code)),
                                   ->(args) { args[:secret].count == args[:guess].count })

S.fdef method(:score),
       :args => ns(:secret_and_guess),
       :ret  => S.keys(:req => [ns(:exact_matches), ns(:loose_matches)]),
       :fn   => ->(fn) {
         sum_matches = fn[:ret].values.reduce(&:+)
         sum_matches.between?(0, fn[:args][:secret].count)
       }

S.fdef method(:exact_matches),
       :args => ns(:secret_and_guess),
       :ret  => ns(S, :natural_integer),
       :fn   => ->(fn) { fn[:ret].between?(0, fn[:args][:secret].count) }

S.exercise_fn method(:exact_matches)

# [[[[:y, :y, :c, :g, :b], [:c, :w, :g, :b, :y]], nil, 0],
#  [[[:y, :c, :w, :y, :b], [:y, :w, :c, :c, :r]], nil, 1],
#  [[[:r, :g, :r, :y, :y], [:c, :r, :c, :c, :w]], nil, 0],
#  [[[:r, :b, :r, :c], [:y, :r, :g, :b]], nil, 0],

STest.check method(:exact_matches)
# [{:spec=>Speculation::FSpec(main.exact_matches), :"Speculation::Test/ret"=>{:num_tests=>1000, :result=>true}, :method=>#<Method: main.exact_matches>}]

STest.instrument method(:exact_matches)
S.exercise_fn method(:score)

# [[[[:b, :g, :w, :b, :r], [:b, :g, :y, :g, :g]], nil, {:"Object/exact_matches"=>2, :"Object/loose_matches"=>0}],
#  [[[:c, :w, :g, :b, :y], [:g, :r, :g, :y, :c]], nil, {:"Object/exact_matches"=>1, :"Object/loose_matches"=>0}],
#  [[[:c, :b, :c, :y, :r], [:r, :c, :y, :r, :b]], nil, {:"Object/exact_matches"=>0, :"Object/loose_matches"=>0}],
#  [[[:y, :c, :y, :g, :c, :c], [:w, :r, :b, :w, :b, :w]], nil, {:"Object/exact_matches"=>0, :"Object/loose_matches"=>0}],
#  [[[:b, :b, :w, :c, :g, :c], [:r, :b, :y, :r, :c, :r]], nil, {:"Object/exact_matches"=>1, :"Object/loose_matches"=>0}],

def self.score(secret, guess)
  { ns(:exact_matches) => exact_matches(secret, guess.take(3)),
    ns(:loose_matches) => 0 }
end

# S.exercise_fn method(:score)

# Speculation::Error: Call to 'main.exact_matches' did not conform to spec:
#  In: [1] val: [:w, :y, :c] fails spec: :"Object/code" at: [:args, :guess] predicate: [#<Method: Speculation::Utils.count_between?>, [[:w, :y, :c], 4, 6]]
# Speculation/args [[:r, :b, :c, :y, :b, :r], [:w, :y, :c]]
# Speculation/failure :instrument
# Speculation::Test/caller "(pry):69:in `score'"

def self.score(secret, guess)
  { ns(:exact_matches) => exact_matches(secret, guess),
    ns(:loose_matches) => 0 }
end

def match_count; end

S.fdef method(:match_count),
       :args => ns(:secret_and_guess),
       :ret  => ns(S, :natural_integer),
       :fn   => ->(fn) { fn[:ret].between?(0, fn[:args][:secret].count) }

S.exercise_fn method(:exact_matches), 10, S.get_spec(method(:match_count))

# [[[[:w, :c, :w, :b, :b], [:r, :r, :c, :c, :r]], nil, 0],
#  [[[:y, :b, :g, :y, :y], [:w, :b, :g, :y, :w]], nil, 3],
#  [[[:y, :r, :r, :r, :b], [:b, :w, :b, :c, :r]], nil, 0],
#  [[[:w, :g, :r, :b, :b, :b], [:y, :c, :g, :b, :c, :b]], nil, 2],

STest.check_method method(:exact_matches), S.get_spec(method(:match_count))

# {:spec=>Speculation::FSpec(main.match_count), :"Speculation::Test/ret"=>{:num_tests=>1000, :result=>true}, :method=>#<Method: main.exact_matches>}

STest.instrument method(:exact_matches), :spec => { method(:exact_matches) => S.get_spec(method(:match_count)) }

S.exercise_fn method(:score)

# [[[[:y, :y, :r, :b, :c, :c], [:b, :b, :b, :w, :w, :r]], nil, {:"Object/exact_matches"=>0, :"Object/loose_matches"=>0}],
#  [[[:r, :b, :r, :b, :b], [:r, :y, :c, :y, :y]], nil, {:"Object/exact_matches"=>1, :"Object/loose_matches"=>0}],
#  [[[:c, :y, :y, :w], [:y, :c, :y, :r]], nil, {:"Object/exact_matches"=>1, :"Object/loose_matches"=>0}],
#  [[[:w, :g, :w, :g, :b, :g], [:r, :y, :r, :y, :c, :b]], nil, {:"Object/exact_matches"=>0, :"Object/loose_matches"=>0}],

STest.check method(:score)

# [{:spec=>Speculation::FSpec(main.score), :"Speculation::Test/ret"=>{:num_tests=>1000, :result=>true}, :method=>#<Method: main.score>}]

def self.all_matches(secret, guess)
  frequencies = ->(xs) { xs.group_by(&:itself).transform_values(&:count) }
  select_keys = ->(h, ks) { Hash[ks.zip(h.values_at(*ks))].compact }

  select_keys.call(frequencies.call(secret), guess).
    merge(select_keys.call(frequencies.call(guess), secret)) { |k, a, b| [a, b].min }.
    values.
    reduce(0, &:+)
end

S.exercise_fn method(:all_matches), 10, S.get_spec(method(:match_count))

# [[[[:b, :r, :c, :r, :y, :w], [:r, :r, :c, :w, :y, :w]], nil, 5],
#  [[[:c, :y, :g, :c, :r, :c], [:b, :w, :b, :b, :w, :b]], nil, 0],
#  [[[:w, :g, :y, :r, :y], [:g, :y, :w, :r, :w]], nil, 4],
#  [[[:g, :c, :c, :g], [:w, :g, :y, :w]], nil, 1],

def self.score(secret, guess)
  exact = exact_matches(secret, guess)
  all = all_matches(secret, guess)

  { ns(:exact_matches) => exact,
    ns(:loose_matches) => all - exact }
end

STest.instrument [method(:exact_matches), method(:all_matches)],
                 :spec => { method(:exact_matches) => S.get_spec(method(:exact_matches)),
                            method(:all_matches)   => S.get_spec(method(:exact_matches)) }

S.exercise_fn method(:score)
# [[[[:w, :g, :y, :y], [:w, :c, :c, :r]], nil, {:"Object/exact_matches"=>1, :"Object/loose_matches"=>0}],
#  [[[:y, :w, :g, :b, :b], [:y, :w, :g, :y, :b]], nil, {:"Object/exact_matches"=>4, :"Object/loose_matches"=>0}],
#  [[[:b, :b, :c, :b], [:b, :y, :w, :y]], nil, {:"Object/exact_matches"=>1, :"Object/loose_matches"=>0}],
#  [[[:y, :w, :c, :y, :c], [:y, :g, :w, :w, :r]], nil, {:"Object/exact_matches"=>1, :"Object/loose_matches"=>1}],

STest.summarize_results STest.check method(:score)

# {:total=>1, :check_passed=>1}

result = STest.summarize_results STest.check(method(:score))
result[:total] == result[:check_passed] && !result.key?(:check_failed)

# true
