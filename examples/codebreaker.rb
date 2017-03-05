# frozen_string_literal: true
# See http://blog.cognitect.com/blog/2016/10/5/interactive-development-with-clojurespec

require "bundler/inline"
require "set"

gemfile true do
  gem "speculation",
      :git     => "https://github.com/english/speculation.git",
      :require => ["speculation", "speculation/test", "speculation/gen"]
end

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

# [[[[:c, :w, :r, :c, :c, :b], [:c, :y, :y, :r, :c, :b]], {:"main/exact_matches"=>3, :"main/loose_matches"=>0}],
#  [[[:r, :g, :w, :c, :y], [:y, :w, :r, :w, :r]], {:"main/exact_matches"=>0, :"main/loose_matches"=>0}],
#  [[[:b, :b, :r, :b, :r], [:b, :g, :c, :c, :g]], {:"main/exact_matches"=>1, :"main/loose_matches"=>0}],
#  [[[:g, :r, :b, :y], [:g, :c, :r, :w]], {:"main/exact_matches"=>1, :"main/loose_matches"=>0}],

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

# [[[[:g, :b, :b, :b, :c], [:w, :y, :c, :y, :b]], 0],
#  [[[:c, :w, :r, :c], [:y, :r, :y, :g]], 0],
#  [[[:g, :c, :g, :g, :y], [:g, :g, :c, :c, :r]], 1],
#  [[[:g, :r, :y, :g, :g], [:g, :y, :y, :r, :w]], 2],

STest.check method(:exact_matches)
# [{:spec=>Speculation::FSpec(main.exact_matches), :"Speculation::Test/ret"=>{:num_tests=>1000, :result=>true}, :method=>#<Method: main.exact_matches>}]

STest.instrument method(:exact_matches)
S.exercise_fn method(:score)

# [[[[:y, :r, :y, :r], [:g, :w, :b, :g]], {:"main/exact_matches"=>0, :"main/loose_matches"=>0}],
#  [[[:c, :g, :g, :y, :b, :c], [:w, :y, :g, :b, :y, :c]], {:"main/exact_matches"=>2, :"main/loose_matches"=>0}],
#  [[[:r, :b, :r, :g, :w, :r], [:c, :w, :y, :g, :g, :y]], {:"main/exact_matches"=>1, :"main/loose_matches"=>0}],
#  [[[:r, :y, :c, :y, :y, :b], [:g, :r, :b, :c, :r, :y]], {:"main/exact_matches"=>0, :"main/loose_matches"=>0}],

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

S.exercise_fn method(:exact_matches), :n => 10, :fspec => S.get_spec(method(:match_count))

# [[[[:r, :b, :g, :w, :b], [:b, :c, :c, :r, :w]], 0],
#  [[[:c, :r, :c, :g, :g, :y], [:y, :c, :b, :y, :y, :r]], 0],
#  [[[:c, :g, :r, :y, :y], [:w, :y, :y, :c, :w]], 0],

STest.check_method method(:exact_matches), S.get_spec(method(:match_count))

# {:spec=>Speculation::FSpec(main.match_count), :"Speculation::Test/ret"=>{:num_tests=>1000, :result=>true}, :method=>#<Method: main.exact_matches>}

STest.instrument method(:exact_matches), :spec => { method(:exact_matches) => S.get_spec(method(:match_count)) }

S.exercise_fn method(:score)

# [[[[:y, :r, :y, :c, :y], [:b, :w, :c, :g, :b]], {:"main/exact_matches"=>0, :"main/loose_matches"=>0}],
#  [[[:r, :b, :w, :c, :w, :y], [:w, :r, :w, :g, :b, :r]], {:"main/exact_matches"=>1, :"main/loose_matches"=>0}],
#  [[[:c, :w, :r, :w, :g, :c], [:g, :g, :c, :c, :c, :b]], {:"main/exact_matches"=>0, :"main/loose_matches"=>0}],

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

S.exercise_fn method(:all_matches), :n => 10, :fspec => S.get_spec(method(:match_count))

# [[[[:c, :b, :w, :w], [:b, :c, :g, :y]], 2],
#  [[[:y, :r, :g, :r, :b], [:r, :g, :w, :b, :r]], 4],
#  [[[:r, :g, :r, :g, :g], [:r, :g, :g, :b, :c]], 3],

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
# [[[[:w, :r, :w, :c, :c, :c], [:r, :y, :r, :w, :c, :b]], {:"main/exact_matches"=>1, :"main/loose_matches"=>2}],
#  [[[:r, :c, :w, :y, :c], [:g, :c, :y, :y, :y]], {:"main/exact_matches"=>2, :"main/loose_matches"=>0}],
#  [[[:y, :y, :b, :g, :b], [:g, :w, :b, :c, :g]], {:"main/exact_matches"=>1, :"main/loose_matches"=>1}],
#  [[[:c, :b, :r, :y, :g], [:g, :w, :r, :y, :y]], {:"main/exact_matches"=>2, :"main/loose_matches"=>1}],

STest.summarize_results STest.check method(:score)

# {:total=>1, :check_passed=>1}

result = STest.summarize_results STest.check(method(:score))
result[:total] == result[:check_passed] && !result.key?(:check_failed)

# true
