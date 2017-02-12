# frozen_string_literal: true

# @private
module Speculation
  module UtilsSpecs
    using Speculation::NamespacedSymbols.refine(self)

    S = Speculation
    U = Speculation::Utils

    S.fdef(U.method(:hash?),
           :args => S.tuple(:any.ns(S)),
           :ret  => :boolean.ns(S))

    S.fdef(U.method(:array?),
           :args => S.tuple(:any.ns(S)),
           :ret  => :boolean.ns(S))

    S.fdef(U.method(:collection?),
           :args => S.tuple(:any.ns(S)),
           :ret  => :boolean.ns(S))

    S.fdef(U.method(:identity),
           :args => S.cat(:x => :any.ns(S)),
           :ret  => :any.ns(S),
           :fn   => ->(x) { x[:args][:x].equal?(x[:ret]) })

    S.fdef(U.method(:complement),
           :args  => :empty.ns(S),
           :block => S.fspec(:args => S.zero_or_more(:any.ns(S)),
                             :ret  => :any.ns(S)),
           :ret   => S.fspec(:args => S.zero_or_more(:any.ns(S)),
                             :ret  => :boolean.ns(S)))

    S.fdef(U.method(:constantly),
           :args => S.cat(:x => :any.ns(S)),
           :ret  => Proc,
           :fn   => ->(x) { x[:args][:x].equal?(x[:ret].call) })

    S.fdef(U.method(:distinct?),
           :args => S.cat(:coll => Enumerable),
           :ret  => :boolean.ns(S))

    S.fdef(U.method(:ident?),
           :args => S.cat(:x => :any.ns(S)),
           :ret  => :boolean.ns(S))

    S.fdef(U.method(:method?),
           :args => S.cat(:x => :any.ns(S)),
           :ret  => :boolean.ns(S))

    S.fdef(U.method(:empty),
           :args => S.cat(:coll => Enumerable),
           :ret  => S.and(Enumerable, ->(coll) { coll.empty? }),
           :fn   => ->(x) { x[:args][:coll].class == x[:ret].class })
  end
end
