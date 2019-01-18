# frozen_string_literal: true

# This is a Ruby translation of clojure.spec.gen:
#   https://github.com/clojure/clojure/blob/master/src/clj/clojure/spec/gen.clj
# All credit belongs with Rich Hickey and contributors for their original work.

require "set"
require "radagen"
require "concurrent/delay"
require "date"
require "securerandom"
require "uri"
require "date"
require "time"

module Speculation
  module Gen
    # @param gen [Radagen::Generator]
    # @return single value using gen
    # @see https://github.com/smidas/radagen Radagen
    def self.generate(gen, *args)
      gen.gen(*args)
    end

    # Generate `n` values using `gen`
    # @param gen [Radagen::Generator]
    # @param n [Fixnum] how many values to generate
    # @return [Array] array of generated values using gen
    # @see https://github.com/smidas/radagen Radagen
    def self.sample(gen, *args)
      gen.sample(*args)
    end

    # Given a predicate, returns a built-in generator if one exists.
    # @param pred
    # @return [Radagen::Generator] built-in generator for pred
    # @return nil if no built-in generator found
    # @see https://github.com/smidas/radagen Radagen
    def self.gen_for_pred(pred)
      if pred.is_a?(Set)
        Radagen.elements(pred)
      else
        GEN_BUILTINS[pred]
      end
    end

    # @private
    def self.delay(&block)
      delayed = Concurrent::Delay.new(&block)

      Radagen::Generator.new do |prng, size|
        delayed.value!.call(prng, size)
      end
    end

    # @private
    GEN_BUILTINS = {
      Integer    => Radagen.fixnum,
      String     => Radagen.string_alphanumeric,
      Float      => Radagen.float,
      Numeric    => Radagen.one_of(Radagen.fixnum, Radagen.float, Radagen.rational),
      Rational   => Radagen.rational,
      Symbol     => Radagen.symbol,
      TrueClass  => Radagen.return(true),
      FalseClass => Radagen.return(false),
      NilClass   => Radagen.return(nil),
      Date       => Speculation.gen(Speculation.date_in(Date.new(1970, 1, 1)..Date.new(3000, 1, 1))),
      Time       => Speculation.gen(Speculation.time_in(Time.new(1970, 1, 1)..Time.new(3000, 1, 1))),
      URI        => Radagen.fmap(Radagen.uuid) { |uuid| URI("http://#{uuid}.com") },
      Array      => Radagen.array(Radagen.simple_printable),
      Set        => Radagen.set(Radagen.simple_printable),
      SortedSet  => Radagen.fmap(Radagen.set(Radagen.simple_printable)) { |genned| SortedSet.new(genned) },
      Enumerator => Radagen.fmap(Radagen.one_of(Radagen.array(Radagen.simple_printable), Radagen.hash_map(Radagen.simple_printable, Radagen.simple_printable)), &:to_enum),
      Hash       => Radagen.hash_map(Radagen.simple_printable, Radagen.simple_printable),
      Enumerable => Radagen.one_of(Radagen.array(Radagen.simple_printable), Radagen.hash_map(Radagen.simple_printable, Radagen.simple_printable))
    }.freeze
  end
end
