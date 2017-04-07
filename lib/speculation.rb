# frozen_string_literal: true

require "concurrent"
require "set"
require "securerandom"

require "speculation/version"
require "speculation/namespaced_symbols"
require "speculation/identifier"
require "speculation/utils"
require "speculation/spec"
require "speculation/error"

module Speculation
  extend NamespacedSymbols

  class << self
    # Enables or disables spec asserts. Defaults to false.
    attr_accessor :check_asserts

    # A soft limit on how many times a branching spec (or/alt/zero_or_more) can
    # be recursed through during generation.  After this a non-recursive branch
    # will be chosen.
    attr_accessor :recursion_limit

    # The number of times an anonymous fn specified by fspec will be
    # (generatively) tested during conform.
    attr_accessor :fspec_iterations

    # The number of elements validated in a collection spec'ed with 'every'.
    attr_accessor :coll_check_limit

    # The number of errors reported by explain in a collection spec'ed with
    # 'every'
    attr_accessor :coll_error_limit
  end

  @check_asserts    = ENV["SPECULATION_CHECK_ASSERTS"] == "true"
  @recursion_limit  = 4
  @fspec_iterations = 21
  @coll_check_limit = 101
  @coll_error_limit = 20

  @registry_ref = Concurrent::Atom.new({})

  INVALID = ns(:invalid)

  # @private
  OP = ns(:op)
  # @private
  ALT = ns(:alt)
  # @private
  AMP = ns(:amp)
  # @private
  PCAT = ns(:pcat)
  # @private
  REP = ns(:rep)
  # @private
  ACCEPT = ns(:accept)
  # @private
  NIL = ns(:nil)
  # @private
  RECURSION_LIMIT = ns(:recursion_limit)
  # @private
  GEN = ns(:gen)
  # @private
  NAME = ns(:name)

  # Can be enabled or disabled at runtime:
  # - enabled/disabled by setting `check_asserts`.
  # - enabled by setting environment variable SPECULATION_CHECK_ASSERTS to the
  #   string "true"
  # Defaults to false if not set.
  # @param spec [Spec]
  # @param x value to validate
  # @return x if x is valid? according to spec
  # @raise [Error] with explain_data plus :Speculation/failure of :assertion_failed
  def self.assert(spec, x)
    return x unless check_asserts
    return x if valid?(spec, x)

    ed = _explain_data(spec, [], [], [], x).merge(ns(:failure) => :assertion_failed)
    out = StringIO.new
    explain_out(ed, out)

    raise Speculation::Error.new("Spec assertion failed\n#{out.string}", ed)
  end

  # @param infinite [Boolean] whether +/- infinity allowed (default true)
  # @param nan [Boolean] whether Flaot::NAN allowed (default true)
  # @param min [Boolean] minimum value (inclusive, default none)
  # @param max [Boolean] maximum value (inclusive, default none)
  # @return [Spec] that validates floats
  def self.float_in(min: nil, max: nil, infinite: true, nan: true)
    preds = [Float]

    preds.push(->(x) { !x.nan? })      unless nan
    preds.push(->(x) { !x.infinite? }) unless infinite
    preds.push(->(x) { x <= max })     if max
    preds.push(->(x) { x >= min })     if min

    min ||= Float::MIN
    max ||= Float::MAX

    gens = [[20, ->(_) { rand(min.to_f..max.to_f) }]]
    gens << [1, ->(r) { r.choose(Float::INFINITY, -Float::INFINITY) }] if infinite
    gens << [1, ->(_) { Float::NAN }] if nan

    spec(self.and(*preds), :gen => ->(rantly) { rantly.freq(*gens) })
  end

  # @param range [Range<Integer>]
  # @return Spec that validates ints in the given range
  def self.int_in(range)
    spec(self.and(Integer, ->(x) { range.include?(x) }),
         :gen => ->(_) { rand(range) })
  end

  # @param time_range [Range<Time>]
  # @return Spec that validates times in the given range
  def self.time_in(time_range)
    spec(self.and(Time, ->(x) { time_range.cover?(x) }),
         :gen => ->(_) { rand(time_range) })
  end

  # @param date_range [Range<Date>]
  # @return Spec that validates dates in the given range
  def self.date_in(date_range)
    spec(self.and(Date, ->(x) { date_range.cover?(x) }),
         :gen => ->(_) { rand(date_range) })
  end

  # @param x [Spec, Object]
  # @return [Spec, false] x if x is a spec, else false
  def self.spec?(x)
    x if x.is_a?(Spec)
  end

  # @param x [Hash, Object]
  # @return [Hash, false] x if x is a (Speculation) regex op, else logical false
  def self.regex?(x)
    Utils.hash?(x) && x[OP] && x
  end

  # @param value return value of a `conform` call
  # @return [Boolean] true if value is the result of an unsuccessful conform
  def self.invalid?(value)
    value.equal?(INVALID)
  end

  # @param spec [Spec]
  # @param value value to conform
  # @return [Symbol, Object] :Speculation/invalid if value does not match spec, else the (possibly destructured) value
  def self.conform(spec, value)
    spec = Identifier(spec)
    specize(spec).conform(value)
  end

  # Takes a spec and a one-arg generator function and returns a version of the spec that uses that generator
  # @param spec [Spec]
  # @param gen [Proc] generator proc that receives a Rantly instance
  # @return [Spec]
  def self.with_gen(spec, gen)
    if regex?(spec)
      spec.merge(ns(:gfn) => gen)
    else
      specize(spec).tap { |s| s.gen = gen }
    end
  end

  # @private
  def self._explain_data(spec, path, via, inn, value)
    probs = specize(spec).explain(path, via, inn, value)

    if probs && probs.any?
      { ns(:problems) => probs }
    end
  end

  # Given a spec and a value x which ought to conform, returns nil if x
  # conforms, else a hash with at least the key :"Speculation/problems" whose
  # value is a collection of problem-hashes, where problem-hash has at least
  # :path :pred and :val keys describing the predicate and the value that failed
  # at that path.
  # @param spec [Spec]
  # @param x value which ought to conform
  # @return [nil, Hash] nil if x conforms, else a hash with at least the key
  #   :Speculation/problems whose value is a collection of problem-hashes,
  #   where problem-hash has at least :path :pred and :val keys describing the
  #   predicate and the value that failed at that path.
  def self.explain_data(spec, x)
    spec = Identifier(spec)
    name = spec_name(spec)
    _explain_data(spec, [], Array(name), [], x)
  end

  # @param ed [Hash] explain data (per 'explain_data')
  # @param out [IO] destination to write explain human readable message to (default STDOUT)
  def self.explain_out(ed, out = STDOUT)
    return out.puts("Success!") unless ed

    ed.fetch(ns(:problems)).each do |prob|
      path, pred, val, reason, via, inn = prob.values_at(:path, :pred, :val, :reason, :via, :in)

      out.print("In: ", inn.to_a.inspect, " ") unless inn.empty?
      out.print("val: ", val.inspect, " fails")
      out.print(" spec: ", via.last.inspect) unless via.empty?
      out.print(" at: ", path.to_a.inspect) unless path.empty?
      out.print(" predicate: ", pred.inspect)
      out.print(", ", reason.inspect) if reason

      prob.each do |k, v|
        unless [:path, :pred, :val, :reason, :via, :in].include?(k)
          out.print("\n\t ", k.inspect, PP.pp(v, String.new))
        end
      end

      out.puts
    end

    ed.each do |k, v|
      out.puts("#{k} #{PP.pp(v, String.new)}") unless k == ns(:problems)
    end

    nil
  end

  # Given a spec and a value that fails to conform, prints an explaination to STDOUT
  # @param spec [Spec]
  # @param x
  def self.explain(spec, x)
    explain_out(explain_data(spec, x))
  end

  # @param spec [Spec]
  # @param x a value that fails to conform
  # @return [String] a human readable explaination
  def self.explain_str(spec, x)
    out = StringIO.new
    explain_out(explain_data(spec, x), out)
    out.string
  end

  # @private
  def self.gensub(spec, overrides, path, rmap)
    overrides ||= {}

    spec = specize(spec)
    gfn = overrides[spec_name(spec) || spec] || overrides[path]
    g = gfn || spec.gen(overrides, path, rmap)

    if g
      Gen.such_that(->(x) { valid?(spec, x) }, g)
    else
      raise Speculation::Error.new("unable to construct gen at: #{path.inspect} for: #{spec.inspect}",
                                   ns(:failure) => :no_gen, ns(:path) => path)
    end
  end

  # Given a spec, returns the generator for it, or raises if none can be
  # constructed.
  #
  # Optionally an overrides hash can be provided which should map
  # spec names or paths (array of symbols) to no-arg generator Procs.
  # These will be used instead of the generators at those names/paths. Note that
  # parent generator (in the spec or overrides map) will supersede those of any
  # subtrees. A generator for a regex op must always return a sequential
  # collection (i.e. a generator for Speculation.zero_or_more should return
  # either an empty array or an array with one item in it)
  #
  # @param spec [Spec]
  # @param overrides <Hash>
  # @return [Proc]
  def self.gen(spec, overrides = nil)
    spec = Identifier(spec)
    gensub(spec, overrides, [], RECURSION_LIMIT => recursion_limit)
  end

  # @private
  def self.Identifier(x)
    case x
    when Method        then Identifier.new(x.receiver, x.name, false)
    when UnboundMethod then Identifier.new(x.owner, x.name, true)
    else x
    end
  end

  # Given a namespace-qualified symbol key, and a spec, spec name, predicate or
  # regex-op makes an entry in the registry mapping key to the spec
  # @param key [Symbol] namespace-qualified symbol
  # @param spec [Spec, Symbol, Proc, Hash] a spec, spec name, predicate or regex-op
  # @return [Symbol, Method]
  def self.def(key, spec)
    key = Identifier(key)

    unless Utils.ident?(key) && (!key.is_a?(Symbol) || NamespacedSymbols.namespace(key))
      raise ArgumentError, "key must be a namespaced Symbol, e.g. #{ns(:my_spec)}, or a Method"
    end

    spec = if spec?(spec) || regex?(spec) || registry[spec]
             spec
           else
             spec_impl(spec, false)
           end

    @registry_ref.swap do |reg|
      reg.merge(key => with_name(spec, key)).freeze
    end

    key.is_a?(Identifier) ? key.get_method : key
  end

  # @return [Hash] the registry hash
  # @see get_spec
  def self.registry
    @registry_ref.value
  end

  # @param key [Symbol, Method]
  # @return [Spec, nil] spec registered for key, or nil
  def self.get_spec(key)
    registry[Identifier(key)]
  end

  # NOTE: it is not generally necessary to wrap predicates in spec when using
  # `S.def` etc., only to attach a unique generator.
  #
  # Optionally takes :gen generator function, which must be a proc of one arg
  # (Rantly instance) that generates a valid value.
  #
  # @param pred [Proc, Method, Set, Class, Regexp, Hash] Takes a single predicate. A
  #   predicate can be one of:
  #
  #   - Proc, e.g. `-> (x) { x.even? }`, will be called with the given value
  #   - Method, e.g. `Foo.method(:bar?)`, will be called with the given value
  #   - Set, e.g. `Set[1, 2]`, will be tested whether it includes the given value
  #   - Class/Module, e.g. `String`, will be tested for case equality (is_a?)
  #     with the given value
  #   - Regexp, e.g. `/foo/`, will be tested using `===` with given value
  #
  #   Can also be passed the result of one of the regex ops - cat, alt,
  #   zero_or_more, one_or_more, zero_or_one, in which case it will return a
  #   regex-conforming spec, useful when nesting an independent regex.
  #
  # @param gen [Proc] generator function, which must be a proc of one
  #   arg (Rantly instance) that generates a valid value.
  # @return [Spec]
  def self.spec(pred, gen: nil)
    if pred
      spec_impl(pred, false).tap do |spec|
        spec.gen = gen if gen
      end
    end
  end

  # Creates and returns a hash validating spec. :req and :opt are both arrays of
  # namespaced-qualified keywords (e.g. ":MyApp/foo"). The validator will ensure
  # the :req keys are present. The :opt keys serve as documentation and may be
  # used by the generator.
  #
  # The :req key array supports 'and_keys' and 'or_keys' for key groups:
  #
  #     S.keys(req: [ns(:x), ns(:y), S.or_keys(ns(:secret), S.and_keys(ns(:user), ns(:pwd)))],
  #            opt: [ns(:z)])
  #
  # There are also _un versions of :req and :opt. These allow you to connect
  # unqualified keys to specs. In each case, fully qualfied keywords are passed,
  # which name the specs, but unqualified keys (with the same name component)
  # are expected and checked at conform-time, and generated during gen:
  #
  #     S.keys(req_un: [:"MyApp/x", :"MyApp/y"])
  #
  # The above says keys :x and :y are required, and will be validated and
  # generated by specs (if they exist) named :"MyApp/x" :"MyApp/y" respectively.
  #
  # In addition, the values of *all* namespace-qualified keys will be validated
  # (and possibly destructured) by any registered specs. Note: there is
  # no support for inline value specification, by design.
  #
  # @param req [Array<Symbol>]
  # @param opt [Array<Symbol>]
  # @param req_un [Array<Symbol>]
  # @param opt_un [Array<Symbol>]
  # @param gen [Proc] generator function, which must be a proc of one arg
  #   (Rantly instance) that generates a valid value
  def self.keys(req: [], opt: [], req_un: [], opt_un: [], gen: nil)
    HashSpec.new(req, opt, req_un, opt_un).tap do |spec|
      spec.gen = gen
    end
  end

  # @see keys
  def self.or_keys(*ks)
    [ns(:or), *ks]
  end

  # @see keys
  def self.and_keys(*ks)
    [ns(:and), *ks]
  end

  # @param key_preds [Hash] Takes key+pred hash
  # @return [Spec] a destructuring spec that returns a two element array containing the key of the first
  #   matching pred and the corresponding value. Thus the 'key' and 'val' functions can be used to
  #   refer generically to the components of the tagged return.
  # @example
  #   S.or(even: -> (n) { n.even? }, small: -> (n) { n < 42 })
  def self.or(key_preds)
    OrSpec.new(key_preds)
  end

  # @param preds [Array] predicate/specs
  # @return [Spec] a spec that returns the conformed value. Successive
  #   conformed values propagate through rest of predicates.
  # @example
  #   S.and(Numeric, -> (n) { n < 42 })
  def self.and(*preds)
    AndSpec.new(preds)
  end

  # @param preds [Array] hash-validating specs (e.g. 'keys' specs)
  # @return [Spec] a spec that returns a conformed hash satisfying all of the specs.
  # @note Unlike 'and', merge can generate maps satisfying the union of the predicates.
  def self.merge(*preds)
    MergeSpec.new(preds)
  end

  # @note that 'every' does not do exhaustive checking, rather it samples
  #   `coll_check_limit` elements. Nor (as a result) does it do any conforming of
  #   elements. 'explain' will report at most coll_error_limit problems. Thus
  #   'every' should be suitable for potentially large collections.
  # @param pred predicate to validate collections with
  # @param opts [Hash] Takes several kwargs options that further constrain the collection:
  # @option opts :kind (nil) a pred/spec that the collection type must satisfy, e.g. `Array`
  #   Note that if :kind is specified and :into is not, this pred must generate in order for every
  #   to generate.
  # @option opts :count [Integer] (nil) specifies coll has exactly this count
  # @option opts :min_count [Integer] (nil) coll has count >= min_count
  # @option opts :max_count [Integer] (nil) coll has count <= max_count
  # @option opts :distinct [Boolean] (nil) all the elements are distinct
  # @option opts :gen_max [Integer] (20) the maximum coll size to generate
  # @option opts :into [Array, Hash, Set] (Array) one of [], {}, Set[], the
  #   default collection to generate into (default: empty coll as generated by
  #   :kind pred if supplied, else [])
  # @option opts :gen [Proc] generator proc, which must be a proc of one arg
  #   (Rantly instance) that generates a valid value.
  # @see coll_of
  # @see every_kv
  # @return [Spec] spec that validates collection elements against pred
  def self.every(pred, opts = {})
    gen = opts.delete(:gen)

    EverySpec.new(pred, opts).tap do |spec|
      spec.gen = gen
    end
  end

  # Like 'every' but takes separate key and val preds and works on associative collections.
  #
  # Same options as 'every', :into defaults to {}
  #
  # @see every
  # @see hash_of
  # @param kpred key pred
  # @param vpred val pred
  # @param options [Hash]
  # @return [Spec] spec that validates associative collections
  def self.every_kv(kpred, vpred, options)
    every(tuple(kpred, vpred), ns(:kfn) => ->(_i, v) { v.first },
                               :into    => {},
                               **options)
  end

  # Returns a spec for a collection of items satisfying pred. Unlike 'every', coll_of will
  # exhaustively conform every value.
  #
  # Same options as 'every'. conform will produce a collection corresponding to :into if supplied,
  # else will match the input collection, avoiding rebuilding when possible.
  #
  # @see every
  # @see hash_of
  # @param pred
  # @param opts [Hash]
  # @return [Spec]
  def self.coll_of(pred, opts = {})
    every(pred, ns(:conform_all) => true, **opts)
  end

  # Returns a spec for a hash whose keys satisfy kpred and vals satisfy vpred.
  # Unlike 'every_kv', hash_of will exhaustively conform every value.
  #
  # Same options as 'every', :kind defaults to `Speculation::Utils.hash?`, with
  # the addition of:
  #
  # :conform_keys - conform keys as well as values (default false)
  #
  # @see every_kv
  # @param kpred key pred
  # @param vpred val pred
  # @param options [Hash]
  # @return [Spec]
  def self.hash_of(kpred, vpred, options = {})
    every_kv(kpred, vpred, :kind            => Utils.method(:hash?),
                           ns(:conform_all) => true,
                           **options)
  end

  # @param pred
  # @return [Hash] regex op that matches zero or more values matching pred. Produces
  #   an array of matches iff there is at least one match
  def self.zero_or_more(pred)
    rep(pred, pred, [], false)
  end

  # @param pred
  # @return [Hash] regex op that matches one or more values matching pred. Produces
  # an array of matches
  def self.one_or_more(pred)
    pcat(:predicates => [pred, rep(pred, pred, [], true)], :return_value => [])
  end

  # @param pred
  # @return [Hash] regex op that matches zero or one value matching pred. Produces a
  # single value (not a collection) if matched.
  def self.zero_or_one(pred)
    _alt([pred, accept(NIL)], nil)
  end

  # @param kv_specs [Hash] key+pred pairs
  # @example
  #   S.alt(even: :even?.to_proc, small: -> (n) { n < 42 })
  # @return [Hash] regex op that returns a two item array containing the key of the
  #   first matching pred and the corresponding value. Thus can be destructured
  #   to refer generically to the components of the return.
  def self.alt(kv_specs)
    _alt(kv_specs.values, kv_specs.keys).merge(:id => SecureRandom.uuid)
  end

  # @example
  #   S.cat(e: :even?.to_proc, o: :odd?.to_proc)
  # @param named_specs [Hash] key+pred hash
  # @return [Hash] regex op that matches (all) values in sequence, returning a map
  #   containing the keys of each pred and the corresponding value.
  def self.cat(named_specs)
    keys = named_specs.keys
    predicates = named_specs.values

    pcat(:keys => keys, :predicates => predicates, :return_value => {})
  end

  # @param re [Hash] regex op
  # @param preds [Array] predicates
  # @return [Hash] regex-op that consumes input as per re but subjects the
  #   resulting value to the conjunction of the predicates, and any conforming
  #   they might perform.
  def self.constrained(re, *preds)
    { OP => AMP, :p1 => re, :predicates => preds }
  end

  # @param f predicate function with the semantics of conform i.e. it should
  #   return either a (possibly converted) value or :"Speculation/invalid"
  # @return [Spec] a spec that uses pred as a predicate/conformer.
  def self.conformer(f)
    spec_impl(f, true)
  end

  # Takes :args :ret and (optional) :block and :fn kwargs whose values are preds and returns a spec
  # whose conform/explain take a method/proc and validates it using generative testing. The
  # conformed value is always the method itself.
  #
  # fspecs can generate procs that validate the arguments and fabricate a return value compliant
  # with the :ret spec, ignoring the :fn spec if present.
  #
  # @param args predicate
  # @param ret predicate
  # @param fn predicate
  # @param block predicate
  # @param gen [Proc] generator proc, which must be a proc of one arg (Rantly
  #   instance) that generates a valid value.
  # @return [Spec]
  # @see fdef See 'fdef' for a single operation that creates an fspec and registers it, as well as a
  #   full description of :args, :block, :ret and :fn
  def self.fspec(args: nil, ret: nil, fn: nil, block: nil, gen: nil)
    FSpec.new(:args => spec(args), :ret => spec(ret), :fn => spec(fn), :block => spec(block)).tap do |spec|
      spec.gen = gen
    end
  end

  # @param preds [Array] one or more preds
  # @return [Spec] a spec for a tuple, an array where each element conforms to
  #   the corresponding pred. Each element will be referred to in paths using its
  #   ordinal.
  def self.tuple(*preds)
    TupleSpec.new(preds)
  end

  # Once registered, specs are checked by instrument and tested by the runner Speculation::Test.check
  #
  # @example to register method specs for the Hash[] method:
  #   S.fdef(Hash.method(:[]),
  #     args: S.alt(
  #       hash: Hash,
  #       array_of_pairs: S.coll_of(S.tuple(ns(S, :any), ns(S, :any)), kind: Array),
  #       kvs: S.constrained(S.one_or_more(ns(S, :any)), -> (kvs) { kvs.count.even? })
  #     ),
  #     ret: Hash
  #   )
  #
  # @param method [Method]
  # @param spec [Hash]
  # @option spec :args [Hash] regex spec for the method arguments as a list
  # @option spec :block an fspec for the method's block
  # @option spec :ret a spec for the method's return value
  # @option spec :fn a spec of the relationship between args and ret - the value passed is
  #   { args: conformed_args, block: given_block, ret: conformed_ret } and is expected to contain
  #   predicates that relate those values
  # @return [Method] the method spec'ed
  # @note Note that :fn specs require the presence of :args and :ret specs to conform values, and so :fn
  #   specs will be ignored if :args or :ret are missing.
  def self.fdef(method, spec)
    self.def(Identifier(method), fspec(spec))
    method
  end

  # @param spec
  # @param x
  # @return [Boolean] true when x is valid for spec.
  def self.valid?(spec, x)
    spec = Identifier(spec)
    spec = specize(spec)

    !invalid?(spec.conform(x))
  end

  # @param pred
  # @return [Spec] a spec that accepts nil and values satisfying pred
  def self.nilable(pred)
    NilableSpec.new(pred)
  end

  # @param spec
  # @return [Spec] a spec that has the same properies as the given spec, except
  #   `conform` will return the original (not the conformed) value. Note, will
  #   specize regex ops.
  def self.nonconforming(spec)
    NonconformingSpec.new(spec)
  end

  # Generates a number (default 10) of values compatible with spec and maps
  # conform over them, returning a sequence of [val conformed-val] tuples.
  # @param spec
  # @param n [Integer]
  # @param overrides [Hash] a generator overrides hash as per `gen`
  # @return [Array] an array of [val, conformed_val] tuples
  # @see gen for generator overrides
  def self.exercise(spec, n: 10, overrides: {})
    Gen.sample(gen(spec, overrides), n).map { |value|
      [value, conform(spec, value)]
    }
  end

  # Exercises the method by applying it to n (default 10) generated samples of
  # its args spec. When fspec is supplied its arg spec is used, and method can
  # be a proc.
  # @param method [Method]
  # @param n [Integer]
  # @param fspec [Spec]
  # @return [Array] an array of triples of [args, block, ret].
  def self.exercise_fn(method, n = 10, fspec = nil)
    fspec ||= get_spec(method)
    raise ArgumentError, "No fspec found for #{method}" unless fspec

    block_gen = fspec.block ? gen(fspec.block) : Utils.constantly(nil)
    gen = Gen.tuple(gen(fspec.args), block_gen)

    Gen.sample(gen, n).map { |(args, block)| [args, block, method.call(*args, &block)] }
  end

  ### impl ###

  # @private
  def self.recur_limit?(rmap, id, path, k)
    rmap[id] > rmap[RECURSION_LIMIT] &&
      path.include?(k)
  end

  # @private
  def self.inck(h, k)
    h.merge(k => h.fetch(k, 0).next)
  end

  # @private
  def self.dt(pred, x)
    return x unless pred

    spec = the_spec(pred)

    if spec
      conform(spec, x)
    elsif pred.is_a?(Module) || pred.is_a?(::Regexp)
      pred === x ? x : INVALID
    elsif pred.is_a?(Set)
      pred.include?(x) ? x : INVALID
    elsif pred.respond_to?(:call)
      pred.call(x) ? x : INVALID
    else
      raise "#{pred} is not a class, proc, set or regexp"
    end
  end

  # internal helper function that returns true when x is valid for spec.
  # @private
  def self.pvalid?(pred, x)
    !invalid?(dt(pred, x))
  end

  # @private
  def self.explain1(pred, path, via, inn, value)
    spec = maybe_spec(pred)

    if spec?(spec)
      name = spec_name(spec)
      via = Utils.conj(via, name) if name

      spec.explain(path, via, inn, value)
    else
      [{ :path => path, :val => value, :via => via, :in => inn, :pred => [pred, [value]] }]
    end
  end

  # @private
  def self.spec_impl(pred, should_conform)
    if spec?(pred)
      pred
    elsif regex?(pred)
      RegexSpec.new(pred)
    elsif Utils.ident?(pred)
      the_spec(pred)
    else
      PredicateSpec.new(pred, should_conform)
    end
  end

  # @private
  def self.explain_pred_list(preds, path, via, inn, value)
    return_value = value

    preds.each do |pred|
      nret = dt(pred, return_value)

      if invalid?(nret)
        return explain1(pred, path, via, inn, return_value)
      else
        return_value = nret
      end
    end

    nil
  end

  ### regex

  # @private
  def self.re_gen(p, overrides, path, rmap)
    origp = p
    p = reg_resolve!(p)

    id, op, ps, ks, p1, p2, ret, id, gen = p.values_at(
      :id, OP, :predicates, :keys, :p1, :p2, :return_value, :id, GEN
    ) if regex?(p)

    id = p.id if spec?(p)
    ks ||= []

    rmap = inck(rmap, id) if id

    ggens = ->(preds, keys) do
      preds.zip(keys).map do |pred, k|
        unless rmap && id && k && recur_limit?(rmap, id, path, k)
          if id
            Gen.delay { Speculation.re_gen(pred, overrides, k ? Utils.conj(path, k) : path, rmap) }
          else
            re_gen(pred, overrides, k ? Utils.conj(path, k) : path, rmap)
          end
        end
      end
    end

    ogen = overrides[spec_name(origp)] ||
      overrides[spec_name(p)] ||
      overrides[path]

    if ogen
      if [:accept, nil].include?(op)
        return ->(rantly) { [*ogen.call(rantly)] }
      else
        return ->(rantly) { ogen.call(rantly) }
      end
    end

    return gen if gen

    if p
      case op
      when ACCEPT
        if ret == NIL
          ->(_rantly) { [] }
        else
          ->(_rantly) { [ret] }
        end
      when nil
        g = gensub(p, overrides, path, rmap)

        ->(rantly) { [g.call(rantly)] }
      when AMP
        re_gen(p1, overrides, path, rmap)
      when PCAT
        gens = ggens.call(ps, ks)

        if gens.all?
          ->(rantly) do
            gens.flat_map { |gg| gg.call(rantly) }
          end
        end
      when ALT
        gens = ggens.call(ps, ks).compact

        ->(rantly) { rantly.branch(*gens) } unless gens.empty?
      when REP
        if recur_limit?(rmap, id, [id], id)
          ->(_rantly) { [] }
        else
          g = re_gen(p2, overrides, path, rmap)

          if g
            ->(rantly) do
              rantly.range(0, 20).times.flat_map { g.call(rantly) }
            end
          end
        end
      end
    end
  end

  # @private
  def self.re_conform(regex, data)
    data.each do |x|
      regex = deriv(regex, x)
      return INVALID unless regex
    end

    if accept_nil?(regex)
      return_value = preturn(regex)

      return_value == NIL ? nil : return_value
    else
      INVALID
    end
  end

  # @private
  def self.re_explain(path, via, inn, regex, input)
    p = regex

    input.each_with_index do |value, index|
      dp = deriv(p, value)

      if dp
        p = dp
        next
      end

      if accept?(p)
        if p[OP] == PCAT
          return op_explain(p, path, via, Utils.conj(inn, index), input[index..-1])
        else
          return [{ :path   => path,
                    :reason => "Extra input",
                    :val    => input,
                    :via    => via,
                    :in     => Utils.conj(inn, index) }]
        end
      else
        return op_explain(p, path, via, Utils.conj(inn, index), input[index..-1]) ||
            [{ :path   => path,
               :reason => "Extra input",
               :val    => input,
               :via    => via,
               :in     => Utils.conj(inn, index) }]
      end
    end

    if accept_nil?(p)
      nil # success
    else
      op_explain(p, path, via, inn, nil)
    end
  end

  class << self
    private

    # returns the spec/regex at end of alias chain starting with k, throws if not found, k if k not ident
    def reg_resolve!(key)
      return key unless Utils.ident?(key)
      spec = reg_resolve(key)

      if spec
        spec
      else
        raise "Unable to resolve spec: #{key}"
      end
    end

    def deep_resolve(reg, spec)
      spec = reg[spec] while Utils.ident?(spec)
      spec
    end

    # returns the spec/regex at end of alias chain starting with k, nil if not found, k if k not ident
    def reg_resolve(key)
      return key unless Utils.ident?(key)

      spec = @registry_ref.value[key]

      if Utils.ident?(spec)
        deep_resolve(registry, spec)
      else
        spec
      end
    end

    def with_name(spec, name)
      if Utils.ident?(spec)
        spec
      elsif regex?(spec)
        spec.merge(NAME => name)
      else
        spec.tap { |s| s.name = name }
      end
    end

    def spec_name(spec)
      if Utils.ident?(spec)
        spec
      elsif regex?(spec)
        spec[NAME]
      elsif spec.respond_to?(:name)
        spec.name
      end
    end

    # spec_or_key must be a spec, regex or ident, else returns nil. Raises if
    # unresolvable ident (Speculation::Utils.ident?)
    def the_spec(spec_or_key)
      spec = maybe_spec(spec_or_key)
      return spec if spec

      if Utils.ident?(spec_or_key)
        raise "Unable to resolve spec: #{spec_or_key}"
      end
    end

    # spec_or_key must be a spec, regex or resolvable ident, else returns nil
    def maybe_spec(spec_or_key)
      spec = (Utils.ident?(spec_or_key) && reg_resolve(spec_or_key)) ||
        spec?(spec_or_key) ||
        regex?(spec_or_key) ||
        nil

      if regex?(spec)
        with_name(RegexSpec.new(spec), spec_name(spec))
      else
        spec
      end
    end

    def and_preds(x, preds)
      preds.each do |pred|
        x = dt(pred, x)
        return INVALID if invalid?(x)
      end

      x
    end

    def specize(spec)
      if spec?(spec)
        spec
      else
        case spec
        when Symbol, Identifier
          specize(reg_resolve!(spec))
        else
          spec_impl(spec, false)
        end
      end
    end

    ### regex ###

    def accept(x)
      { OP => ACCEPT, :return_value => x }
    end

    def accept?(hash)
      if hash.is_a?(Hash)
        hash[OP] == ACCEPT
      end
    end

    def pcat(regex)
      predicate, *rest_predicates = regex[:predicates]

      keys = regex[:keys]
      key, *rest_keys = keys

      return unless regex[:predicates].all?

      unless accept?(predicate)
        return { OP            => PCAT,
                 :predicates   => regex[:predicates],
                 :keys         => keys,
                 :return_value => regex[:return_value] }
      end

      val = keys ? { key => predicate[:return_value] } : predicate[:return_value]
      return_value = Utils.conj(regex[:return_value], val)

      if rest_predicates.any?
        pcat(:predicates   => rest_predicates,
             :keys         => rest_keys,
             :return_value => return_value)
      else
        accept(return_value)
      end
    end

    def rep(p1, p2, return_value, splice)
      return unless p1

      regex = { OP => REP, :p2 => p2, :splice => splice, :id => SecureRandom.uuid }

      if accept?(p1)
        regex.merge(:p1 => p2, :return_value => Utils.conj(return_value, p1[:return_value]))
      else
        regex.merge(:p1 => p1, :return_value => return_value)
      end
    end

    def filter_alt(ps, ks)
      if ks
        pks = ps.zip(ks).select { |(p, _k)| yield(p) }
        [pks.map(&:first), pks.map(&:last)]
      else
        [ps.select { |p| yield(p) }, ks]
      end
    end

    def _alt(predicates, keys)
      predicates, keys = filter_alt(predicates, keys) { |p| p }
      return if predicates.empty?

      predicate, *rest_predicates = predicates
      key, *_rest_keys = keys

      return_value = { OP => ALT, :predicates => predicates, :keys => keys }
      return return_value unless rest_predicates.empty?

      return predicate unless key
      return return_value unless accept?(predicate)

      accept([key, predicate[:return_value]])
    end

    def alt2(p1, p2)
      if p1 && p2
        _alt([p1, p2], nil)
      else
        p1 || p2
      end
    end

    def no_ret?(p1, pret)
      return true if pret == NIL

      regex = reg_resolve!(p1)
      op = regex[OP]

      [REP, PCAT].include?(op) && pret.empty? || nil
    end

    def accept_nil?(regex)
      regex = reg_resolve!(regex)
      return unless regex?(regex)

      case regex[OP]
      when ACCEPT then true
      when PCAT   then regex[:predicates].all? { |p| accept_nil?(p) }
      when ALT    then regex[:predicates].any? { |p| accept_nil?(p) }
      when REP    then (regex[:p1] == regex[:p2]) || accept_nil?(regex[:p1])
      when AMP
        p1 = regex[:p1]

        return false unless accept_nil?(p1)

        no_ret?(p1, preturn(p1)) ||
          !invalid?(and_preds(preturn(p1), regex[:predicates]))
      else
        raise "Unexpected #{OP} #{regex[OP]}"
      end
    end

    def preturn(regex)
      regex = reg_resolve!(regex)
      return unless regex?(regex)

      p0, *_pr = regex[:predicates]
      k, *_ks = regex[:keys]

      case regex[OP]
      when ACCEPT then regex[:return_value]
      when PCAT   then add_ret(p0, regex[:return_value], k)
      when REP    then add_ret(regex[:p1], regex[:return_value], k)
      when AMP
        pret = preturn(regex[:p1])

        if no_ret?(regex[:p1], pret)
          NIL
        else
          and_preds(pret, regex[:predicates])
        end
      when ALT
        pred, key = regex[:predicates].zip(Array(regex[:keys])).find { |(p, _k)| accept_nil?(p) }

        r = if pred.nil?
              NIL
            else
              preturn(pred)
            end

        key ? [key, r] : r
      else
        raise "Unexpected #{OP} #{regex[OP]}"
      end
    end

    def add_ret(regex, r, key)
      regex = reg_resolve!(regex)
      return r unless regex?(regex)

      case regex[OP]
      when ACCEPT, ALT, AMP
        return_value = preturn(regex)

        if return_value == NIL
          r
        else
          Utils.conj(r, key ? { key => return_value } : return_value)
        end
      when PCAT, REP
        return_value = preturn(regex)

        if return_value.empty?
          r
        else
          val = key ? { key => return_value } : return_value

          regex[:splice] ? Utils.into(r, val) : Utils.conj(r, val)
        end
      else
        raise "Unexpected #{OP} #{regex[OP]}"
      end
    end

    def deriv(predicate, value)
      predicate = reg_resolve!(predicate)
      return unless predicate

      unless regex?(predicate)
        return_value = dt(predicate, value)

        return if invalid?(return_value)
        return accept(return_value)
      end

      regex = predicate

      predicates, p1, p2, keys, return_value, splice =
        regex.values_at(:predicates, :p1, :p2, :keys, :return_value, :splice)

      pred, *rest_preds = predicates
      key, *rest_keys = keys

      case regex[OP]
      when ACCEPT then nil
      when PCAT
        regex1 = pcat(:predicates => [deriv(pred, value), *rest_preds], :keys => keys, :return_value => return_value)
        regex2 = nil

        if accept_nil?(pred)
          regex2 = deriv(
            pcat(:predicates => rest_preds, :keys => rest_keys, :return_value => add_ret(pred, return_value, key)),
            value
          )
        end

        alt2(regex1, regex2)
      when ALT
        _alt(predicates.map { |p| deriv(p, value) }, keys)
      when REP
        regex1 = rep(deriv(p1, value), p2, return_value, splice)
        regex2 = nil

        if accept_nil?(p1)
          regex2 = deriv(rep(p2, p2, add_ret(p1, return_value, nil), splice), value)
        end

        alt2(regex1, regex2)
      when AMP
        p1 = deriv(p1, value)
        return unless p1

        if p1[OP] == ACCEPT
          ret = and_preds(preturn(p1), predicates)
          accept(ret) unless invalid?(ret)
        else
          constrained(p1, *predicates)
        end
      else
        raise "Unexpected #{OP} #{regex[OP]}"
      end
    end

    def insufficient(pred, path, via, inn)
      [{ :path   => path,
         :reason => "Insufficient input",
         :pred   => [pred, []],
         :val    => [],
         :via    => via,
         :in     => inn }]
    end

    def op_explain(p, path, via, inn, input)
      p = reg_resolve!(p)
      return unless p

      input ||= []
      x = input.first

      unless regex?(p)
        if input.empty?
          return insufficient(p, path, via, inn)
        else
          return explain1(p, path, via, inn, x)
        end
      end

      case p[OP]
      when ACCEPT then nil
      when AMP
        if input.empty?
          if accept_nil?(p[:p1])
            explain_pred_list(p[:predicates], path, via, inn, preturn(p[:p1]))
          else
            insufficient(p, path, via, inn)
          end
        else
          p1 = deriv(p[:p1], x)

          if p1
            explain_pred_list(p[:predicates], path, via, inn, preturn(p1))
          else
            op_explain(p[:p1], path, via, inn, input)
          end
        end
      when PCAT
        pks = p[:predicates].zip(Array(p[:keys]))
        pred, k = if pks.count == 1
                    pks.first
                  else
                    pks.find { |(predicate, _)| !accept_nil?(predicate) }
                  end

        path = Utils.conj(path, k) if k

        if input.empty? && !pred
          insufficient(pred, path, via, inn)
        else
          op_explain(pred, path, via, inn, input)
        end
      when ALT
        return insufficient(p, path, via, inn) if input.empty?

        probs = p[:predicates].zip(Array(p[:keys])).flat_map { |(predicate, key)|
          op_explain(predicate, key ? Utils.conj(path, key) : path, via, inn, input)
        }

        probs.compact
      when REP
        op_explain(p[:p1], path, via, inn, input)
      else
        raise "Unexpected #{OP} #{p[OP]}"
      end
    end
  end

  @registry_ref.reset(
    ns(:any)              => with_gen(Utils.constantly(true), ->(r) { r.branch(*Gen::GEN_BUILTINS.values) }),
    ns(:boolean)          => Set[true, false],
    ns(:positive_integer) => with_gen(self.and(Integer, ->(x) { x > 0 }), ->(r) { r.range(1) }),
    # Rantly#positive_integer is actually a natural integer
    ns(:natural_integer)  => with_gen(self.and(Integer, ->(x) { x >= 0 }), :positive_integer.to_proc),
    ns(:negative_integer) => with_gen(self.and(Integer, ->(x) { x < 0 }), ->(r) { r.range(nil, -1) }),
    ns(:empty)            => with_gen(Utils.method(:empty?), Utils.constantly([]))
  )
end
