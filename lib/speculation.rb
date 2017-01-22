require 'concurrent/atom'
require 'concurrent/delay'
require 'functional'
require 'hamster/hash'
require 'hamster/set'
require 'hamster/vector'
require 'set'
require 'securerandom'

require "speculation/identifier"
require "speculation/namespaced_symbols"
require "speculation/conj"
require "speculation/utils"
require "speculation/version"

module Speculation
  S = self
  H = Hamster::Hash
  V = Hamster::Vector
  Protocol = Functional::Protocol

  using NamespacedSymbols.refine(S)
  using Conj

  # TODO: Make these configurable

  # A soft limit on how many times a branching spec
  # (or/alt/*/opt-keys/multi-spec) can be recursed through during generation.
  # After this a non-recursive branch will be chosen.
  RECURSION_LIMIT = 4

  # The number of times an anonymous fn specified by fspec will be
  # (generatively) tested during conform.
  FSPEC_ITERATIONS = 21

  # The number of elements validated in a collection spec'ed with 'every'.
  COLL_CHECK_LIMIT = 101

  # The number of errors reported by explain in a collection spec'ed with
  # 'every'
  COLL_ERROR_LIMIT = 20

  Functional.SpecifyProtocol(:Spec.ns) do
    instance_method :conform, 1
    instance_method :explain, 4
    instance_method :gen, 3
  end unless Protocol.Specified?(:Spec.ns)

  @registry_ref = Concurrent::Atom.new(H[])

  private_class_method def self.deep_resolve(reg, spec)
    spec = reg[spec] until !Utils.ident?(spec)
    spec
  end

  # returns the spec/regex at end of alias chain starting with k, nil if not
  # found, k if k not ident
  private_class_method def self.reg_resolve(key)
    return key unless Utils.ident?(key)

    spec = @registry_ref.value[key]

    if Utils.ident?(spec)
      deep_resolve(registry, spec)
    else
      spec
    end
  end

  # returns the spec/regex at end of alias chain starting with k, throws if not
  # found, k if k not ident
  # private
  def self._reg_resolve!(key)
    return key unless Utils.ident?(key)
    spec = reg_resolve(key)

    if spec
      spec
    else
      raise "Unable to resolve spec: #{key}"
    end
  end

  # returns spec if spec is a spec object, else logical false, spec spec spec
  def self.spec?(spec)
    # TODO remove Protocol dependency, just use inheritance
    spec if Protocol.Satisfy?(spec, :Spec.ns)
  end

  # returns x if x is a (Speculation) regex op, else logical false
  def self.regex?(x)
    Utils.hash?(x) && x[:op.ns] && x
  end

  private_class_method def self.with_name(spec, name)
    if Utils.ident?(spec)
      spec
    elsif regex?(spec)
      spec.put(:name.ns, name)
    else
      spec.tap { |s| s.name = name }
    end
  end

  private_class_method def self.spec_name(spec)
    if Utils.ident?(spec)
      spec
    elsif regex?(spec)
      spec[:name.ns]
    elsif spec.respond_to?(:name)
      spec.name
    end
  end

  # spec_or_key must be a spec, regex or resolvable ident, else returns nil
  private_class_method def self.maybe_spec(spec_or_key)
    spec = (Utils.ident?(spec_or_key) && reg_resolve(spec_or_key)) ||
      spec?(spec_or_key) ||
      regex?(spec_or_key) ||
      nil

    regex?(spec) ?
      with_name(RegexSpec.new(spec), spec_name(spec)) :
      spec
  end

  # spec_or_key must be a spec, regex or ident, else returns nil. Raises if
  # unresolvable ident (Speculation::Utils.ident?)
  private_class_method def self.the_spec(spec_or_key)
    spec = maybe_spec(spec_or_key)
    return spec if spec

    if Utils.ident?(spec_or_key)
      raise "Unable to resolve spec: #{spec_or_key}"
    end
  end

  Functional.SpecifyProtocol(:Specize.ns) do
    instance_method :specize, 0
  end unless Protocol.Specified?(:Specize.ns)

  using Module.new do
    refine Symbol do
      def specize
        S._reg_resolve!(self).specize
      end
    end

    refine Identifier do
      def specize
        S._reg_resolve!(self).specize
      end
    end

    refine Object do
      def specize
        S.spec_impl(self, false)
      end
    end
  end

  # private
  def self._specize(spec)
    if spec?(spec)
      spec
    else
      case spec
      when Symbol, Identifier
        _specize(S._reg_resolve!(spec))
      else
        spec_impl(spec, false, nil)
      end
    end
  end

  # tests the validity of a conform return value
  def self.invalid?(value)
    value.equal?(:invalid.ns)
  end

  # Given a spec and a value, returns :Speculation/invalid if value does not
  # match spec, else the (possibly destructured) value
  def self.conform(spec, value)
    _specize(spec).conform(value)
  end

  # TODO unform
  # TODO form
  # TODO abbrev
  # TODO describe

  # Takes a spec and a no-arg, generator block and returns a version of that
  # spec that uses that generator
  def self.with_gen(spec, &gen)
    if regex?(spec)
      spec.put(:gfn.ns, gen)
    else
      _specize(spec).with_gen(gen)
    end
  end

  # @private
  def self._explain_data(spec, path, via, _in, value)
    probs = _specize(spec).explain(path, via, _in, value)

    if probs&.any?
      { :problems.ns => probs }
    end
  end

  # Given a spec and a value x which ought to conform, returns nil if x
  # conforms, else a hash with at least the key :"Speculation/problems" whose
  # value is a collection of problem-hashes, where problem-hash has at least
  # :path :pred and :val keys describing the predicate and the value that failed
  # at that path.
  def self.explain_data(spec, x)
    name = spec_name(spec)
    _explain_data(spec, [], [name] || [], [], x)
  end

  # returns explanation data (per 'explain_data') as a human readable string
  def self.explain_str(data)
    return "Success!" unless data

    s = ""

    data.fetch(:problems.ns).each do |prob|
      path, pred, val, reason, via, _in = prob.values_at(:path, :pred, :val, :reason, :via, :in)

      s << "In: #{_in.to_a.inspect} " unless _in.empty?
      s << "val: #{val.inspect} fails"
      s << " spec: #{via.last.inspect}" unless via.empty?
      s << " at: #{path.to_a.inspect}" unless path.empty?
      s << " predicate: #{pred.inspect}"
      s << ", #{reason.inspect}" if reason

      prob.each do |k, v|
        unless [:path, :pred, :val, :reason, :via, :in].include?(k)
          s << "\n\t #{k.inspect} #{v.inspect}"
        end
      end

      s << "\n"
    end

    data.each do |k, v|
      s << "#{k} #{v}\n" unless k == :problems.ns
    end

    s
  end

  # Given a spec and a value that fails to conform, returns an explanation as a
  # string
  def self.explain(spec, x)
    explain_str(explain_data(spec, x))
  end

  # @private
  def self.gensub(spec, overrides, path, rmap)
    overrides ||= {}
    spec = _specize(spec)
    gfn = overrides[spec.name || spec] || overrides[path]
    g = gfn ? gfn : spec.gen(overrides, path, rmap)

    if g
      Gen.such_that(-> (x) { S.valid?(spec, x) }, g, 100)
    else
      raise "unable to construct gen at: #{path.inspect} for: #{spec.inspect}"
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
  def self.gen(spec, overrides = nil)
    gensub(spec, overrides, [], H[:recursion_limit.ns => RECURSION_LIMIT])
  end

  # returns an identifier if x is a method, otherwise x
  private_class_method def self.Identifier(x)
    case x
    when Method
      Identifier.new(x.receiver, x.name, false)
    when UnboundMethod
      Identifier.new(x.owner, x.name, true)
    else
      x
    end
  end

  # Given a namespace-qualified symbol or Speculation::Identifier k, and a spec,
  # spec name, predicate or regex-op makes an entry in the registry mapping k to
  # the spec
  def self.def(key, spec)
    key = Identifier(key)

    unless Utils.ident?(key) && key.namespace
      raise ArgumentError,
        "key must be a namespaced Symbol, e.g. #{:my_spec.ns}, given #{key}, or a Speculation::Identifier"
    end

    spec = if spec?(spec) || regex?(spec) || registry[spec]
             spec
           else
             self.spec_impl(spec, false, nil)
           end

    @registry_ref.swap { |reg| reg.store(key, with_name(spec, key)) }

    key
  end

  # returns the registry hash, prefer 'get_spec' to lookup a spec by name
  def self.registry
    @registry_ref.value
  end

  # Returns spec registered for symbol/method key, or nil.
  def self.get_spec(key)
    registry[Identifier(key)]
  end

  # Takes a single predicate. A predicate can be one of:
  # - Proc, e.g. `-> (x) { x.even? }`, will be called with the given value
  # - Method, e.g. `Foo.method(:bar?)`, will be called with the given value
  # - Set, e.g. `Set[1, 2]`, will be tested whether it includes the given value
  # - Class/Module, e.g. `String`, will be tested for case equality (is_a?) with the
  #   given value
  #
  # Note that it is not generally necessary to wrap predicates in spec when
  # using `S.def` etc., only to attach a unique generator
  #
  # Can also be passed the result of one of the regex ops -
  # cat, alt, zero_or_more, one_or_more, zero_or_one, in which case it will
  # return a regex-conforming spec, useful when nesting an independent regex.
  # ---

  # Optionally takes :gen generator function, which must be a proc of one arg
  # (Rantly instance) that generates a valid value.

  # Returns a spec.
  def self.spec(pred, opts = {})
    spec_impl(pred, false, opts[:gen]) if pred
  end

  # Creates and returns a hash validating spec. :req and :opt are both arrays of
  # namespaced-qualified keywords (e.g. ":MyApp/foo"). The validator will ensure
  # the :req keys are present. The :opt keys serve as documentation and may be
  # used by the generator.
  #
  # The :req key array supports 'and_keys' and 'or_keys' for key groups:
  #
  # S.keys(req: [:x.ns, :y.ns, S.or_keys(:secret.ns,
  #                                      S.and_keys(:user.ns, :pwd.ns))],
  #        opt: [:z.ns])
  #
  # There are also _un versions of :req and :opt. These allow you to connect
  # unqualified keys to specs. In each case, fully qualfied keywords are passed,
  # which name the specs, but unqualified keys (with the same name component)
  # are expected and checked at conform-time, and generated during gen:
  #
  # S.keys(req_un: [:"MyApp/x", :"MyApp/y"])
  #
  # The above says keys :x and :y are required, and will be validated and
  # generated by specs (if they exist) named :"MyApp/x" :"MyApp/y" respectively.
  #
  # In addition, the values of *all* namespace-qualified keys will be validated
  # (and possibly destructured) by any registered specs. Note: there is
  # no support for inline value specification, by design.
  #
  # Optionally takes :gen generator function, which must be a proc of one arg
  # (Rantly instance) that generates a valid value.
  def self.keys(req: [], opt: [], req_un: [], opt_un: [])
    HashSpec.new(req, opt, req_un, opt_un, nil)
  end

  # See Speculation.keys
  def self.or_keys(*ks)
    [:or.ns, *ks]
  end

  # See Speculation.keys
  def self.and_keys(*ks)
    [:and.ns, *ks]
  end

  # Takes key+pred pairs, e.g.
  #
  # S.or(even: -> (n) { n.even? }, small: -> (n) { n < 42 })
  #
  # Returns a destructuring spec that returns a two element array containing the
  # key of the first matching pred and the corresponding value. Thus the 'key'
  # and 'val' functions can be used to refer generically to the components of
  # the tagged return.
  def self.or(key_preds)
    # FIXME nested `or`s don't tag nested conforms
    OrSpec.new(key_preds)
  end

  # Takes predicate/spec-forms, e.g.
  #
  # S.and(Numeric, -> (n) { n < 42 })
  #
  # Returns a spec that returns the conformed value. Successive conformed values
  # propagate through rest of predicates.
  def self.and(*preds)
    AndSpec.new(preds)
  end

  # Takes hash-validating specs (e.g. 'keys' specs) and returns a spec that
  # returns a conformed hash satisfying all of the specs. Unlike 'and', merge
  # can generate maps satisfying the union of the predicates.
  def self.merge(*preds)
    MergeSpec.new(preds, nil)
  end

  # Takes a pred and validates collection elements against that pred.
  #
  # Note that 'every' does not do exhaustive checking, rather it samples
  # `coll_check_limit` elements. Nor (as a result) does it do any conforming of
  # elements. 'explain' will report at most coll_error_limit problems. Thus
  # 'every' should be suitable for potentially large collections.
  #
  # Takes several kwargs options that further constrain the collection:
  #
  # :kind - a pred/spec that the collection type must satisfy, e.g. `Array`
  #   (default nil) Note that if :kind is specified and :into is
  #   not, this pred must generate in order for every to generate.
  # :count - specifies coll has exactly this count (default nil)
  # :min_count, :max_count - coll has count `between?` min_count and max_count
  #   (defaults nil)
  # :distinct - all the elements are distinct (default nil)
  #
  # And additional args that control gen
  #
  # :gen_max - the maximum coll size to generate (default 20)
  # :into - one of [], {}, Set[] - the default collection to generate into
  #   (default: empty coll as generated by :kind pred if supplied, else [])
  #
  # Optionally takes :gen generator proc, which must be a proc of one arg
  # (Rantly instance) that generates a valid value.
  #
  # See also - coll_of, every_kv
  def self.every(predicate, options = {})
    gen = options.delete(:gen)
    EverySpec.new(predicate, options, gen)
  end

  # Like 'every' but takes separate key and val preds and works on associative
  # collections.
  #
  # Same options as 'every', :into defaults to {}
  #
  # See also - hash_of
  def self.every_kv(kpred, vpred, options)
    every(tuple(kpred, vpred), :kfn.ns => -> (i, v) { v.first },
                               into: {},
                               **options)
  end

  # Returns a spec for a collection of items satisfying pred. Unlike 'every',
  # coll_of will exhaustively conform every value.
  #
  # Same options as 'every'. conform will produce a collection corresponding to
  # :into if supplied, else will match the input collection, avoiding rebuilding
  # when possible.
  #
  # See also - every, hash_of
  def self.coll_of(spec, opts = {})
    every(spec, :conform_all.ns => true, **opts)
  end

  # Returns a spec for a hash whose keys satisfy kpred and vals satisfy vpred.
  # Unlike 'every_kv', hash_of will exhaustively conform every value.
  #
  # Same options as 'every', :kind defaults to `Speculation::Utils.hash?`, with
  # the addition of:
  #
  # :conform_keys - conform keys as well as values (default false)
  #
  # See also - every_kv
  def self.hash_of(kpred, vpred, options = {})
    every_kv(kpred, vpred, kind: Utils.method(:hash?).to_proc,
                           :conform_all.ns => true,
                           **options)
  end

  # Returns a regex op that matches zero or more values matching pred. Produces
  # an array of matches iff there is at least one match
  def self.zero_or_more(pred)
    rep(pred, pred, [], false)
  end

  # Returns a regex op that matches one or more values matching pred. Produces
  # an array of matches
  def self.one_or_more(pred)
    pcat(H[predicates: [pred, rep(pred, pred, [], true)], return_value: []])
  end

  # Returns a regex op that matches zero or one value matching pred. Produces a
  # single value (not a collection) if matched.
  def self.zero_or_one(pred)
    _alt([pred, accept(:nil.ns)], nil)
  end

  # Takes key+pred pairs, e.g.
  #
  # S.alt(even: :even?.to_proc, small: -> (n) { n < 42 })
  #
  # Returns a regex op that returns a two item array containing the key of the
  # first matching pred and the corresponding value. Thus can be destructured
  # to refer generically to the components of the return.
  def self.alt(kv_specs)
    _alt(kv_specs.values, kv_specs.keys).merge(id: SecureRandom.uuid)
  end

  # Takes key+pred pairs, e.g.
  #
  # S.cat(e: :even?.to_proc, o: :odd?.to_proc)
  #
  # Returns a regex op that matches (all) values in sequence, returning a map
  # containing the keys of each pred and the corresponding value.
  def self.cat(named_specs)
    keys = named_specs.keys
    predicates = named_specs.values

    pcat(H[keys: keys, predicates: predicates, return_value: {}])
  end

  # Takes a regex op re, and predicates. Returns a regex-op that consumes input
  # as per re but subjects the resulting value to the conjunction of the
  # predicates, and any conforming they might perform.
  def self.constrained(re, *preds)
    H[:op.ns => :amp.ns, p1: re, predicates: preds]
  end

  # Takes a predicate function with the semantics of conform i.e. it should
  # return either a (possibly converted) value or :"Speculation/invalid", and
  # returns a spec that uses it as a predicate/conformer.
  #
  # TODO Optionally takes a second fn that does unform of result of first
  def self.conformer(f)
    spec_impl(f, true, nil)
  end

  # Takes :args :ret and (optional) :fn kwargs whose values are preds and
  # returns a spec whose conform/explain take a method/proc and validates it
  # using generative testing. The conformed value is always the method itself.

  # See 'fdef' for a single operation that creates an fspec and registers it, as
  # well as a full description of :args, :ret and :fn

  # fspecs can generate procs that validate the arguments and fabricate a return
  # value compliant with the :ret spec, ignoring the :fn spec if present.

  # Optionally takes :gen generator proc, which must be a proc of one arg
  # (Rantly instance) that generates a valid value.
  def self.fspec(args: nil, ret: nil, fn: nil, gen: nil) # TODO :gen
    FnSpec.new(argspec: spec(args), retspec: spec(ret), fnspec: spec(fn), gen: gen)
  end

  # Takes :args :ret and (optional) :fn kwargs whose values are preds and
  # returns a spec whose conform/explain take a method and validates it using
  # generative testing. The conformed value is always the method itself.
  #
  # See 'fdef' for a single operation that creates an fspec and registers it, as
  # well as a full description of :args, :ret and :fn
  #
  # fspecs can generate functions that validate the arguments and fabricate a
  # return value compliant with the :ret spec, ignoring the :fn spec if present.
  #
  # Optionally takes :gen generator proc, which must be a proc of one arg
  # (Rantly instance) that generates a valid value.
  def self.fspec(args: nil, ret: nil, fn: nil, gen: nil)
    FnSpec.new(argspec: spec(args), retspec: spec(ret), fnspec: spec(fn), gen: gen)
  end

  # Takes one or more preds and returns a spec for a tuple, an array where each
  # element conforms to the corresponding pred. Each element will be referred to
  # in paths using its ordinal.
  def self.tuple(*preds)
    TupleSpec.new(preds)
  end

  # Takes a method object, and one or more of the following:
  #
  # :args A regex spec for the function arguments as a list
  # :ret A spec for the function's return value
  # :fn A spec of the relationship between args and ret - the value passed is
  #   { args: conformed_args ret: conformed_ret } and is expected to contain
  #   predicates that relate those values
  #
  # Once registered, checked by instrument and tested by the runner
  # Speculation::Test.check
  #
  # Note that :fn specs require the presence of :args and :ret specs to
  # conform values, and so :fn specs will be ignored if :args or :ret
  # are missing.
  #
  # Returns the Speculation::Identifier object representing the method which is
  # used as the spec's key in the spec registry.
  #
  # For example, to register method specs for the Hash[] method:
  #
  # S.fdef(Hash.method(:[]),
  #   args: S.alt(
  #     hash: Hash,
  #     array_of_pairs: S.coll_of(S.tuple(:any.ns(S), :any.ns(S)), kind: Array),
  #     kvs: S.constrained(S.one_or_more(:any.ns(S)), -> (kvs) { kvs.count.even? })
  #   ),
  #   ret: Hash
  # )
  def self.fdef(method, spec)
    ident = Identifier(method)
    self.def(ident, fspec(spec))
  end

  ### impl ###

  # @private
  def self.recur_limit?(rmap, id, path, k)
    rmap[id] > rmap[:recursion_limit.ns] &&
      path.include?(k)
  end

  # @private
  def self.inck(h, k)
    h.merge(k => h.fetch(k, 0).next)
  end

  # @private
  def self.dt(pred, x)
    return x unless pred

    spec = the_spec(pred)

    if spec
      conform(spec, x)
    else
      if pred.is_a?(Module) || pred.is_a?(Proc) || pred.is_a?(::Regexp)
        pred === x ? x : :invalid.ns
      elsif pred.is_a?(Set)
        pred.include?(x) ? x : :invalid.ns
      else
        raise "#{pred} is not a class, proc, set or regexp"
      end
    end
  end

  # Helper function that returns true when x is valid for spec.
  def self.valid?(spec, x)
    spec = _specize(spec)

    !invalid?(spec.conform(x))
  end

  # internal helper function that returns true when x is valid for spec.
  # @private
  def self.pvalid?(pred, x)
    !invalid?(dt(pred, x))
  end

  # @private
  def self.explain1(pred, path, via, _in, value)
    spec = maybe_spec(pred)

    if spec?(spec)
      name = spec_name(spec)
      via = via.conj(name) if name

      spec.explain(path, via, _in, value)
    else
      [{ path: path, val: value, via: via, in: _in, pred: pred }]
    end
  end

  class HashSpec
    attr_accessor :name, :id

    def initialize(req, opt, req_un, opt_un, gen = nil)
      @id = SecureRandom.uuid
      @req = req
      @opt = opt
      @req_un = req_un
      @opt_un = opt_un
      @gen = gen

      req_keys     = req.flat_map(&method(:extract_keys))
      req_un_specs = req_un.flat_map(&method(:extract_keys))

      unless (req_keys + req_un_specs + opt + opt_un).all? { |s| s.is_a?(Symbol) && s.namespace }
        raise "all keys must be namespaced Symbols"
      end

      req_specs = req_keys + req_un_specs
      req_keys  = req_keys + req_un_specs.map(&method(:unqualify_key))

      pred_exprs = [Utils.method(:hash?)]
      pred_exprs.push(-> (v) { parse_req(req, v, :itself.to_proc) }) if req.any?
      pred_exprs.push(-> (v) { parse_req(req_un, v, method(:unqualify_key)) }) if req_un.any?

      @req_keys        = req_keys
      @req_specs       = req_specs
      @opt_keys        = opt + opt_un.map(&method(:unqualify_key))
      @opt_specs       = opt + opt_un
      @keys_pred       = -> (v) { pred_exprs.all? { |p| p.call(v) } }
      @key_to_spec_map = H[req_keys.concat(@opt_keys).zip(req_specs.concat(@opt_specs))]
    end

    def conform(value)
      return :invalid.ns unless @keys_pred.call(value)

      reg = S.registry
      ret = value

      value.each do |key, v|
        spec_name = @key_to_spec_map.fetch(key, key)
        spec = reg[spec_name]

        next unless spec

        conformed_value = S.conform(spec, v)

        if S.invalid?(conformed_value)
          return :invalid.ns
        else
          unless conformed_value.equal?(v)
            ret = ret.merge(key => conformed_value)
          end
        end
      end

      ret
    end

    def explain(path, via, _in, value)
      unless Utils.hash?(value)
        return [{ path: path, pred: :hash?, val: value, via: via, in: _in }]
      end

      problems = []

      if @req.any?
        valid_or_failure = parse_req2(@req, value, :itself.to_proc)

        unless valid_or_failure == true
          valid_or_failure.each do |failure_sexp|
            pred = sexp_to_rb(failure_sexp)
            problems << { path: path, pred: pred, val: value, via: via, in: _in }
          end
        end
      end

      if @req_un.any?
        valid_or_failure = parse_req2(@req_un, value, method(:unqualify_key))

        unless valid_or_failure == true
          valid_or_failure.each do |failure_sexp|
            pred = sexp_to_rb(failure_sexp)
            problems << { path: path, pred: pred, val: value, via: via, in: _in }
          end
        end
      end

      problems += value.flat_map do |(k, v)|
        next unless S.registry.key?(@key_to_spec_map[k])

        unless S.pvalid?(@key_to_spec_map.fetch(k), v)
          S.explain1(@key_to_spec_map.fetch(k), path.conj(k), via, _in.conj(k), v)
        end
      end

      problems.compact
    end

    def specize
      self
    end

    # TODO overrrides
    def gen(overrides, path, rmap)
      return @gen if @gen

      rmap = S.inck(rmap, @id)

      reqs = @req_keys.zip(@req_specs).
        reduce({}) { |m, (k, s)|
          m.merge(k => S.gensub(s, overrides, path.conj(k), rmap))
        }

      opts = @opt_keys.zip(@opt_specs).
        reduce({}) { |m, (k, s)|
          m.merge(k => S.gensub(s, overrides, path.conj(k), rmap))
        }

      -> (rantly) do
        count = rantly.range(0, opts.count)
        opts = opts.to_a.shuffle.take(count).to_h

        reqs.merge(opts).each_with_object({}) { |(k, spec_gen), h|
          h[k] = spec_gen.call(rantly)
        }
      end
    end

    def with_gen(gen)
      self.class.new(@req, @opt, @req_un, @opt_un, gen)
    end

    def inspect
      "#{self.class.to_s}(#{@name})"
    end

    Protocol.Satisfy!(self, :Specize.ns, :Spec.ns)

    private

    def sexp_to_rb(sexp, level = 0)
      if sexp.is_a?(Array)
        op, *keys = sexp
        rb_string = ""

        rb_string << "(" unless level == 0

        keys.each_with_index do |key, i|
          unless i == 0
            rb_string << " #{op.name} "
          end

          rb_string << sexp_to_rb(key, level + 1)
        end

        rb_string << ")" unless level == 0

        rb_string
      else
        ":#{sexp}"
      end
    end

    def extract_keys(symbol_or_arr)
      if symbol_or_arr.is_a?(Array)
        symbol_or_arr[1..-1].flat_map(&method(:extract_keys))
      else
        symbol_or_arr
      end
    end

    def unqualify_key(x)
      x.name.to_sym
    end

    def parse_req2(ks, v, f)
      k, *ks = ks

      ret = if k.is_a?(Array)
              op, *kks = k
              case op
              when :or.ns
                if kks.one? { |k| parse_req([k], v, f) == true }
                  true
                else
                  [k]
                end
              when :and.ns
                if kks.all? { |k| parse_req([k], v, f) == true }
                  true
                else
                  [k]
                end
              else
                raise "Expected or, and, got #{op}"
              end
            else
              if v.key?(f.call(k))
                true
              else
                [k]
              end
            end

      if ks.any?
        if ret === true
          parse_req2(ks, v, f)
        else
          ret + parse_req2(ks, v, f)
        end
      else
        ret
      end
    end

    def parse_req(ks, v, f)
      k, *ks = ks

      ret = if k.is_a?(Array)
              op, *kks = k
              case op
              when :or.ns  then kks.one? { |k| parse_req([k], v, f) }
              when :and.ns then kks.all? { |k| parse_req([k], v, f) }
              else         raise "Expected or, and, got #{op}"
              end
            else
              v.key?(f.call(k))
            end

      if ks.any?
        ret && parse_req(ks, v, f)
      else
        ret
      end
    end
  end

  # @private
  def self.spec_impl(pred, should_conform, gen)
    if spec?(pred)
      pred
    elsif regex?(pred)
      RegexSpec.new(pred, gen)
    elsif Utils.ident?(pred)
      the_spec(pred)
    else
      Spec.new(pred, should_conform, gen)
    end
  end

  class Spec
    attr_accessor :name

    def initialize(predicate, should_conform, gen = nil)
      @predicate = predicate
      @should_conform = should_conform
      @gen = gen
    end

    def conform(value)
      ret = case @predicate
            when Set            then @predicate.include?(value)
            when Regexp, Module then @predicate === value
            else                     @predicate.call(value)
            end

      if @should_conform
        ret
      else
        ret ? value : :invalid.ns
      end
    end

    def explain(path, via, _in, value)
      if S.invalid?(S.dt(@predicate, value))
        [{ path: path, val: value, via: via, in: _in, pred: @predicate }]
      end
    end

    def gen(_, _, _)
      if @gen
        @gen
      else
        Gen.gen_for_pred(@predicate)
      end
    end

    def with_gen(gen)
      self.class.new(@predicate, @should_conform, gen)
    end

    def specize
      self
    end

    def inspect
      "#{self.class.to_s}(#{@name || @predicate.inspect})"
    end

    Protocol.Satisfy!(self, :Specize.ns, :Spec.ns)
  end

  class TupleSpec
    attr_accessor :name

    def initialize(preds, gen = nil)
      @preds = preds
      @gen   = gen

      @delayed_specs = Concurrent::Delay.new do
        preds.map { |pred| S._specize(pred) }
      end
    end

    def conform(collection)
      specs = @delayed_specs.value

      unless Utils.array?(collection) && collection.count == specs.count
        return :invalid.ns
      end

      return_value = collection.class.new

      collection.zip(specs).each do |(value, spec)|
        conformed_value = spec.conform(value)

        if S.invalid?(conformed_value)
          return :invalid.ns
        else
          return_value += [conformed_value]
        end
      end

      return_value
    end

    def explain(path, via, _in, value)
      if !Utils.array?(value)
        [{ path: path, val: value, via: via, in: _in, pred: "array?" }]
      elsif @preds.count != value.count
        [{ path: path, val: value, via: via, in: _in, pred: "count == predicates.count" }]
      else
        probs = @preds.zip(value).each_with_index.flat_map do |(pred, x), index|
          unless S.pvalid?(pred, x)
            S.explain1(pred, path.conj(index), via, _in.conj(index), x)
          end
        end

        probs.compact
      end
    end

    def gen(overrides, path, rmap)
      return @gen if @gen

      gens = @preds.each_with_index.
        map { |p, i| S.gensub(p, overrides, path.conj(i), rmap) }

      -> (rantly) do
        gens.map { |g| g.call(rantly) }
      end
    end

    def with_gen(gen)
      self.class.new(@preds, gen)
    end

    def specize
      self
    end

    def inspect
      "#{self.class.to_s}(#{@name})"
    end

    Protocol.Satisfy!(self, :Specize.ns, :Spec.ns)
  end

  class OrSpec
    attr_accessor :name, :id

    def initialize(named_specs, gen = nil)
      @id = SecureRandom.uuid
      @named_specs = named_specs
      @keys = named_specs.keys
      @preds = preds = named_specs.values
      @gen = gen

      @delayed_specs = Concurrent::Delay.new do
        preds.map { |spec| S._specize(spec) }
      end
    end

    def conform(value)
      @delayed_specs.value.each_with_index do |spec, index|
        conformed = spec.conform(value)

        unless S.invalid?(conformed)
          return [@keys[index], value]
        end
      end

      :invalid.ns
    end

    def specize
      self
    end

    def explain(path, via, _in, value)
      return unless !S.pvalid?(self, value)

      @keys.zip(@preds).flat_map do |(key, pred)|
        next if S.pvalid?(pred, value)
        S.explain1(pred, path.conj(key), via, _in, value)
      end
    end

    def gen(overrides, path, rmap)
      return gen if @gen

      gs = @keys.zip(@preds).
        map { |(k, p)|
          rmap = S.inck(rmap, @id)

          unless S.recur_limit?(rmap, @id, path, k)
            S.gensub(p, overrides, path.conj(k), rmap)
          end
        }.
        compact

      unless gs.empty?
        -> (rantly) { rantly.branch(*gs) }
      end
    end

    def with_gen(gen)
      self.class.new(@named_specs, gen)
    end

    def inspect
      "#{self.class.to_s}(#{@name})"
    end

    Protocol.Satisfy!(self, :Specize.ns, :Spec.ns)
  end

  private_class_method def self.and_preds(x, preds)
    preds.each do |pred|
      x = dt(pred, x)

      return :invalid.ns if invalid?(x)
    end

    x
  end

  # @private
  def self.explain_pred_list(preds, path, via, _in, value)
    return_value = value

    preds.each do |pred|
      nret = dt(pred, return_value)

      if invalid?(nret)
        return explain1(pred, path, via, _in, return_value)
      else
        return_value = nret
      end
    end

    nil
  end

  class AndSpec
    attr_accessor :name

    def initialize(preds, gen = nil)
      @preds = preds
      @gen = gen
      @specs = Concurrent::Delay.new do
        preds.map { |pred| S._specize(pred) }
      end
    end

    def conform(value)
      @specs.value.each do |spec|
        value = spec.conform(value)

        return :invalid.ns if S.invalid?(value)
      end

      value
    end

    def specize
      self
    end

    def explain(path, via, _in, value)
      S.explain_pred_list(@preds, path, via, _in, value)
    end

    def gen(overrides, path, rmap)
      if @gen
        @gen
      else
        S.gensub(@preds.first, overrides, path, rmap)
      end
    end

    def with_gen(gen)
      self.class.new(@preds, gen)
    end

    def inspect
      "#{self.class.to_s}(#{@name})"
    end

    Protocol.Satisfy!(self, :Specize.ns, :Spec.ns)
  end

  class MergeSpec
    attr_accessor :name

    def initialize(preds, gen = nil)
      @preds = preds
      @gen = gen
    end

    def conform(x)
      ms = @preds.map { |pred| S.dt(pred, x) }

      if ms.any?(&S.method(:invalid?))
        :invalid.ns(S)
      else
        ms.reduce(&:merge)
      end
    end

    def explain(path, via, _in, x)
      @preds.
        flat_map { |pred| S.explain1(pred, path, via, _in, x) }.
        compact
    end

    def gen(overrides, path, rmap)
      return @gen if @gen

      gens = @preds.
        map { |pred| S.gensub(pred, overrides, path, rmap) }

      -> (r) do
        gens.map { |gen| gen.call(r) }.reduce(&:merge)
      end
    end

    def with_gen(gen)
      self.class.new(@preds, gen)
    end

    def specize
      self
    end

    Protocol.Satisfy!(self, :Specize.ns, :Spec.ns)
  end

  # TODO move inside EverySpec?
  # @private
  def self.collection_problems(x, kfn, distinct, count, min_count, max_count, path, via, _in)
    pred = kfn || Utils.method(:collection?)

    if !pvalid?(pred, x)
      return explain1(pred, path, via, _in, x)
    end

    if count && count != x.count
      return [{ path: path, pred: 'count == x.count', val: x, via: via, in: _in }]
    end

    if min_count || max_count
      if x.count.between?(min_count || 0, max_count || Float::Infinity)
        return [{ path: path, pred: 'count.between?(min_count || 0, max_count || Float::Infinity)', val: x, via: via, in: _in }]
      end
    end

    if distinct && !x.empty? && Utils.distinct?(x)
      [{ path: path, pred: 'distinct?', val: x, via: via, in: _in }]
    end
  end

  class EverySpec
    attr_accessor :name

    def initialize(predicate, options, gen)
      @predicate = predicate
      @options = options
      @gen = gen

      collection_predicates = [options.fetch(:kind, Enumerable)]

      if options.key?(:count)
        collection_predicates.push(-> (coll) { coll.count == options[:count] })
      elsif options.key?(:min_count) || options.key?(:max_count)
        collection_predicates.push(-> (coll) do
          min = options.fetch(:min_count, 0)
          max = options.fetch(:max_count, Float::INFINITY)

          coll.count.between?(min, max)
        end)
      end

      @collection_predicate = -> (coll) { collection_predicates.all? { |f| f === coll } }
      @delayed_spec = Concurrent::Delay.new { S._specize(predicate) }
      @kfn = options.fetch(:kfn.ns, -> (i, v) { i })
      @conform_keys, @conform_all, @kind, @gen_into, @gen_max, @distinct, @count, @min_count, @max_count =
        options.values_at(:conform_keys, :conform_all.ns, :kind, :into, :gen_max, :distinct, :count, :min_count, :max_count)
      @gen_max ||= 20
      @conform_into = @gen_into

      # returns a tuple of [init add complete] fns
      @cfns = -> (x) do
        if Utils.array?(x) && (!@conform_into || Utils.array?(@conform_into))
          [:itself.to_proc,
           -> (ret, i, v, cv) { v.equal?(cv) ? ret : ret.tap { |r| r[i] = cv } },
           :itself.to_proc]
        elsif Utils.hash?(x) && ((@kind && !@conform_into) || Utils.hash?(@conform_into))
          [@conform_keys ? Utils.method(:empty) : :itself.to_proc,
           -> (ret, i, v, cv) {
            if v.equal?(cv) && !@conform_keys
              ret
            else
              ret.merge((@conform_keys ? cv : v).first => cv.last)
            end
           },
           :itself.to_proc]
        else
          [-> (x) { Utils.empty(@conform_into || x) },
           -> (ret, i, v, cv) { ret.conj(cv) },
           :itself.to_proc]
        end
      end
    end

    def conform(value)
      return :invalid.ns unless @collection_predicate.call(value)

      spec = @delayed_spec.value

      if @conform_all
        init, add, complete = @cfns.call(value)

        return_value = init.call(value)

        value.each_with_index do |value, index|
          conformed_value = spec.conform(value)

          if S.invalid?(conformed_value)
            return :invalid.ns
          else
            return_value = add.call(return_value, index, value, conformed_value)
          end
        end

        complete.call(return_value)
      else
        # OPTIMIZE: check if value is indexed (array, hash etc.) vs not
        # indexed (list, custom enumerable)
        limit = COLL_CHECK_LIMIT

        value.each_with_index do |item, index|
          return value if index == limit
          return :invalid.ns unless S.valid?(spec, item)
        end

        value
      end
    end

    def specize
      self
    end

    def explain(path, via, _in, value)
      probs = S.collection_problems(value, @kind, @distinct, @count, @min_count, @max_count, path, via, _in)
      return probs if probs

      spec = @delayed_spec.value

      probs = value.lazy.each_with_index.flat_map do |v, i|
        k = @kfn.call(i, v)

        unless S.valid?(spec, v)
          S.explain1(@predicate, path, via, _in.conj(k), v)
        end
      end

      probs = @conform_all ? probs.to_a : probs.take(COLL_ERROR_LIMIT)
      probs.compact
    end

    def gen(overrides, path, rmap)
      return @gen if @gen

      pgen = S.gensub(@predicate, overrides, path, rmap)

      -> (rantly) do
        init = if @gen_into
                 Utils.empty(@gen_into)
               elsif @kind
                 Utils.empty(S.gensub(@kind, overrides, path, rmap).call(rantly))
               else
                 []
               end

        val = if @distinct
                if @count
                  rantly.array(@count, &pgen).tap { |arr| rantly.guard(Utils.distinct?(arr)) }
                else
                  min = @min_count || 0
                  max = @max_count || [@gen_max, 2 * min].max
                  count = rantly.range(min, max)

                  rantly.array(count, &pgen).tap { |arr| rantly.guard(Utils.distinct?(arr)) }
                end
              elsif @count
                rantly.array(@count, &pgen)
              elsif @min_count || @max_count
                min = @min_count || 0
                max = @max_count || [@gen_max, 2 * min].max
                count = rantly.range(min, max)

                rantly.array(count, &pgen)
              else
                count = rantly.range(0, @gen_max)
                rantly.array(count, &pgen)
              end

        Utils.into(init, val)
      end
    end

    def with_gen(gen)
      self.class.new(@predicate, @options, gen)
    end

    def inspect
      "#{self.class.to_s}(#{@name})"
    end

    Protocol.Satisfy!(self, :Specize.ns, :Spec.ns)
  end

  ### regex

  private_class_method def self.accept(x)
    H[:op.ns => :accept.ns, return_value: x]
  end

  private_class_method def self.accept?(hash)
    if hash.is_a?(H)
      hash[:op.ns] == :accept.ns
    end
  end

  private_class_method def self.pcat(regex)
    predicate, *rest_predicates = regex[:predicates]

    keys = regex[:keys]
    key, *rest_keys = keys

    return unless regex[:predicates].all?

    unless accept?(predicate)
      return H[:op.ns => :pcat.ns,
               predicates: regex[:predicates],
               keys: keys,
               return_value: regex[:return_value]]
    end

    val = keys ? { key => predicate[:return_value] } : predicate[:return_value]
    return_value = regex[:return_value].conj(val)

    if rest_predicates
      pcat(H[predicates: rest_predicates,
             keys: rest_keys,
             return_value: return_value])
    else
      accept(return_value)
    end
  end

  private_class_method def self.rep(p1, p2, return_value, splice)
    return unless p1

    regex = H[:op.ns => :rep.ns, p2: p2, splice: splice, id: SecureRandom.uuid]

    if accept?(p1)
      regex.merge(p1: p2, return_value: return_value.conj(p1[:return_value]))
    else
      regex.merge(p1: p1, return_value: return_value)
    end
  end

  private_class_method def self.filter_alt(ps, ks, &block)
    if ks
      pks = ps.zip(ks).select { |xs| block.call(xs.first) }
      [pks.map(&:first), pks.map(&:last)]
    else
      [ps.select(&block), ks]
    end
  end

  # alt*
  private_class_method def self._alt(predicates, keys)
    predicates, keys = filter_alt(predicates, keys, &:itself)
    return unless predicates

    predicate, *rest_predicates = predicates
    key, *rest_keys = keys

    return_value = H[:op.ns => :alt.ns, predicates: predicates, keys: keys]
    return return_value unless rest_predicates.empty?

    return predicate unless key
    return return_value unless accept?(predicate)

    accept([key, predicate[:return_value]])
  end

  private_class_method def self.alt2(p1, p2)
    if p1 && p2
      _alt([p1, p2], nil)
    else
      p1 || p2
    end
  end

  private_class_method def self.no_ret?(p1, pret)
    return true if pret == :nil.ns

    regex = _reg_resolve!(p1)
    op = regex[:op.ns]

    [:rep.ns, :pcat.ns].include?(op) && pret.empty? || nil
  end

  private_class_method def self.accept_nil?(regex)
    regex = _reg_resolve!(regex)
    return unless regex?(regex)

    case regex[:op.ns]
    when :accept.ns then true
    when :pcat.ns   then regex[:predicates].all?(&method(:accept_nil?))
    when :alt.ns    then regex[:predicates].any?(&method(:accept_nil?))
    when :rep.ns    then (regex[:p1] == regex[:p2]) || accept_nil?(regex[:p1])
    when :amp.ns
      p1 = regex[:p1]

      return false unless accept_nil?(p1)

      no_ret?(p1, preturn(p1)) ||
        !invalid?(and_preds(preturn(p1), regex[:predicates]))
    else
      raise "Unexpected #{:op.ns} #{regex[:op.ns]}"
    end
  end

  private_class_method def self.preturn(regex)
    regex = _reg_resolve!(regex)
    return unless regex?(regex)

    p0, *pr = regex[:predicates]
    k, *ks = regex[:keys]

    case regex[:op.ns]
    when :accept.ns then regex[:return_value]
    when :pcat.ns   then add_ret(p0, regex[:return_value], k)
    when :rep.ns    then add_ret(regex[:p1], regex[:return_value], k)
    when :amp.ns
      pret = preturn(regex[:p1])

      if no_ret?(regex[:p1], pret)
        :nil.ns
      else
        and_preds(pret, regex[:predicates])
      end
    when :alt.ns
      ps, ks = filter_alt(regex[:predicates], regex[:keys], &method(:accept_nil?))

      r = if ps.first.nil?
            :nil.ns
          else
            preturn(ps.first)
          end

      if ks && ks.first
        [ks.first, r]
      else
        r
      end
    else
      raise "Unexpected #{:op.ns} #{regex[:op.ns]}"
    end
  end

  private_class_method def self.add_ret(regex, r, key)
    regex = _reg_resolve!(regex)
    return r unless regex?(regex)

    prop = -> do
      return_value = preturn(regex)

      if return_value.empty?
        r
      else
        val = key ? { key => return_value } : return_value

        regex[:splice] ? Utils.into(r, val) : r.conj(val)
      end
    end

    case regex[:op.ns]
    when :accept.ns, :alt.ns, :amp.ns
      return_value = preturn(regex)

      if return_value == :nil.ns
        r
      else
        r.conj(key ? { key => return_value } : return_value)
      end
    when :pcat.ns, :rep.ns then prop.call
    else
      raise "Unexpected #{:op.ns} #{regex[:op.ns]}"
    end
  end

  private_class_method def self.deriv(predicate, value)
    predicate = _reg_resolve!(predicate)
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

    case regex[:op.ns]
    when :accept.ns then nil
    when :pcat.ns
      regex1 = pcat(H[predicates: [deriv(pred, value), *rest_preds], keys: keys, return_value: return_value])
      regex2 = nil

      if accept_nil?(pred)
        regex2 = deriv(
          pcat(H[predicates: rest_preds, keys: rest_keys, return_value: add_ret(pred, return_value, key)]),
          value
        )
      end

      alt2(regex1, regex2)
    when :alt.ns
      _alt(predicates.map { |p| deriv(p, value) }, keys)
    when :rep.ns
      regex1 = rep(deriv(p1, value), p2, return_value, splice)
      regex2 = nil

      if accept_nil?(p1)
        regex2 = deriv(rep(p2, p2, add_ret(p1, return_value, nil), splice), value)
      end

      alt2(regex1, regex2)
    when :amp.ns
      p1 = deriv(p1, value)
      return unless p1

      if p1[:op.ns] == :accept.ns
        ret = and_preds(preturn(p1), predicates)
        accept(ret) unless invalid?(ret)
      else
        constrained(p1, *predicates)
      end
    else
      raise "Unexpected #{:op.ns} #{regex[:op.ns]}"
    end
  end

  private_class_method def self.op_explain(p, path, via, _in, input)
    p = _reg_resolve!(p)
    return unless p

    insufficient = -> (path) do
      [{ path: path,
         reason: "Insufficient input",
         val: [],
         via: via,
         in: _in }]
    end

    input ||= []
    x = input.first

    unless regex?(p)
      if input.empty?
        return insufficient.call(path)
      else
        return explain1(p, path, via, _in, x)
      end
    end

    case p[:op.ns]
    when :accept.ns then nil
    when :amp.ns
      if input.empty?
        if accept_nil?(p[:p1])
          explain_pred_list(p[:predicates], path, via, _in, preturn(p[:p1]))
        else
          insufficient.call(path)
        end
      else
        p1 = deriv(p[:p1], x)

        if p1
          explain_pred_list(p[:predicates], path, via, _in, preturn(p1))
        else
          op_explain(p[:p1], path, via, _in, input)
        end
      end
    when :pcat.ns
      pks = p[:predicates].zip(p[:keys] || [])
      pred, k = if pks.count == 1
                  pks.first
                else
                  pks.lazy.reject { |(p, _)| accept_nil?(p) }.first
                end
      path = path.conj(k) if k

      if input.empty? && !pred
        insufficient.call(path)
      else
        op_explain(pred, path, via, _in, input)
      end
    when :alt.ns
      return insufficient.call(path) if input.empty?

      probs = p[:predicates].zip(p[:keys]).flat_map do |(p, k)|
        op_explain(p, k ? path.conj(k) : path, via, _in, input)
      end

      probs.compact
    when :rep.ns
      op_explain(p[:p1], path, via, _in, input)
    else
      raise "Unexpected #{:op.ns} #{p[:op.ns]}"
    end
  end

  # @private
  def self.re_gen(p, overrides, path, rmap)
    origp = p
    p = _reg_resolve!(p)

    id, op, ps, ks, p1, p2, splice, ret, id, gen = p.values_at(
      :id, :op.ns, :predicates, :keys, :p1, :p2, :splice, :return_value, :id, :gen.ns
    ) if regex?(p)

    ks ||= []

    id = p.id if p.respond_to?(:id)
    rmap = inck(rmap, id) if id

    ggens = -> (ps, ks) do
      ps.zip(ks).map do |p, k|
        unless rmap && id && k && recur_limit?(rmap, id, path, k)
          # TODO delay if we have an id?
          re_gen(p, overrides, k ? path.conj(k) : path, rmap)
        end
      end
    end

    ogen = overrides[spec_name(origp)] ||
      overrides[spec_name(p)] ||
      overrides[path]

    if ogen
      if [:accept, nil].include?(op)
        return -> (rantly) { [*ogen.call(rantly)] }
      else
        return -> (rantly) { ogen.call(rantly) }
      end
    end

    return gen if gen

    if p
      case op
      when :accept.ns
        if ret == :nil.ns
          -> (rantly) { [] }
        else
          -> (rantly) { [ret] }
        end
      when nil
        g = gensub(p, overrides, path, rmap)

        -> (rantly) { [g.call(rantly)] }
      when :amp.ns
        re_gen(p1, overrides, path, rmap)
      when :pcat.ns
        gens = ggens.call(ps, ks)

        if gens.all?
          -> (rantly) do
            gens.flat_map { |g| g.call(rantly) }
          end
        end
      when :alt.ns
        gens = ggens.call(ps, ks).compact

        -> (rantly) { rantly.branch(*gens) } unless gens.empty?
      when :rep.ns
        if recur_limit?(rmap, id, [id], id)
          -> (rantly) { [] }
        else
          g = re_gen(p2, overrides, path, rmap)

          if g
            # TODO wrap in shrinkable?
            -> (rantly) do
              # TODO how big?
              rantly.range(0, 30).times.flat_map { g.call(rantly) }
            end
          end
        end
      end
    end
  end

  # @private
  def self.re_conform(regex, data)
    x, *xs = data

    if data.empty?
      return :invalid.ns unless accept_nil?(regex)

      return_value = preturn(regex)

      if return_value == :nil.ns
        nil
      else
        return_value
      end
    else
      dp = deriv(regex, x)

      if dp
        re_conform(dp, xs)
      else
        :invalid.ns
      end
    end
  end

  # @private
  def self.re_explain(path, via, _in, regex, input)
    p = regex

    input.each_with_index do |value, index|
      dp = deriv(p, value)

      if dp
        p = dp
        next
      end

      if accept?(p)
        if p[:op.ns] == :pcat.ns
          return op_explain(p, path, via, _in.conj(index), input[index..-1])
        else
          return [{ path: path,
                    reason: "Extra input",
                    val: input,
                    via: via,
                    in: _in.conj(index) }]
        end
      else
        return op_explain(p, path, via, _in.conj(index), input[index..-1]) ||
          [{ path: path,
             reason: "Extra input",
             val: input,
             via: via,
             in: _in.conj(index) }]
      end
    end

    if accept_nil?(p)
      nil # success
    else
      op_explain(p, path, via, _in, nil)
    end
  end

  class RegexSpec
    attr_accessor :name

    def initialize(regex, gen = nil)
      @regex = regex
      @gen = gen
    end

    def conform(value)
      if value.nil? || Utils.collection?(value)
        S.re_conform(@regex, value)
      else
        :invalid.ns
      end
    end

    def explain(path, via, _in, value)
      if value.nil? || Utils.collection?(value)
        S.re_explain(path, via, _in, @regex, value || [])
      else
        [{ path: path, val: value, via: via, in: _in }]
      end
    end

    def gen(overrides, path, rmap)
      return @gen if @gen

      S.re_gen(@regex, overrides, path, rmap)
    end

    def specize
      self
    end

    def with_gen(gen)
      self.class.new(@regex, gen)
    end

    def inspect
      "#{self.class.to_s}(#{@name})"
    end

    Protocol.Satisfy!(self, :Specize.ns, :Spec.ns)
  end

  ## HOFs

  # TODO call_valid?
  # TODO validate_fn

  class FnSpec
    attr_reader :argspec, :retspec, :fnspec
    attr_accessor :name

    def initialize(argspec: nil, retspec: nil, fnspec: nil, gen: nil)
      @argspec = argspec
      @retspec = retspec
      @fnspec = fnspec
      @gen = gen
    end

    def conform(value)
      raise "Can't conform fspec without args spec: #{self.inspect}" unless @argspec
      # TODO value.is_a?(Method) correct? maybe Identifier?
      return :invalid.ns unless value.is_a?(Proc) || value.is_a?(Method)

      # TODO: quick-check the function to determine validity
      #       returning fn here so that fn generation can happen
      #       (since it will can fspec.conform and check it's not
      #       :invalid
      value
    end

    def explain(path, via, _in, value)
      # TODO implement me
      raise NotImplementedError
    end

    def with_gen(gen)
      self.class.new(argspec: @argspec, retspec: @retspec, fnspec: @fnspec, gen: @gen)
    end

    def gen(overrides, path, rmap)
      return @gen if @gen

      -> (rantly) do
        -> (*args) do
          unless S.pvalid?(@argspec, args)
            raise S.explain(@argspec, args)
          end

          Gen.generate(S.gen(@retspec, overrides))
        end
      end
    end

    def specize
      self
    end

    def inspect
      "#{self.class.to_s}(#{@name})"
    end

    Protocol.Satisfy!(self, :Specize.ns, :Spec.ns)
  end

  class NilableSpec
    attr_accessor :name

    def initialize(pred, gen = nil)
      @pred = pred
      @gen  = gen
      @delayed_spec = Concurrent::Delay.new { S._specize(pred) }
    end

    def conform(value)
      value.nil? ? value : @delayed_spec.value.conform(value)
    end

    def explain(path, via, _in, value)
      return if S.pvalid?(@delayed_spec.value, value) || value.nil?

      S.
        explain1(@pred, path.conj(:pred.ns), via, _in, value).
        conj({ path: path.conj(:nil.ns), pred: NilClass, val: value, via: via, in: _in })
    end

    def gen(overrides, path, rmap)
      return @gen if @gen

      -> (rantly) do
        rantly.freq([1, Utils.constantly(nil)],
                    [9, S.gensub(@pred, overrides, path.conj(:pred.ns), rmap)])
      end
    end

    def with_gen(gen)
      self.class.new(@pred, gen)
    end

    def specize
      self
    end

    def inspect
      "#{self.class.to_s}(#{@name})"
    end

    Protocol.Satisfy!(self, :Specize.ns, :Spec.ns)
  end

  # returns a spec that accepts nil and values satisfying pred
  def self.nilable(pred)
    NilableSpec.new(pred)
  end

  # Generates a number (default 10) of values compatible with spec and maps
  # conform over them, returning a sequence of [val conformed-val] tuples.
  # Optionally takes a generator overrides hash as per gen
  def self.exercise(spec, n: 10, overrides: {})
    Gen.sample(gen(spec, overrides), n).map { |value|
      [value, conform(spec, value)]
    }
  end

  # TODO exercise_fn

  # TODO date_in_range?
  # TODO date_in
  # TODO int_in_range?
  # TODO int_in
  # TODO float_in

  # TODO assert

  # Resets the spec registry to only builtin specs
  def self.reset_registry!
    builtins = H[
      :any.ns              => with_gen(Utils.constantly(true)) { |r| r.branch(*Gen::GEN_BUILTINS.values) },
      :boolean.ns          => Set[true, false],
      :positive_integer.ns => with_gen(self.and(Integer, :positive?.to_proc)) { |r| r.range(1) },
      # Rantly#positive_integer is actually a natural integer
      :natural_integer.ns  => with_gen(self.and(Integer, Utils.complement(&:negative?)), &:positive_integer),
      :negative_integer.ns => with_gen(self.and(Integer, :negative?.to_proc)) { |r| r.range(nil, -1) }
    ]

    @registry_ref.reset(builtins)
  end

  reset_registry!
end
