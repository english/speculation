# frozen_string_literal: true
require "concurrent/atom"
require "concurrent/delay"
require "hamster/hash"
require "set"
require "securerandom"

require "speculation/version"
require "speculation/namespaced_symbols"
require "speculation/conj"
require "speculation/identifier"
require "speculation/utils"
require "speculation/spec_impl"
require "speculation/error"

module Speculation
  H = Hamster::Hash

  using NamespacedSymbols.refine(self)
  using Conj

  class << self
    # A soft limit on how many times a branching spec
    # (or/alt/zero_or_more) can be recursed through during generation.
    # After this a non-recursive branch will be chosen.
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

  @recursion_limit  = 4
  @fspec_iterations = 21
  @coll_check_limit = 101
  @coll_error_limit = 20

  @registry_ref = Concurrent::Atom.new(H[])

  private_class_method def self.deep_resolve(reg, spec)
    spec = reg[spec] while Utils.ident?(spec)
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

  # returns the spec/regex at end of alias chain starting with k, throws if not
  # found, k if k not ident
  # private
  def self._reg_resolve!(key)
    return key unless Utils.ident?(key)
    spec = reg_resolve(key)

    if spec
      spec
    else
      raise "Unable to resolve spec: #{key}"
    end
  end

  # returns spec if spec is a spec object, else logical false, spec spec spec
  def self.spec?(spec)
    spec if spec.is_a?(SpecImpl)
  end

  # returns x if x is a (Speculation) regex op, else logical false
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

  # spec_or_key must be a spec, regex or resolvable ident, else returns nil
  private_class_method def self.maybe_spec(spec_or_key)
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

  # spec_or_key must be a spec, regex or ident, else returns nil. Raises if
  # unresolvable ident (Speculation::Utils.ident?)
  private_class_method def self.the_spec(spec_or_key)
    spec = maybe_spec(spec_or_key)
    return spec if spec

    if Utils.ident?(spec_or_key)
      raise "Unable to resolve spec: #{spec_or_key}"
    end
  end

  # private
  def self.specize(spec)
    if spec?(spec)
      spec
    else
      case spec
      when Symbol, Identifier
        specize(_reg_resolve!(spec))
      else
        spec_impl(spec, false)
      end
    end
  end

  # tests the validity of a conform return value
  def self.invalid?(value)
    value.equal?(:invalid.ns)
  end

  # Given a spec and a value, returns :Speculation/invalid if value does not
  # match spec, else the (possibly destructured) value
  def self.conform(spec, value)
    spec = Identifier(spec)
    specize(spec).conform(value)
  end

  # Takes a spec and a one-arg generator block and returns a version of the
  # spec that uses that generator
  def self.with_gen(spec, &gen)
    if regex?(spec)
      spec.put(:gfn.ns, gen)
    else
      specize(spec).tap { |s| s.gen = gen }
    end
  end

  # @private
  def self._explain_data(spec, path, via, inn, value)
    probs = specize(spec).explain(path, via, inn, value)

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
    spec = Identifier(spec)
    name = spec_name(spec)
    _explain_data(spec, [], [name] || [], [], x)
  end

  # returns explanation data (per 'explain_data') as a human readable string
  def self.explain_str(data)
    return "Success!" unless data

    s = String.new

    data.fetch(:problems.ns).each do |prob|
      path, pred, val, reason, via, inn = prob.values_at(:path, :pred, :val, :reason, :via, :in)

      s << "In: #{inn.to_a.inspect} " unless inn.empty?
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

  # Given a spec and a value that fails to conform, returns an explanation as a
  # string
  def self.explain(spec, x)
    explain_str(explain_data(spec, x))
  end

  # @private
  def self.gensub(spec, overrides, path, rmap)
    overrides ||= {}
    spec = specize(spec)
    gfn = overrides[spec.name || spec] || overrides[path]
    g = gfn || spec.gen(overrides, path, rmap)

    if g
      Gen.such_that(->(x) { valid?(spec, x) }, g)
    else
      raise Speculation::Error.new("unable to construct gen at: #{path.inspect} for: #{spec.inspect}",
                                   :failure.ns => :no_gen, :path.ns => path)
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
    spec = Identifier(spec)
    gensub(spec, overrides, [], H[:recursion_limit.ns => recursion_limit])
  end

  # rubocop:disable Style/MethodName
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
  # rubocop:enable Style/MethodName

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
             spec_impl(spec, false)
           end

    @registry_ref.swap do |reg|
      reg.store(key, with_name(spec, key))
    end

    key
  end

  # returns the registry hash, prefer 'get_spec' to lookup a spec by name
  def self.registry
    @registry_ref.value
  end

  # Returns spec registered for symbol/method key, or nil.
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
    if pred
      spec_impl(pred, false).tap do
        spec.gen = opts[:gen] if opts[:gen]
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
  def self.keys(req: [], opt: [], req_un: [], opt_un: [], gen: nil)
    HashSpec.new(req, opt, req_un, opt_un).tap do |spec|
      spec.gen = gen
    end
  end

  # See Speculation.keys
  def self.or_keys(*ks)
    [:or.ns, *ks]
  end

  # See Speculation.keys
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
    MergeSpec.new(preds)
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
  def self.every(predicate, opts = {})
    gen = opts.delete(:gen)

    EverySpec.new(predicate, opts).tap do |spec|
      spec.gen = gen
    end
  end

  # Like 'every' but takes separate key and val preds and works on associative
  # collections.
  #
  # Same options as 'every', :into defaults to {}
  #
  # See also - hash_of
  def self.every_kv(kpred, vpred, options)
    every(tuple(kpred, vpred), :kfn.ns => ->(_i, v) { v.first },
                               :into   => {},
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
    every_kv(kpred, vpred, :kind           => Utils.method(:hash?).to_proc,
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
    pcat(H[:predicates => [pred, rep(pred, pred, [], true)], :return_value => []])
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
    _alt(kv_specs.values, kv_specs.keys).merge(:id => SecureRandom.uuid)
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

    pcat(H[:keys => keys, :predicates => predicates, :return_value => {}])
  end

  # Takes a regex op re, and predicates. Returns a regex-op that consumes input
  # as per re but subjects the resulting value to the conjunction of the
  # predicates, and any conforming they might perform.
  def self.constrained(re, *preds)
    H[:op.ns => :amp.ns, :p1 => re, :predicates => preds]
  end

  # Takes a predicate function with the semantics of conform i.e. it should
  # return either a (possibly converted) value or :"Speculation/invalid", and
  # returns a spec that uses it as a predicate/conformer.
  def self.conformer(f)
    spec_impl(f, true)
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
  def self.fspec(args: nil, ret: nil, fn: nil, gen: nil)
    FSpec.new(:argspec => spec(args), :retspec => spec(ret), :fnspec => spec(fn)).tap do |spec|
      spec.gen = gen
    end
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

  # @private
  def self.recur_limit?(rmap, id, path, k)
    rmap[id] > rmap[:recursion_limit.ns] &&
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
      pred === x ? x : :invalid.ns
    elsif pred.is_a?(Set)
      pred.include?(x) ? x : :invalid.ns
    elsif pred.respond_to?(:call)
      pred.call(x) ? x : :invalid.ns
    else
      raise "#{pred} is not a class, proc, set or regexp"
    end
  end

  # Helper function that returns true when x is valid for spec.
  def self.valid?(spec, x)
    spec = Identifier(spec)
    spec = specize(spec)

    !invalid?(spec.conform(x))
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
      via = via.conj(name) if name

      spec.explain(path, via, inn, value)
    else
      [{ :path => path, :val => value, :via => via, :in => inn, :pred => pred }]
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
      Spec.new(pred, should_conform)
    end
  end

  private_class_method def self.and_preds(x, preds)
    preds.each do |pred|
      x = dt(pred, x)

      return :invalid.ns if invalid?(x)
    end

    x
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

  private_class_method def self.accept(x)
    H[:op.ns => :accept.ns, :return_value => x]
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
      return H[:op.ns        => :pcat.ns,
               :predicates   => regex[:predicates],
               :keys         => keys,
               :return_value => regex[:return_value]]
    end

    val = keys ? { key => predicate[:return_value] } : predicate[:return_value]
    return_value = regex[:return_value].conj(val)

    if rest_predicates
      pcat(H[:predicates   => rest_predicates,
             :keys         => rest_keys,
             :return_value => return_value])
    else
      accept(return_value)
    end
  end

  private_class_method def self.rep(p1, p2, return_value, splice)
    return unless p1

    regex = H[:op.ns => :rep.ns, :p2 => p2, :splice => splice, :id => SecureRandom.uuid]

    if accept?(p1)
      regex.merge(:p1 => p2, :return_value => return_value.conj(p1[:return_value]))
    else
      regex.merge(:p1 => p1, :return_value => return_value)
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

  # alt*
  private_class_method def self._alt(predicates, keys)
    predicates, keys = filter_alt(predicates, keys, &:itself)
    return unless predicates

    predicate, *rest_predicates = predicates
    key, *_rest_keys = keys

    return_value = H[:op.ns => :alt.ns, :predicates => predicates, :keys => keys]
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

    p0, *_pr = regex[:predicates]
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
      regex1 = pcat(H[:predicates => [deriv(pred, value), *rest_preds], :keys => keys, :return_value => return_value])
      regex2 = nil

      if accept_nil?(pred)
        regex2 = deriv(
          pcat(H[:predicates => rest_preds, :keys => rest_keys, :return_value => add_ret(pred, return_value, key)]),
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

  private_class_method def self.insufficient(path, via, inn)
    [{ :path   => path,
       :reason => "Insufficient input",
       :val    => [],
       :via    => via,
       :in     => inn }]
  end

  private_class_method def self.op_explain(p, path, via, inn, input)
    p = _reg_resolve!(p)
    return unless p

    input ||= []
    x = input.first

    unless regex?(p)
      if input.empty?
        return insufficient(path, via, inn)
      else
        return explain1(p, path, via, inn, x)
      end
    end

    case p[:op.ns]
    when :accept.ns then nil
    when :amp.ns
      if input.empty?
        if accept_nil?(p[:p1])
          explain_pred_list(p[:predicates], path, via, inn, preturn(p[:p1]))
        else
          insufficient(path, via, inn)
        end
      else
        p1 = deriv(p[:p1], x)

        if p1
          explain_pred_list(p[:predicates], path, via, inn, preturn(p1))
        else
          op_explain(p[:p1], path, via, inn, input)
        end
      end
    when :pcat.ns
      pks = p[:predicates].zip(p[:keys] || [])
      pred, k = if pks.count == 1
                  pks.first
                else
                  pks.lazy.reject { |(predicate, _)| accept_nil?(predicate) }.first
                end
      path = path.conj(k) if k

      if input.empty? && !pred
        insufficient(path, via, inn)
      else
        op_explain(pred, path, via, inn, input)
      end
    when :alt.ns
      return insufficient(path, via, inn) if input.empty?

      probs = p[:predicates].zip(p[:keys]).flat_map { |(predicate, key)|
        op_explain(predicate, key ? path.conj(key) : path, via, inn, input)
      }

      probs.compact
    when :rep.ns
      op_explain(p[:p1], path, via, inn, input)
    else
      raise "Unexpected #{:op.ns} #{p[:op.ns]}"
    end
  end

  # @private
  def self.re_gen(p, overrides, path, rmap)
    origp = p
    p = _reg_resolve!(p)

    id, op, ps, ks, p1, p2, ret, id, gen = p.values_at(
      :id, :op.ns, :predicates, :keys, :p1, :p2, :return_value, :id, :gen.ns
    ) if regex?(p)

    id = p.id if spec?(p)
    ks ||= []

    rmap = inck(rmap, id) if id

    ggens = ->(preds, keys) do
      preds.zip(keys).map do |pred, k|
        unless rmap && id && k && recur_limit?(rmap, id, path, k)
          # TODO: delay if we have an id?
          re_gen(pred, overrides, k ? path.conj(k) : path, rmap)
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
      when :accept.ns
        if ret == :nil.ns
          ->(_rantly) { [] }
        else
          ->(_rantly) { [ret] }
        end
      when nil
        g = gensub(p, overrides, path, rmap)

        ->(rantly) { [g.call(rantly)] }
      when :amp.ns
        re_gen(p1, overrides, path, rmap)
      when :pcat.ns
        gens = ggens.call(ps, ks)

        if gens.all?
          ->(rantly) do
            gens.flat_map { |gg| gg.call(rantly) }
          end
        end
      when :alt.ns
        gens = ggens.call(ps, ks).compact

        ->(rantly) { rantly.branch(*gens) } unless gens.empty?
      when :rep.ns
        if recur_limit?(rmap, id, [id], id)
          ->(_rantly) { [] }
        else
          g = re_gen(p2, overrides, path, rmap)

          if g
            # TODO: wrap in shrinkable?
            ->(rantly) do
              # TODO: how big?
              rantly.range(0, 30).times.flat_map { g.call(rantly) }
            end
          end
        end
      end
    end
  end

  # @private
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
        if p[:op.ns] == :pcat.ns
          return op_explain(p, path, via, inn.conj(index), input[index..-1])
        else
          return [{ :path   => path,
                    :reason => "Extra input",
                    :val    => input,
                    :via    => via,
                    :in     => inn.conj(index) }]
        end
      else
        return op_explain(p, path, via, inn.conj(index), input[index..-1]) ||
            [{ :path   => path,
               :reason => "Extra input",
               :val    => input,
               :via    => via,
               :in     => inn.conj(index) }]
      end
    end

    if accept_nil?(p)
      nil # success
    else
      op_explain(p, path, via, inn, nil)
    end
  end

  # returns a spec that accepts nil and values satisfying pred
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

  # Exercises the method by applying it to n (default 10) generated samples of
  # its args spec. When fspec is supplied its arg spec is used, and
  # method can be a proc. Returns an arrray of tuples of [args, ret].
  def self.exercise_fn(method, n: 10, fspec: nil)
    fspec ||= get_spec(method)
    raise ArgumentError, "No fspec found for #{method}" unless fspec

    Gen.sample(gen(fspec.argspec), n).map { |args| [args, method.call(*args)] }
  end

  # TODO: date_in_range?
  # TODO: date_in
  # TODO: int_in_range?
  # TODO: int_in
  # TODO: float_in

  # TODO: assert

  # Resets the spec registry to only builtin specs
  def self.reset_registry!
    builtins = H[
      :any.ns              => with_gen(Utils.constantly(true)) { |r| r.branch(*Gen::GEN_BUILTINS.values) },
      :boolean.ns          => Set[true, false],
      :positive_integer.ns => with_gen(self.and(Integer, :positive?.to_proc)) { |r| r.range(1) },
      # Rantly#positive_integer is actually a natural integer
      :natural_integer.ns  => with_gen(self.and(Integer, Utils.complement(&:negative?)), &:positive_integer),
      :negative_integer.ns => with_gen(self.and(Integer, :negative?.to_proc)) { |r| r.range(nil, -1) }
    ]

    @registry_ref.reset(builtins)
  end

  reset_registry!
end
