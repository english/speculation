require 'concurrent/atom'
require 'concurrent/delay'
require 'hamster/hash'
require 'hamster/vector'
require 'hamster/set'
require 'functional'

module Speculation
  using NamespacedSymbols.refine(self)

  module Core
    H = Hamster::Hash
    V = Hamster::Vector
    Protocol = Functional::Protocol

    module Specize
      refine Symbol do
        def specize
          Core.reg_resolve!(self).specize
        end
      end

      refine Object do
        def specize
          Core.spec_impl(self, false)
        end
      end
    end
    using Specize

    module Conj
      refine Hamster::Hash do
        def conj(v)
          merge(v)
        end
      end

      refine Hamster::Vector do
        def conj(x)
          add(x)
        end
      end
    end
    using Conj

    Functional.SpecifyProtocol(:Spec.ns) do
      instance_method :conform, 1
      instance_method :explain, 4
      instance_method :gen, 3
    end unless Protocol.Specified?(:Spec.ns)

    Functional.SpecifyProtocol(:Specize.ns) do
      instance_method :specize, 0
    end unless Protocol.Specified?(:Specize.ns)

    REGISTRY = Concurrent::Atom.new(H[])

    # TODO: Make this configurable
    COLL_CHECK_LIMIT = 101
    COLL_ERROR_LIMIT = 20
    RECURSION_LIMIT = 4

    def self.coll_check_limit
      COLL_CHECK_LIMIT
    end

    def self.coll_error_limit
      COLL_ERROR_LIMIT
    end

    # TODO add #inspect or #to_s so prints with name in error messages
    class Spec
      attr_accessor :name

      def initialize(predicate, should_conform, gen = nil)
        @predicate = predicate
        @should_conform = should_conform
        @gen = gen
      end

      def conform(value)
        ret = case @predicate
              when Set           then @predicate.include?(value)
              when Regexp, Class then @predicate === value
              else                    @predicate.call(value)
              end

        if @should_conform
          ret
        else
          ret ? value : :invalid.ns
        end
      end

      def explain(path, via, _in, value)
        if Core.invalid?(Core.dt(@predicate, value))
          V[H[path: path, val: value, via: via, in: _in, pred: @predicate]]
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

      Protocol.Satisfy!(self, :Specize.ns, :Spec.ns)
    end

    class AndSpec
      attr_accessor :name

      def initialize(preds, gen = nil)
        @preds = preds
        @gen = gen
        @specs = Concurrent::Delay.new do
          preds.map { |pred| Core.specize(pred) }
        end
      end

      def conform(value)
        @specs.value.each do |spec|
          value = spec.conform(value)

          return :invalid.ns if Core.invalid?(value)
        end

        value
      end

      def specize
        self
      end

      def explain(path, via, _in, value)
        Core.explain_pred_list(@preds, path, via, _in, value)
      end

      def gen(overrides, path, rmap)
        if @gen
          @gen
        else
          Core.gensub(@preds.first, overrides, path, rmap)
        end
      end

      def with_gen(gen)
        self.class.new(@preds, gen)
      end

      Protocol.Satisfy!(self, :Specize.ns, :Spec.ns)
    end

    class OrSpec
      attr_accessor :name

      def initialize(named_specs, gen = nil)
        @named_specs = named_specs
        @keys = named_specs.keys
        @preds = preds = named_specs.values
        @gen = gen

        @delayed_specs = Concurrent::Delay.new do
          preds.map { |spec| Core.specize(spec) }
        end
      end

      def conform(value)
        @delayed_specs.value.each_with_index do |spec, index|
          conformed = spec.conform(value)

          unless Core.invalid?(conformed)
            return [@keys[index], value]
          end
        end

        :invalid.ns
      end

      def specize
        self
      end

      def explain(path, via, _in, value)
        return unless !Core.pvalid?(self, value)

        V.new(@keys).zip(@preds).flat_map do |(key, pred)|
          next if Core.pvalid?(pred, value)
          Core.explain1(pred, path.conj(key), via, _in, value)
        end
      end

      def gen(overrides, path, rmap)
        return gen if @gen

        # TODO handle recur-limit with inck rmap
        gs = @keys.zip(@preds).
          map { |(k, p)| Core.gensub(p, overrides, path.conj(k), rmap) }.
          compact

        unless gs.empty?
          -> (rantly) { rantly.branch(*gs) }
        end
      end

      def with_gen(gen)
        self.class.new(@named_specs, gen)
      end

      Protocol.Satisfy!(self, :Specize.ns, :Spec.ns)
    end

    class RegexSpec
      attr_accessor :name

      def initialize(regex, gen = nil)
        @regex = regex
        @gen = gen
      end

      def conform(value)
        if value.nil? || Utils.collection?(value)
          Core.re_conform(@regex, value)
        else
          :invalid.ns
        end
      end

      def explain(path, via, _in, value)
        if value.nil? || Utils.collection?(value)
          Core.re_explain(path, via, _in, @regex, value || V[])
        else
          V[H[path: path, val: value, via: via, in: _in]]
        end
      end

      def gen(overrides, path, rmap)
        return @gen if @gen

        Core.re_gen(@regex, overrides, path, rmap)
      end

      def specize
        self
      end

      def with_gen(gen)
        self.class.new(@regex, gen)
      end

      Protocol.Satisfy!(self, :Specize.ns, :Spec.ns)
    end

    class HashSpec
      attr_accessor :name

      def initialize(req:, req_un:, req_keys:, req_specs:, opt_keys:, opt_specs:, keys_pred:, gen: nil)
        @req             = req
        @req_un          = req_un
        @req_keys        = req_keys
        @req_specs       = req_specs
        @opt_keys        = opt_keys
        @opt_specs       = opt_specs
        @keys_pred       = keys_pred
        @gen             = gen
        @key_to_spec_map = H[req_keys.concat(opt_keys).zip(req_specs.concat(opt_specs))]
      end

      def conform(value)
        return :invalid.ns unless @keys_pred.call(value)

        reg = Core.registry
        ret = value

        value.each do |key, v|
          spec_name = @key_to_spec_map.fetch(key, key)
          spec = reg[spec_name]

          next unless spec

          conformed_value = Core.conform(spec, v)

          if Core.invalid?(conformed_value)
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
          return V[H[path: path, pred: :hash?, val: value, via: via, in: _in]]
        end

        problems =
          if @keys_pred.call(value)
            V[]
          else
            pred = { req: @req, req_un: @req_un }.reject { |k, v| v.empty? }

            V[H[path: path, pred: pred, val: value, via: via, in: _in]]
          end

        problems += value.flat_map do |(k, v)|
          next unless Core.registry.key?(@key_to_spec_map[k])

          unless Core.pvalid?(@key_to_spec_map.fetch(k), v)
            Core.explain1(@key_to_spec_map.fetch(k), path.conj(k), via, _in.conj(k), v)
          end
        end

        problems.compact
      end

      def specize
        self
      end

      def gen(overrides, path, rmap)
        return @gen if @gen

        reqs = @req_keys.zip(@req_specs).
          reduce(H[]) { |m, (k, s)|
            m.put(k, Core.gensub(s, overrides, path.conj(k), rmap))
          }

        opts = @opt_keys.zip(@opt_specs).
          reduce(H[]) { |m, (k, s)|
            m.put(k, Core.gensub(s, overrides, path.conj(k), rmap))
          }

        -> (rantly) do
          count = rantly.choose(0, opts.count)
          opts = H.new(opts.to_a.shuffle.take(count))

          reqs.merge(opts).each_with_object({}) { |(k, spec_gen), h|
            h[k] = spec_gen.call(rantly)
          }
        end
      end

      def with_gen(gen)
        self.class.new(req: @req, req_un: @req_un, req_keys: @req_keys,
                       req_specs: @req_specs, opt_keys: @opt_keys, opt_specs: @opt_specs,
                       keys_pred: @keys_pred, gen: gen)
      end

      Protocol.Satisfy!(self, :Specize.ns, :Spec.ns)
    end

    class EverySpec
      attr_accessor :name

      def initialize(predicate, options, gen = nil)
        @predicate = predicate
        @options = options

        collection_predicates = [options.fetch(:kind, Utils.method(:collection?))]

        if options.key?(:count)
          collection_predicates.push(-> (coll) { coll.count == options[:count] })
        elsif options.key?(:min_count) || options.key?(:max_count)
          collection_predicates.push(-> (coll) do
            min = options.fetch(:min_count, 0)
            max = options.fetch(:max_count, Float::INFINITY)

            coll.count.between?(min, max)
          end)
        end

        @collection_predicate = -> (coll) { collection_predicates.all? { |f| f.call(coll) } }
        @delayed_spec = Concurrent::Delay.new { Core.specize(predicate) }
        @kfn = options.fetch(:kfn, -> (i, v) { i })
        @conform_keys, @conform_all, @kind, @gen_into, @gen_max, @distinct, @count, @min_count, @max_count =
          options.values_at(:conform_keys, :conform_all.ns, :kind, :gen_into, :gen_max, :distinct, :count, :min_count, :max_count)
        @gen_max ||= 20
      end

      def conform(value)
        return :invalid.ns unless @collection_predicate.call(value)

        spec = @delayed_spec.value

        if @conform_all
          return_value = init(value)

          value.each_with_index do |value, index|
            conformed_value = spec.conform(value)

            if Core.invalid?(conformed_value)
              return :invalid.ns
            else
              return_value = add(return_value, index, value, conformed_value)
            end
          end

          return_value
        else
          # OPTIMIZE: check if value is indexed (array, hash etc.) vs not
          # indexed (list, custom enumerable)
          limit = Core.coll_check_limit

          value.each_with_index do |item, index|
            return value if index == limit
            return :invalid.ns unless Core.valid?(spec, item)
          end

          value
        end
      end

      def specize
        self
      end

      def explain(path, via, _in, value)
        probs = Core.collection_problems(value, @kind, @distinct, @count, @min_count, @max_count, path, via, _in)
        return probs if probs

        spec = @delayed_spec.value

        probs = value.lazy.each_with_index.flat_map do |v, i|
          k = @kfn.call(i, v)

          unless Core.valid?(spec, v)
            Core.explain1(@predicate, path, via, _in.conj(k), v)
          end
        end

        probs = @conform_all ? probs.to_a : probs.take(Core.coll_error_limit)

        V.new(probs.compact)
      end

      def gen(overrides, path, rmap)
        return @gen if @gen

        pgen = Core.gensub(@predicate, overrides, path, rmap)
        # TODO handle ~~:gen-into~~ and :kind options

        -> (rantly) do
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

          # TODO handle gen_into: Set[] etc.
          if @gen_into.is_a?(Hash)
            val.to_h
          else
            val
          end
        end
      end

      def with_gen(gen)
        self.class.new(@predicate, @options, gen)
      end

      private

      def add(coll, index, value, conformed_value)
        if Utils.hash?(coll)
          key = @conform_keys ? conformed_value.first : value.first

          coll.merge(key => conformed_value.last)
        else
          coll + [value]
        end
      end

      def init(coll)
        # OPTIMIZE if we're not conforming we can return `coll`
        coll.class.new
      end

      Protocol.Satisfy!(self, :Specize.ns, :Spec.ns)
    end

    class TupleSpec
      attr_accessor :name

      def initialize(preds, gen = nil)
        @preds = preds
        @gen   = gen

        @delayed_specs = Concurrent::Delay.new do
          preds.map { |pred| Core.specize(pred) }
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

          if Core.invalid?(conformed_value)
            return :invalid.ns
          else
            return_value += [conformed_value]
          end
        end

        return_value
      end

      def explain(path, via, _in, value)
        if !Utils.array?(value)
          V[H[path: path, val: value, via: via, in: _in, pred: "array?"]]
        elsif @preds.count != value.count
          V[H[path: path, val: value, via: via, in: _in, pred: "count == predicates.count"]]
        else
          probs = @preds.zip(value).each_with_index.flat_map do |(pred, x), index|
            unless Core.pvalid?(pred, x)
              Core.explain1(pred, path.conj(index), via, _in.conj(index), x)
            end
          end

          V.new(probs.compact)
        end
      end

      def gen(overrides, path, rmap)
        return @gen if @gen

        gens = @preds.each_with_index.
          map { |p, i| Core.gensub(p, overrides, path.conj(i), rmap) }

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

      Protocol.Satisfy!(self, :Specize.ns, :Spec.ns)
    end

    class MethodSpec
      attr_reader :args, :ret, :fn
      attr_accessor :name

      def initialize(args: nil, ret: nil, fn: nil, gen: nil)
        @args = args
        @ret = ret
        @fn = fn
        @gen = gen
      end

      def conform(value)
      end

      def explain(path, via, _in, value)
      end

      def with_gen(gen)
        self.class.new(args: @args, ret: @ret, fn: @fn, gen: @gen)
      end

      def gen(overrides, path, rmap)
        return @gen if @gen

        -> (rantly) do
          -> (*args) do
            unless Core.pvalid?(@args, args)
              raise Core.explain(@args, args)
            end

            Gen.generate(Core.gen(@ret, overrides))
          end
        end
      end

      def specize
        self
      end

      Protocol.Satisfy!(self, :Specize.ns, :Spec.ns)
    end

    class NilableSpec
      attr_accessor :name

      def initialize(pred, gen = nil)
        @pred = pred
        @gen  = gen
        @delayed_spec = Concurrent::Delay.new { Core.specize(pred) }
      end

      def conform(value)
        value.nil? ? value : @delayed_spec.value.conform(value)
      end

      def explain(path, via, _in, value)
        return if Core.pvalid?(@delayed_spec.value, value) || value.nil?

        Core.
          explain1(@pred, path.conj(:pred.ns), via, _in, value).
          conj(H[path: path.conj(:nil.ns), pred: NilClass, val: value, via: via, in: _in])
      end

      def gen(overrides, path, rmap)
        return @gen if @gen

        -> (rantly) do
          rantly.freq([1, Utils.constantly(nil)],
                      [9, Core.gensub(@pred, overrides, path.conj(:pred.ns), rmap)])
        end
      end

      def with_gen(gen)
        self.class.new(@pred, gen)
      end

      def specize
        self
      end

      Protocol.Satisfy!(self, :Specize.ns, :Spec.ns)
    end

    def self.registry
      REGISTRY.value
    end

    def self.def(key, spec)
      unless key.is_a?(Symbol) && key.namespaced?
        raise ArgumentError,
          "key must be a namespaced Symbol, e.g. #{:my_spec.ns}, given #{key}"
      end

      spec = if spec?(spec) || regex?(spec) || registry[spec]
               spec
             else
               self.spec_impl(spec, false)
             end

      REGISTRY.swap { |reg| reg.store(key, with_name(spec, key)) }

      key
    end

    def self.fdef(method, spec)
      self.def(:"__method__#{method.name}/#{method.hash}", fspec(spec))
    end

    def self.fspec(args: nil, ret: nil, fn: nil) # TODO :gen
      MethodSpec.new(args: spec(args), ret: spec(ret), fn: spec(fn))
    end

    def self.get_spec(key)
      case key
      when Method, UnboundMethod
        registry[:"__method__#{key.name}/#{key.hash}"]
      else
        registry[key.to_sym]
      end
    end

    def self.with_name(spec, name)
      if spec.is_a?(Symbol)
        spec
      elsif regex?(spec)
        spec.put(:name.ns, name)
      else
        spec.tap { |s| s.name = name }
      end
    end

    def self.spec(pred)
      spec_impl(pred, false)
    end

    def self.nilable(pred)
      NilableSpec.new(pred)
    end

    def self.spec_impl(pred, should_conform)
      if spec?(pred)
        pred
      elsif regex?(pred)
        RegexSpec.new(pred)
      elsif pred.is_a?(Symbol)
        the_spec(pred)
      else
        Spec.new(pred, should_conform)
      end
    end

    def self.spec?(spec)
      spec if Protocol.Satisfy?(spec, :Spec.ns)
    end

    def self.reset_registry!
      REGISTRY.reset(H[])
    end

    def self.conform(spec, value)
      spec = specize(spec)
      spec = RegexSpec.new(spec) if regex?(spec)

      spec.conform(value)
    end

    def self.valid?(spec, value)
      spec = specize(spec)
      spec = RegexSpec.new(spec) if regex?(spec)
      value = spec.conform(value)

      !invalid?(value)
    end

    def self.invalid?(value)
      value.equal?(:invalid.ns)
    end

    def self.conformer(f)
      spec_impl(f, true)
    end

    def self.and(*preds)
      AndSpec.new(preds)
    end

    def self.or(named_specs)
      OrSpec.new(named_specs)
    end

    def self.cat(named_specs)
      keys = named_specs.keys
      predicates = named_specs.values

      pcat(H[keys: keys, predicates: predicates, return_value: H[]])
    end

    def self.alt(kv_specs)
      _alt(kv_specs.values, kv_specs.keys)
    end

    def self.zero_or_more(predicate)
      rep(predicate, predicate, V[], false)
    end

    def self.one_or_more(predicate)
      pcat(H[predicates: [predicate, rep(predicate, predicate, V[], true)], return_value: V[]])
    end

    def self.zero_or_one(predicate)
      _alt([predicate, accept(:nil.ns)], nil)
    end

    def self.constrained(regex, *preds)
      H[:op.ns => :amp.ns, p1: regex, predicates: preds]
    end

    def self.keys(req: [], opt: [], req_un: [], opt_un: [])
      extract_keys = -> (symbol_or_arr) do
        if symbol_or_arr.is_a?(Array)
          symbol_or_arr[1..-1].flat_map(&extract_keys)
        else
          symbol_or_arr
        end
      end

      req_keys     = req.flat_map(&extract_keys)
      req_un_specs = req_un.flat_map(&extract_keys)

      unless (req_keys + req_un_specs + opt + opt_un).all? { |s| s.is_a?(Symbol) && s.namespaced? }
        raise "all keys must be namespaced"
      end

      unk = -> (x) { x.unnamespaced }

      req_specs = req_keys + req_un_specs
      req_keys  = req_keys + req_un_specs.map(&unk)

      opt_keys  = opt + opt_un.map(&unk)
      opt_specs = opt + opt_un

      pred_exprs = [Utils.method(:hash?)]

      parse_req = -> (ks, v, f) do
        k, *ks = ks

        ret = if k.is_a?(Array)
          op, *kks = k
          case op
          when :or  then kks.one? { |k| parse_req.call([k], v, f) }
          when :and then kks.all? { |k| parse_req.call([k], v, f) }
          else      raise "Expected or, and, got #{op}"
          end
        else
          v.key?(f.call(k))
        end

        if ks.any?
          ret && parse_req.call(ks, v, f)
        else
          ret
        end
      end

      pred_exprs.push(-> (v) { parse_req.call(req, v, Utils.method(:identity)) }) if req.any?
      pred_exprs.push(-> (v) { parse_req.call(req_un, v, unk) }) if req_un.any?
      keys_pred = -> (v) { pred_exprs.all? { |p| p.call(v) } }

      HashSpec.new(req: req, req_un: req_un,
                   req_keys: V.new(req_keys), req_specs: V.new(req_specs),
                   opt_keys: V.new(opt_keys), opt_specs: V.new(opt_specs),
                   keys_pred: keys_pred)
    end

    def self.keys_or(*ks)
      [:or, *ks]
    end

    def self.keys_and(*ks)
      [:and, *ks]
    end

    def self.coll_of(spec, opts = {})
      every(spec, :conform_all.ns => true, **opts)
    end

    def self.tuple(*specs)
      TupleSpec.new(specs)
    end

    def self.hash_of(key_predicate, value_predicate, options = {})
      every_kv(key_predicate, value_predicate, kind: Utils.method(:hash?).to_proc, gen_into: {}, :conform_all.ns => true, **options)
    end

    def self.every_kv(key_predicate, value_predicate, options)
      kfn = -> (i, v) { v.first }
      every(tuple(key_predicate, value_predicate), kfn: kfn, **options)
    end

    def self.every(predicate, options)
      EverySpec.new(predicate, options)
    end

    def self.explain_data(spec, value)
      _explain_data(spec, V[], V[spec_name(spec)], V[], value)
    end

    def self.explain(spec, x)
      explain_str(explain_data(spec, x))
    end

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

    def self._explain_data(spec, path, via, _in, value)
      probs = specize(spec).explain(path, via, _in, value)

      if probs&.any?
        # TODO: deal with procs better...
        probs = probs.map do |p|
          p.put(:pred) { |pred| Proc === pred ? "<proc>" : pred }
        end

        H[:problems.ns => probs]
      end
    end

    def self.explain_pred(predicate)
      if Proc === predicate
        "<proc>"
      else
        predicate
      end
    end

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

    def self.explain1(pred, path, via, _in, value)
      spec = maybe_spec(pred)

      if spec?(spec)
        name = spec_name(spec)
        via = via.conj(name) if name

        spec.explain(path, via, _in, value)
      else
        V[H[path: path, val: value, via: via, in: _in, pred: pred]]
      end
    end

    def self.rep(p1, p2, return_value, splice)
      return unless p1

      regex = H[:op.ns => :rep.ns, p2: p2, splice: splice]

      regex = if accept?(p1)
        regex.merge(p1: p2, return_value: return_value.add(p1[:return_value]))
      else
        regex.merge(p1: p1, return_value: return_value)
      end
    end

    def self.pcat(regex)
      predicate, *rest_predicates = regex[:predicates]

      keys = regex[:keys]
      key, *rest_keys = keys

      return unless regex[:predicates].all?

      unless accept?(predicate)
        return H[:op.ns => :pcat.ns,
                 predicates: regex[:predicates], keys: keys,
                 return_value: regex[:return_value]]
      end

      val = keys ? H[key => predicate[:return_value]] : predicate[:return_value]
      return_value = regex[:return_value].conj(val)

      if rest_predicates
        pcat(H[predicates: rest_predicates,
               keys: rest_keys,
               return_value: return_value])
      else
        accept(return_value)
      end
    end

    def self.regex?(x)
      Utils.hash?(x) && x[:op.ns] && x
    end

    def self.accept(x)
      H[:op.ns => :accept.ns, return_value: x]
    end

    def self.accept?(hash)
      if hash.is_a?(H)
        hash[:op.ns] == :accept.ns
      end
    end

    def self.reg_resolve(key)
      return key unless key.is_a?(Symbol)

      spec = registry[key]

      if spec.is_a?(Symbol)
        deep_resolve(registry, spec)
      else
        spec
      end
    end

    def self.reg_resolve!(key)
      return key unless key.is_a?(Symbol)
      reg_resolve(key) or raise "Unable to resolve spec: #{key}"
    end

    def self.deep_resolve(reg, spec)
      spec = reg[spec] until !spec.is_a?(Symbol)
      spec
    end

    def self.specize(spec)
      spec?(spec) || spec.specize
    end

    def self.alt2(p1, p2)
      if p1 && p2
        _alt([p1, p2], nil)
      else
        p1 || p2
      end
    end

    def self._alt(predicates, keys)
      identity = Utils.method(:identity)
      predicates, keys = filter_alt(predicates, keys, &identity)
      return unless predicates

      predicate, *rest_predicates = predicates
      key, *rest_keys = keys

      return_value = H[:op.ns => :alt.ns, predicates: predicates, keys: keys]
      return return_value unless rest_predicates.empty?

      return predicate unless key
      return return_value unless accept?(predicate)

      accept(V[key, predicate[:return_value]])
    end

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

    def self.accept_nil?(regex)
      regex = reg_resolve!(regex)
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

    def self.no_ret?(p1, pret)
      return true if pret == :nil.ns

      regex = reg_resolve!(p1)
      op = regex[:op.ns]

      [:rep.ns, :pcat.ns].include?(op) && pret.empty? || nil
    end

    def self.and_preds(x, preds)
      preds.each do |pred|
        x = dt(pred, x)

        return :invalid.ns if invalid?(x)
      end

      x
    end

    def self.preturn(regex)
      regex = reg_resolve!(regex)
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
          V[ks.first, r]
        else
          r
        end
      else
        raise "Unexpected #{:op.ns} #{regex[:op.ns]}"
      end
    end

    def self.filter_alt(ps, ks, &block)
      if ks
        pks = ps.zip(ks).select { |xs| block.call(xs.first) }
        [pks.map(&:first), pks.map(&:last)]
      else
        [ps.select(&block), ks]
      end
    end

    def self.deriv(predicate, value)
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

      case regex[:op.ns]
      when :accept.ns then nil
      when :pcat.ns
        regex1 = pcat(H[predicates: V[deriv(pred, value), *rest_preds], keys: keys, return_value: return_value])
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

    def self.dt(pred, x)
      return x unless pred

      spec = the_spec(pred)

      if spec
        conform(spec, x)
      else
        if pred.is_a?(Class) || pred.is_a?(Proc) || pred.is_a?(::Regexp)
          pred === x ? x : :invalid.ns
        elsif pred.is_a?(Set)
          pred.include?(x) ? x : :invalid.ns
        else
          raise "#{pred} is not a class, proc, set or regexp"
        end
      end
    end

    def self.the_spec(spec_or_key)
      spec = maybe_spec(spec_or_key)
      return spec if spec

      if Symbol === spec_or_key
        raise "Unable to resolve spec: #{spec_or_key}"
      end
    end

    def self.maybe_spec(spec_or_key)
      spec = (Symbol === spec_or_key && reg_resolve(spec_or_key)) ||
        spec?(spec_or_key) ||
        regex?(spec_or_key) ||
        nil

      if regex?(spec)
        with_name(RegexSpec.new(spec), spec_name(spec))
      else
        spec
      end
    end

    def self.spec_name(spec)
      if spec.is_a?(Symbol)
        spec
      elsif regex?(spec)
        spec[:name.ns]
      elsif spec.respond_to?(:name)
        spec.name
      end
    end

    def self.add_ret(regex, r, key)
      regex = reg_resolve!(regex)
      return r unless regex?(regex)

      prop = -> do
        return_value = preturn(regex)

        if return_value.empty?
          r
        else
          val = key ? H[key => return_value] : return_value
          regex[:splice] ? r + val : r.conj(val)
        end
      end

      case regex[:op.ns]
      when :accept.ns, :alt.ns, :amp.ns
        return_value = preturn(regex)

        if return_value == :nil.ns
          r
        else
          r.conj(key ? H[key => return_value] : return_value)
        end
      when :pcat.ns, :rep.ns then prop.call
      else
        raise "Unexpected #{:op.ns} #{regex[:op.ns]}"
      end
    end

    def self.pvalid?(pred, value)
      !invalid?(dt(pred, value))
    end

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
            return V[H[path: path,
                       reason: "Extra input",
                       val: input,
                       via: via,
                       in: _in.conj(index)]]
          end
        else
          return op_explain(p, path, via, _in.conj(index), input[index..-1]) ||
            V[H[path: path,
                reason: "Extra input",
                val: input,
                via: via,
                in: _in.conj(index)]]
        end
      end

      if accept_nil?(p)
        nil # success
      else
        op_explain(p, path, via, _in, nil)
      end
    end

    def self.op_explain(p, path, via, _in, input)
      p = reg_resolve!(p)
      return unless p

      insufficient = -> (path) do
        V[H[path: path,
            reason: "Insufficient input",
            val: V[],
            via: via,
            in: _in]]
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

        V.new(probs.compact)
      when :rep.ns
        op_explain(p[:p1], path, via, _in, input)
      else
        raise "Unexpected #{:op.ns} #{p[:op.ns]}"
      end
    end

    def self.collection_problems(x, kfn, distinct, count, min_count, max_count, path, via, _in)
      pred = kfn || Utils.method(:collection?).to_proc

      if !pvalid?(pred, x)
        return explain1(pred, path, via, _in, x)
      end

      if count && count != x.count
        return V[H[path: path, pred: 'count == x.count', val: x, via: via, in: _in]]
      end

      if min_count || max_count
        if x.count.between?(min_count || 0, max_count || Float::Infinity)
          return V[H[path: path, pred: 'count.between?(min_count || 0, max_count || Float::Infinity)', val: x, via: via, in: _in]]
        end
      end

      if distinct && !x.empty? && Utils.distinct?(x)
        V[H[path: path, pred: 'distinct?', val: x, via: via, in: _in]]
      end
    end

    def self.gen(spec, overrides = nil)
      gensub(spec, nil, V[], H[:recursion_limit.ns => RECURSION_LIMIT])
    end

    def self.gensub(spec, overrides, path, rmap)
      overrides ||= {}
      spec = specize(spec)
      gfn = overrides[spec.name || spec] || overrides[path]
      # TODO gfn.call
      g = gfn ? gfn : spec.gen(overrides, path, rmap)

      if g
        Gen.such_that(-> (x) { Core.valid?(spec, x) }, g, 100)
      else
        raise "unable to construct gen at: #{path.inspect} for: #{spec.inspect}"
      end
    end

    def self.re_gen(p, overrides, path, rmap)
      origp = p
      p = reg_resolve!(p)

      op, ps, ks, p1, p2, splice, ret, id, gen = p.values_at(
        :op.ns, :predicates, :keys, :p1, :p2, :splice, :return_value, :id, :gen.ns
      ) if regex?(p)

      # TODO inck rmap
      ggens = -> (ps, ks) do
        ps.zip(ks).map do |p, k|
          re_gen(p, overrides, k ? path.conj(k) : k, rmap)
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
          # TODO: recur_limit
          g = re_gen(p2, overrides, path, rmap)

          if g
            # TODO wrap in shrinkable?
            -> (rantly) do
              # TODO how big?
              rantly.range(0, 6).times.flat_map { g.call(rantly) }
            end
          end
        end
      end
    end

    def self.exercise(spec, n: 10, overrides: {})
      Gen.sample(gen(spec, overrides), n)
    end

    def self.with_gen(spec, &gen)
      if regex?(spec)
        spec.put(:gfn.ns, gen)
      else
        specize(spec).with_gen(gen)
      end
    end
  end
end
