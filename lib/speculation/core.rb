require 'concurrent/atom'
require 'concurrent/delay'
require 'hamster/hash'
require 'hamster/vector'
require 'hamster/set'
require 'functional'

module Speculation
  module Core
    def self.namespaced_symbols(namespace)
      Module.new do
        refine Symbol do
          define_method(:ns) do |x = nil|
            if x
              :"#{x}/#{self}"
            else
              :"#{namespace}/#{self}"
            end
          end

          def namespaced?
            to_s.include?("/")
          end

          def unnamespaced
            to_s.split("/").last.to_sym
          end
        end
      end
    end
    using namespaced_symbols(self)

    module Specize
      refine Symbol do
        def specize
          Core.reg_resolve!(self).specize
        end
      end

      refine Object do
        def specize
          Core.spec(self)
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

    module SpecName
      refine Proc do
        def name=(name)
          @_specualtion_name = name
        end

        def name
          @_specualtion_name
        end
      end
    end
    using SpecName

    Functional.SpecifyProtocol(:Spec.ns) do
      instance_method :conform, 1
      instance_method :explain, 4
    end

    Functional.SpecifyProtocol(:Specize.ns) do
      instance_method :specize, 0
    end

    H = Hamster::Hash
    V = Hamster::Vector
    Protocol = Functional::Protocol

    REGISTRY = Concurrent::Atom.new(H[])

    class PredicateSpec
      attr_accessor :name

      def initialize(predicate)
        @predicate = predicate
      end

      def conform(value)
        # calling #=== here so that a either a class or proc can be provided
        @predicate === value ? value : :invalid.ns
      end

      def explain(path, via, _in, value)
        if Core.invalid?(Core.dt(@predicate, value))
          V[H[path: path, val: value, via: via, in: _in, pred: @predicate]]
        end
      end

      def specize
        self
      end

      Protocol.Satisfy!(self, :Specize.ns, :Spec.ns)
    end

    class AndSpec
      attr_accessor :name

      def initialize(preds)
        @preds = preds
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

      Protocol.Satisfy!(self, :Specize.ns, :Spec.ns)
    end

    class OrSpec
      attr_accessor :name

      def initialize(named_specs)
        @keys = named_specs.keys
        @preds = preds = named_specs.values

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

      Protocol.Satisfy!(self, :Specize.ns, :Spec.ns)
    end

    class RegexSpec
      attr_accessor :name

      def initialize(regex)
        @regex = regex
      end

      def conform(value)
        if value.nil? || value.respond_to?(:each)
          Core.re_conform(@regex, value)
        else
          :invalid.ns
        end
      end

      def explain(path, via, _in, value)
        if value.nil? || value.respond_to?(:each)
          Core.re_explain(path, via, _in, @regex, value || V[])
        else
          V[H[path: path, val: value, via: via, in: _in]]
        end
      end

      def specize
        self
      end

      Protocol.Satisfy!(self, :Specize.ns, :Spec.ns)
    end

    class HashSpec
      attr_accessor :name

      def initialize(req_keys:, req_specs:, opt_keys:, opt_specs:)
        # TODO: opt_keys???
        @req_keys = req_keys
        @key_to_spec_map = H[req_keys.concat(opt_keys).zip(req_specs.concat(opt_specs))]
      end

      def conform(value)
        return :invalid.ns unless @req_keys.to_set.subset?(value.keys)

        reg = Core.registry

        ret = value
        keys = value

        keys.each do |key, value|
          sname = @key_to_spec_map.fetch(key, key)
          spec = reg[sname]

          next unless spec

          conformed_value = spec.conform(value)

          if Core.invalid?(conformed_value)
            return :invalid.ns
          else
            unless conformed_value.equal?(value)
              ret = ret.merge(key => conformed_value)
            end
          end
        end

        ret
      end

      def explain(path, via, _in, value)
        unless value.respond_to?(:key?)
          return V[H[path: path, pred: :hash?, val: value, via: via, in: _in]]
        end

        problems = @req_keys.
          reject { |k| value.key?(k) }.
          map { |k| H[path: path, pred: "key?(#{k.inspect})", val: value, via: via, in: _in] }

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

      Protocol.Satisfy!(self, :Specize.ns, :Spec.ns)
    end

    class EverySpec
      attr_accessor :name

      def initialize(predicate, options)
        collection_predicates = [options.fetch(:kind, -> (coll) { coll.respond_to?(:each) })]

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
        @predicate = predicate
        @delayed_spec = Concurrent::Delay.new { Core.specize(predicate) }
        @kfn = options.fetch(:kfn, -> (i, v) { i })

        @conform_all, @kind, @distinct, @count, @min_count, @max_count =
          options.values_at(:conform_all, :kind, :distinct, :count, :min_count, :max_count)
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
              return_value = add(return_value, index, conformed_value)
            end
          end

          return_value
        else
          raise "TODO: handle not conforming all"
        end
      end

      def specize
        self
      end

      def explain(path, via, _in, value)
        probs = Core.coll_prob(value, @kind, @distinct, @count, @min_count, @max_count, path, via, _in)
        return probs if probs

        spec = @delayed_spec.value

        if @conform_all
          probs = value.each_with_index.flat_map do |v, i|
            k = @kfn.call(i, v)

            unless Core.valid?(spec, v)
              Core.explain1(@predicate, path, via, _in.conj(k), v)
            end
          end

          V.new(probs.compact)
        else
          raise "TODO: Handle @conform_all = false"
        end
      end

      private

      def add(coll, index, value)
        if coll.respond_to?(:key?)
          coll.merge(value.first => value.last)
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

      def initialize(preds)
        @preds = preds

        @delayed_specs = Concurrent::Delay.new do
          preds.map { |pred| Core.specize(pred) }
        end
      end

      def conform(collection)
        specs = @delayed_specs.value

        unless collection.respond_to?(:count) && collection.count == specs.count
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
        if !value.respond_to?(:each)
          V[H[path: path, val: value, via: via, in: _in, pred: "respond_to?(:each)"]]
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

      spec = if spec?(spec) || regex?(spec) || registry[key]
               spec
             else
               self.spec(spec)
             end

      REGISTRY.swap { |reg| reg.store(key, with_name(spec, key)) }

      key
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
      if spec?(pred)
        pred
      elsif regex?(pred)
        RegexSpec.new(pred)
      elsif pred.is_a?(Symbol)
        the_spec(pred)
      else
        PredicateSpec.new(pred)
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
      unless (req + opt + req_un + opt_un).all? { |s| s.namespaced? }
        raise "all keys must be namespaced"
      end

      req_specs = req + req_un
      req_keys = req + req_un.map { |k| k.unnamespaced }

      HashSpec.new(req_keys: V.new(req_keys), req_specs: V.new(req_specs),
                   opt_keys: V.new(opt), opt_specs: V.new(opt))
    end

    def self.coll_of(spec, opts = {})
      every(spec, conform_all: true, **opts)
    end

    def self.tuple(*specs)
      TupleSpec.new(specs)
    end

    def self.map_of(key_predicate, value_predicate)
      every_kv(key_predicate, value_predicate, kind: -> (x) { x.respond_to?(:key?) }, conform_all: true)
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

    def self.explain(spec, x, out = StringIO.new)
      data = explain_data(spec, x)
      return "Success!" unless data

      s = ""

      data.fetch(:problems.ns).each do |prob|
        path, pred, val, reason, via, _in = prob.values_at(:path, :pred, :val, :reason, :via, :in)

        s << "In: #{_in.inspect} " unless _in.empty?
        s << "val: #{val.inspect} fails"
        s << " spec: #{via.last.inspect}" unless via.empty?
        s << " at: #{path.inspect}" unless path.empty?
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

    ######## crazy shit ########

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
      x.respond_to?(:get) && x.get(:op.ns) && x
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
      spec = reg[spec] until spec.is_a?(Symbol)
    end

    ### private ###

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
      identity = -> (x) { x }
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
        spec.conform(x)
      else
        if pred.is_a?(Class) || pred.is_a?(Proc) || pred.is_a?(::Regexp)
          pred === x ? x : :invalid.ns
        else
          raise "#{pred} is not a class, proc or regexp, expected a predicate proc or type"
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
      spec = (Symbol === spec_or_key && reg_resolve!(spec_or_key)) ||
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
        spec.fetch(:name.ns)
      else
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

    def self.coll_prob(x, kfn, distinct, count, min_count, max_count, path, via, _in)
      pred = kfn || -> (coll) { coll.respond_to?(:each) }

      if !pvalid?(pred, x)
        return explain1(pred, path, via, _in, x)
      end

      if count && count != x.count
        return V[H[path: path, pred: 'count == x.count', val: x, via: via, in: _in]]
      end

      if min_count || max_count
        if x.count.between?(min_count || 0, max_count || Float::Infinity)
          return V[H[path: path, pred: 'x.count.between?(min_count || 0, max_count || Float::Infinity)', val: x, via: via, in: _in]]
        end
      end

      if distinct && !x.empty? && x.uniq.count != x.count # OPTIMIZE: distinct check
        V[H[path: path, pred: 'distinct?', val: x, via: via, in: _in]]
      end
    end
  end
end
