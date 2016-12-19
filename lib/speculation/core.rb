require 'concurrent/atom'
require 'concurrent/delay'
require 'hamster/hash'
require 'hamster/vector'
require 'hamster/set'
require 'functional'

module Speculation
  module Core
    module NS
      refine Symbol do
        def ns(mod)
          :"#{mod}/#{self}"
        end
      end
    end
    using NS

    module Specize
      refine Symbol do
        def specize
          Core.reg_resolve!(self)
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

    Functional.SpecifyProtocol(:Spec.ns(self)) do
      instance_method :conform, 1
    end

    Functional.SpecifyProtocol(:Specize.ns(self)) do
      instance_method :specize, 0
    end

    H = Hamster::Hash
    V = Hamster::Vector
    Protocol = Functional::Protocol

    REGISTRY = Concurrent::Atom.new(H[])

    class PredicateSpec
      def initialize(predicate)
        @predicate = predicate
      end

      def conform(value)
        # calling #=== here so that a either a class or proc can be provided
        @predicate === value ? value : :invalid.ns(Core)
      end

      def specize
        self
      end

      Protocol.Satisfy!(self, :Specize.ns(Core), :Spec.ns(Core))
    end

    class AndSpec
      def initialize(specs)
        @specs = specs
      end

      def conform(value)
        @specs.value.each do |spec|
          value = spec.conform(value)

          return :invalid.ns(Core) if Core.invalid?(value)
        end

        value
      end

      def specize
        self
      end

      Protocol.Satisfy!(self, :Specize.ns(Core), :Spec.ns(Core))
    end

    class OrSpec
      def initialize(keys, specs)
        @keys = keys
        @specs = specs
      end

      def conform(value)
        @specs.value.each_with_index do |spec, index|
          conformed = spec.conform(value)

          unless Core.invalid?(conformed)
            return [@keys[index], value]
          end
        end

        :invalid.ns(Core)
      end

      def specize
        self
      end

      Protocol.Satisfy!(self, :Specize.ns(Core), :Spec.ns(Core))
    end

    class RegexSpec
      def initialize(regex)
        @regex = regex
      end

      def conform(value)
        if value.nil? || value.respond_to?(:each)
          Core.re_conform(@regex, Array(value))
        else
          :invalid.ns(Core)
        end
      end

      def specize
        self
      end

      Protocol.Satisfy!(self, :Specize.ns(Core), :Spec.ns(Core))
    end

    class HashSpec
      def initialize(keys_pred:, req_keys:, req_specs:, opt_keys:, opt_specs:)
        @keys_pred = keys_pred
        @req_keys  = req_keys
        @req_specs = req_specs
        @opt_keys  = opt_keys
        @opt_specs = opt_specs
      end

      def conform(value)
        # is this needed?
        key_to_spec_map = H[@req_keys.concat(@opt_keys).zip(@req_specs.concat(@opt_specs))]

        return :invalid.ns(Core) unless @keys_pred.call(value)
        reg = Core.registry

        ret = value
        keys = value

        keys.each do |key, value|
          sname = key_to_spec_map.fetch(key, key)
          spec = reg[sname]

          next unless spec

          conformed_value = spec.conform(value)

          if Core.invalid?(conformed_value)
            return :invalid.ns(Core)
          else
            unless conformed_value.equal?(value)
              ret = ret.merge(key => conformed_value)
            end
          end
        end

        ret
      end

      def specize
        self
      end

      Protocol.Satisfy!(self, :Specize.ns(Core), :Spec.ns(Core))
    end

    class EverySpec
      def initialize(delayed_spec, collection_predicate, conform_all)
        @delayed_spec = delayed_spec
        @collection_predicate = collection_predicate
        @conform_all = conform_all
      end

      def conform(value)
        return :invalid.ns(Core) unless @collection_predicate.call(value)

        spec = @delayed_spec.value

        if @conform_all
          return_value = init(value)

          value.each_with_index do |value, index|
            conformed_value = spec.conform(value)

            if Core.invalid?(conformed_value)
              return :invalid.ns(Core)
            else
              return_value = add(return_value, index, conformed_value)
            end
          end

          return_value
        else
          raise "Can't handle not conforming all"
        end
      end

      def specize
        self
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

      Protocol.Satisfy!(self, :Specize.ns(Core), :Spec.ns(Core))
    end

    class TupleSpec
      def initialize(delayed_specs)
        @delayed_specs = delayed_specs
      end

      def conform(collection)
        specs = @delayed_specs.value

        unless collection.respond_to?(:count) && collection.count == specs.count
          return :invalid.ns(Core)
        end

        return_value = collection.class.new

        collection.zip(specs).each do |(value, spec)|
          conformed_value = spec.conform(value)

          if Core.invalid?(conformed_value)
            return :invalid.ns(Core)
          else
            return_value += [conformed_value]
          end
        end

        return_value
      end

      def specize
        self
      end

      Protocol.Satisfy!(self, :Specize.ns(Core), :Spec.ns(Core))
    end

    def self.registry
      REGISTRY.value
    end

    def self.def(key, spec)
      spec = if spec?(spec) || regex?(spec) || registry[key]
               spec
             else
               self.spec(spec)
             end

      REGISTRY.swap { |reg| reg.store(key, spec) }

      key
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
      spec if Protocol.Satisfy?(spec, :Spec.ns(self))
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
      value.equal?(:invalid.ns(self))
    end

    def self.and(*specs)
      delayed_specs = Concurrent::Delay.new do
        specs.map { |spec| specize(spec) }
      end

      AndSpec.new(delayed_specs)
    end

    def self.or(named_specs)
      keys = named_specs.keys

      delayed_specs = Concurrent::Delay.new do
        named_specs.values.map { |spec| specize(spec) }
      end

      OrSpec.new(keys, delayed_specs)
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
      _alt([predicate, accept(:nil.ns(self))], nil)
    end

    def self.constrained(regex, *preds)
      H[:op.ns(self) => :amp.ns(self), p1: regex, predicates: preds]
    end

    def self.keys(req: V[], opt: V[])
      # arbitrary map test
      is_map = -> (x) { x.respond_to?(:key?) }
      pred_exprs = V[is_map]
      pred_exprs += req.map do |key|
        -> (x) { x.key?(key) }
      end

      keys_pred = -> (x) { pred_exprs.all? { |f| f.call(x) } }

      HashSpec.new(keys_pred: keys_pred,
                   req_keys: req, req_specs: req,
                   opt_keys: opt, opt_specs: opt)
    end

    def self.coll_of(spec, opts = {})
      every(spec, conform_all: true, **opts)
    end

    def self.tuple(*specs)
      delayed_specs = Concurrent::Delay.new do
        specs.map { |spec| specize(spec) }
      end

      TupleSpec.new(delayed_specs)
    end

    def self.map_of(key_predicate, value_predicate)
      every_kv(key_predicate, value_predicate, kind: -> (x) { x.respond_to?(:key?) }, conform_all: true)
    end

    def self.every_kv(key_predicate, value_predicate, options)
      every(tuple(key_predicate, value_predicate), **options)
    end

    def self.every(predicate, options)
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

      collection_predicate = -> (coll) { collection_predicates.all? { |f| f.call(coll) } }

      delayed_spec = Concurrent::Delay.new { specize(predicate) }

      EverySpec.new(delayed_spec, collection_predicate, options[:conform_all])
    end

    def self.rep(p1, p2, return_value, splice)
      return unless p1

      regex = H[:op.ns(self) => :rep.ns(self), p2: p2, splice: splice]

      regex = if accept?(p1)
        regex.merge(p1: p2, return_value: return_value.add(p1[:return_value]))
      else
        regex.merge(p1: p1, return_value: return_value)
      end
    end

    ######## crazy shit ########

    def self.pcat(regex)
      predicate, *rest_predicates = regex[:predicates]

      keys = regex[:keys]
      key, *rest_keys = keys

      return unless regex[:predicates].all?

      unless accept?(predicate)
        return H[:op.ns(self) => :pcat.ns(self),
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
      x.respond_to?(:get) && x.get(:op.ns(self)) && x
    end

    def self.accept(x)
      H[:op.ns(self) => :accept.ns(self), return_value: x]
    end

    def self.accept?(hash)
      if hash.is_a?(H)
        hash[:op.ns(self)] == :accept.ns(self)
      end
    end

    def self.reg_resolve!(key)
      if key.is_a?(Symbol)
        registry[key]
      else
        key
      end
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

      return_value = H[:op.ns(self) => :alt.ns(self), predicates: predicates, keys: keys]
      return return_value unless rest_predicates.empty?

      return predicate unless key
      return return_value unless accept?(predicate)

      accept([key, predicate[:return_value]])
    end

    def self.re_conform(regex, data)
      x, *xs = data

      if data.empty?
        return :invalid.ns(self) unless accept_nil?(regex)

        return_value = preturn(regex)

        if return_value == :nil.ns(self)
          nil
        else
          return_value
        end
      else
        dp = deriv(regex, x)

        if dp
          re_conform(dp, xs)
        else
          :invalid.ns(self)
        end
      end
    end

    def self.accept_nil?(regex)
      regex = reg_resolve!(regex)
      return unless regex?(regex)

      case regex[:op.ns(self)]
      when :accept.ns(self) then true
      when :pcat.ns(self)   then regex[:predicates].all? { |p| accept_nil?(p) }
      when :alt.ns(self)    then regex[:predicates].any? { |p| accept_nil?(p) }
      when :rep.ns(self)    then (regex[:p1] == regex[:p2]) || accept_nil?(regex[:p1])
      when :amp.ns(self)
        p1 = regex[:p1]

        return false unless accept_nil?(p1)

        no_ret?(p1, preturn(p1)) ||
          !invalid?(and_preds(preturn(p1), regex[:predicates]))
      else
        raise "Unexpected #{:op.ns(self)} #{regex[:op.ns(self)]}"
      end
    end

    def self.no_ret?(p1, pret)
      return true if pret == :nil.ns(self)

      regex = reg_resolve!(p1)
      op = regex[:op.ns(self)]

      [:rep.ns(self), :pcat.ns(self)].include?(op) && pret.empty? || nil
    end

    def self.and_preds(x, preds)
      preds.each do |pred|
        x = dt(pred, x)

        return :invalid.ns(self) if invalid?(x)
      end

      x
    end

    def self.preturn(regex)
      regex = reg_resolve!(regex)
      return unless regex?(regex)

      p0, *pr = regex[:predicates]
      k, *ks = regex[:keys]

      case regex[:op.ns(self)]
      when :accept.ns(self) then regex[:return_value]
      when :pcat.ns(self)   then add_ret(p0, regex[:return_value], k)
      when :rep.ns(self)    then add_ret(regex[:p1], regex[:return_value], k)
      when :amp.ns(self)
        pret = preturn(regex[:p1])
        if no_ret?(regex[:p1], pret)
          :nil.ns(self)
        else
          and_preds(pret, regex[:predicates])
        end
      when :alt.ns(self)
        ps, ks = filter_alt(regex[:predicates], regex[:keys], &method(:accept_nil?))

        r = if ps.first.nil?
              :nil.ns(self)
            else
              preturn(ps.first)
            end

        if ks && ks.first
          V[ks.first, r]
        else
          r
        end
      else
        raise "Balls #{regex.inspect}"
      end
    end

    def self.filter_alt(ps, ks, &block)
      if ks
        pks = Array(ps).zip(ks).select { |xs| block.call(xs.first) }
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

        return invalid?(return_value) ? return_value : accept(return_value)
      end

      regex = predicate

      predicates, p1, p2, keys, return_value, splice =
        regex.values_at(:predicates, :p1, :p2, :keys, :return_value, :splice)

      pred, *rest_preds = predicates
      key, *rest_keys = keys

      case regex[:op.ns(self)]
      when :accept.ns(self) then nil
      when :pcat.ns(self)
        regex1 = pcat(H[predicates: [deriv(pred, value), *rest_preds], keys: keys, return_value: return_value])
        regex2 = nil

        if accept_nil?(pred)
          regex2 = deriv(
            pcat(H[predicates: rest_preds, keys: rest_keys, return_value: add_ret(pred, return_value, key)]),
            value
          ) 
        end

        alt2(regex1, regex2)
      when :alt.ns(self)
        _alt(predicates.map { |p| deriv(p, value) }, keys)
      when :rep.ns(self)
        regex1 = rep(deriv(p1, value), p2, return_value, splice)
        regex2 = nil

        if accept_nil?(p1)
          regex2 = deriv(rep(p2, p2, add_ret(p1, return_value, nil), splice), value)
        end

        alt2(regex1, regex2)
      when :amp.ns(self)
        p1 = deriv(p1, value)
        return unless p1

        if p1[:op.ns(self)] == :accept.ns(self)
          ret = and_preds(preturn(p1), predicates)
          accept(ret) unless invalid?(ret)
        else
          constrained(p1, *predicates)
        end
      else
        raise "Unexpected #{:op.ns(self)} #{regex[:op.ns(self)]}"
      end
    end

    def self.dt(pred, x)
      return x unless pred

      spec = the_spec(pred)

      if spec
        spec.conform(x)
      else
        if pred.is_a?(Class) || pred.is_a?(Proc) || pred.is_a?(::Regexp)
          pred === x ? x : :invalid.ns(self)
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
      spec = ((Symbol === spec_or_key) && reg_resolve!(spec_or_key)) ||
        spec?(spec_or_key) ||
        regex?(spec_or_key) ||
        nil

      if regex?(spec)
        RegexSpec.new(spec)
      else
        spec
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

      case regex[:op.ns(self)]
      when :accept.ns(self), :alt.ns(self), :amp.ns(self)
        return_value = preturn(regex)

        if return_value == :nil.ns(self)
          r
        else
          r.conj(key ? H[key => return_value] : return_value)
        end
      when :pcat.ns(self), :rep.ns(self) then prop.call
      else
        raise "Unexpected #{:op.ns(self)} #{regex[:op.ns(self)]}"
      end
    end
  end
end
