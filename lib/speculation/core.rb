require 'concurrent/atom'
require 'concurrent/delay'
require 'hamster/hash'
require 'hamster/vector'

module Speculation
  H = Hamster::Hash
  V = Hamster::Vector

  module Core
    REGISTRY = Concurrent::Atom.new(H[])

    def self.ns(sym)
      :"#{self}/#{sym}"
    end

    class Spec
    end

    class PredicateSpec < Spec
      def initialize(predicate)
        @predicate = predicate
      end

      def conform(value)
        if @predicate.call(value)
          value
        else
          Core.ns(:invalid)
        end
      end
    end

    class AndSpec < Spec
      def initialize(specs)
        @specs = specs
      end

      def conform(value)
        @specs.value.each do |spec|
          value = spec.conform(value)

          if Core.invalid?(value)
            return Core.ns(:invalid)
          end
        end

        value
      end
    end

    class OrSpec < Spec
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

        Core.ns(:invalid)
      end
    end

    class RegexSpec < Spec
      attr_reader :regex

      def initialize(regex)
        @regex = regex
      end

      def conform(value)
        if value.nil? or value.respond_to?(:each)
          Core.re_conform(@regex, Array(value))
        else
          Core.ns(:invalid)
        end
      end
    end

    def self.registry
      REGISTRY
    end

    def self.def(key, spec)
      spec = if spec?(spec) or regex?(spec) or registry.value[key]
               spec
             else
               self.spec(spec)
             end

      registry.swap { |reg| reg.put(key, spec) }

      key
    end

    def self.spec(pred)
      if spec?(pred)
        pred
      elsif regex?(pred)
        RegexSpec.new(pred)
      elsif pred.is_a?(Symbol)
        reg_resolve!(pred) or raise "unable to resolve spec #{pred}"
      else
        PredicateSpec.new(pred)
      end
    end

    def self.spec?(spec)
      spec if spec.is_a?(Spec)
    end

    def self.reset_registry!
      registry.reset(H[])
    end

    def self.conform(spec, value)
      spec = specize(spec)

      spec.conform(value)
    end

    def self.valid?(spec, value)
      spec = specize(spec)
      value = spec.conform(value)

      !invalid?(value)
    end

    def self.invalid?(value)
      value.equal?(ns(:invalid))
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

      regex = pcat(H[keys: keys, predicates: predicates, return_value: H[]])
      RegexSpec.new(regex)
    end

    def self.alt(kv_specs)
      _alt(kv_specs.values, kv_specs.keys)
    end

    def self.zero_or_more(predicate)
      RegexSpec.new(rep(predicate, predicate, V[], false))
    end

    def self.rep(p1, p2, return_value, splice)
      return unless p1

      regex = H[ns(:op) => ns(:rep), p2: p2, splice: splice]

      regex = if accept?(p1)
        regex.merge(p1: p2, return_value: return_value.add(p1[:return_value]))
      else
        regex.merge(p1: p1, return_value: return_value)
      end
    end

    ######## crazy shit ########

    def self.pcat(regex)
      predicates = regex[:predicates]
      predicate, *rest_predicates = predicates

      keys = regex[:keys]
      k1, *kr = keys

      return_value = regex[:return_value]

      return unless predicates.all?

      unless accept?(predicate)
        return H[ns(:op) => ns(:pcat),
                 predicates: predicates, keys: keys,
                 return_value: return_value]
      end

      return_value = if keys # any?
                       return_value.put(k1, predicate[:return_value])
                     else
                       return_value.merge(predicate[:return_value])
                     end

      if rest_predicates
        pcat(H[predicates: rest_predicates, keys: kr,
               return_value: return_value])
      else
        accept(return_value)
      end
    end

    def self.regex?(x)
      x.respond_to?(:get) and x.get(ns(:op)) and x
    end

    def self.accept(x)
      H[ns(:op) => ns(:accept), return_value: x]
    end

    def self.accept?(hash)
      if hash.is_a?(H)
        hash[ns(:op)] == ns(:accept)
      end
    end

    def self.reg_resolve!(key)
      if key.is_a?(Symbol)
        registry.value[key]
      else
        key
      end
    end

    ### private ###

    def self.specize(spec)
      case spec
      when Spec       then spec
      when Symbol     then reg_resolve!(spec)
      when Proc       then PredicateSpec.new(spec)
      else
        raise ArgumentError,
          "spec: #{spec} must be a Spec, Symbol or callable, given #{spec.class}"
      end
    end

    def self.alt2(p1, p2)
      if p1 and p2
        _alt([p1, p2], nil)
      else
        p1 or p2
      end
    end

    def self._alt(predicates, keys)
      return unless predicates

      predicate, *rest_predicates = predicates
      key, *rest_keys = keys

      return_value = H[ns(:op) => ns(:alt), predicates: predicates, keys: keys]
      return return_value if rest_predicates.empty?

      return predicate unless key
      return return_value unless accept?(predicate)

      accept([key, predicate[:return_value]])
    end

    def self.re_conform(p, data)
      x, *xs = data

      if data.empty?
        return ns(:invalid) unless accept_nil?(p)

        return_value = preturn(p)

        if return_value == ns(:invalid)
          nil
        else
          return_value
        end
      else
        dp = deriv(p, x)

        if dp
          re_conform(dp, xs)
        else
          ns(:invalid)
        end
      end
    end

    def self.accept_nil?(p)
      p = reg_resolve!(p)
      return unless regex?(p)

      case p[ns(:op)]
      when ns(:accept) then true
      when ns(:pcat) then p[:predicates].all? { |p| accept_nil?(p) }
      when ns(:alt) then p[:predicates].find { |p| accept_nil?(p) }
      when ns(:rep) then (p[:p1] == p[:p2]) or accept_nil?(p[:p1])
      else
        raise "Balls #{p.inspect}"
      end
    end

    def self.preturn(p)
      p = reg_resolve!(p)
      return unless regex?(p)

      p0, *pr = p[:predicates]
      k, *ks = p[:keys]

      case p[ns(:op)]
      when ns(:accept) then p[:return_value]
      when ns(:pcat)   then add_ret(p[:p1], p[:return_value], k)
      when ns(:rep)    then add_ret(p[:p1], p[:return_value], k)
      when ns(:alt)
        ps, ks = filter_alt(ps, ks, method(:accept_nil?))

        r = if ps.first.nil?
              ns(:nil)
            else
              preturn(ps.first)
            end
        if ks.first
          V[ks.first, r]
        else
          r
        end
      else
        raise "Balls #{p.inspect}"
      end
    end

    def self.filter_alt(ps, ks, f)
      if ks
        pks = ps.zip(ks).filter { |xs| f.call(xs.first) }
        [pks.map(&:first), pks.map(&:second)]
      else
        [ps.filter(&f), ks]
      end
    end

    def self.deriv(predicate, value)
      predicate = reg_resolve!(predicate)
      return unless predicate

      unless regex?(predicate)
        return_value = dt(predicate, value)

        if invalid?(return_value)
          return return_value
        else
          return accept(return_value)
        end
      end

      case predicate[ns(:op)]
      when ns(:accept) then nil
      when ns(:pcat)
        return_value = predicate[:return_value]

        pred, *rest_preds = predicate[:predicates]

        keys = predicate[:keys]
        key, *rest_keys = keys

        alt2(
          pcat(H[predicates: [deriv(pred, value), *rest_preds],
                 keys: keys, return_value: return_value]),
          (deriv(pcat(H[predicates: rest_preds, keys: rest_keys, return_value: add_ret(pred, return_value, key)]), value) if accept_nil?(pred))
        )
      when ns(:alt)
        _alt(predicate[:predicates].map { |p| deriv(p, value) }, predicate[:keys])
      when ns(:rep)
        alt2(
          rep(deriv(predicate[:p1], value), predicate[:p2], predicate[:return_value], predicate[:splice]),
          (deriv(rep(predicate[:p2], predicate[:p2], add_ret(predicate[:p1], predicate[:return_value], nil), value)) if accept_nil?(predicate[:p1]))
        )
      else
        raise "Balls #{predicate.inspect}, #{value}"
      end
    end

    def self.dt(spec, x)
      return x unless spec

      if spec.respond_to?(:conform)
        spec.conform(x)
      else
        spec === x ? x : ns(:invalid)
      end
    end

    def self.add_ret(p, r, k)
      p = reg_resolve!(p)

      prop = -> do
        return_value = preturn(p)

        if return_value.empty?
          r
        else
          if p[:splice]
            if k
              r.add(H[k => return_value])
            else
              r.add(return_value)
            end
          else
            if k
              r.merge(k, return_value)
            else
              r.add(return_value)
            end
          end
        end
      end

      return r unless regex?(p)

      case p[ns(:op)]
      when ns(:accept)
        return_value = preturn(p)

        if return_value == ns(:nil)
          r
        else
          if k
            r.merge(k, return_value)
          else
            r.add(return_value)
          end
        end
      when ns(:pcat)
        prop.call
      else
        raise "Balls #{p.inspect}"
      end
    end
  end
end
