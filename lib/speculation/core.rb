require 'concurrent/atom'
require 'concurrent/delay'
require 'hamster/hash'
require 'hamster/vector'
require 'functional'

module Speculation
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

  module NS
    refine Symbol do
      def ns(mod)
        :"#{mod}/#{self}"
      end
    end
  end
end

module Speculation
  using Speculation::Specize
  using Speculation::NS

  H = Hamster::Hash
  V = Hamster::Vector
  Protocol = Functional::Protocol

  module Core
    REGISTRY = Concurrent::Atom.new(H[])

    Functional.SpecifyProtocol(:Spec.ns(self)) do
      instance_method :conform, 1
    end

    class PredicateSpec
      def initialize(predicate)
        @predicate = predicate
      end

      def conform(value)
        # calling #=== here so that a either a class or proc can be provided
        @predicate === value ? value : :invalid.ns(Core)
      end
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
    end

    class RegexSpec
      def initialize(regex)
        @regex = regex
      end

      def conform(value)
        if value.nil? or value.respond_to?(:each)
          Core.re_conform(@regex, Array(value))
        else
          :invalid.ns(Core)
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
        the_spec(pred)
      else
        PredicateSpec.new(pred)
      end
    end

    def self.spec?(spec)
      spec if Protocol.Satisfy?(spec, :Spec.ns(self))
    end

    def self.reset_registry!
      registry.reset(H[])
    end

    def self.conform(spec, value)
      spec = specize(spec)
      spec = RegexSpec.new(spec) if regex?(spec)

      spec.conform(value)
    end

    def self.valid?(spec, value)
      spec = specize(spec)
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

      return_value = if keys # any?
                       if regex[:return_value].respond_to?(:add)
                         regex[:return_value].add(H[key => predicate[:return_value]])
                       else
                         regex[:return_value].store(key, predicate[:return_value])
                       end
                     else
                       if regex[:return_value].respond_to?(:add)
                         regex[:return_value].add(predicate[:return_value])
                       else
                         regex[:return_value].merge(predicate[:return_value])
                       end
                     end

      if rest_predicates
        pcat(H[predicates: rest_predicates,
               keys: rest_keys,
               return_value: return_value])
      else
        accept(return_value)
      end
    end

    def self.regex?(x)
      x.respond_to?(:get) and x.get(:op.ns(self)) and x
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
        registry.value[key]
      else
        key
      end
    end

    ### private ###

    def self.specize(spec)
      spec?(spec) or spec.specize
    end

    def self.alt2(p1, p2)
      if p1 and p2
        _alt([p1, p2], nil)
      else
        p1 or p2
      end
    end

    def self._alt(predicates, keys)
      predicates, keys = filter_alt(predicates, keys, -> (x) { x })
      return unless predicates

      predicate, *rest_predicates = predicates
      key, *rest_keys = keys

      return_value = H[:op.ns(self) => :alt.ns(self), predicates: predicates, keys: keys]
      return return_value unless rest_predicates.empty?

      return predicate unless key
      return return_value unless accept?(predicate)

      accept([key, predicate[:return_value]])
    end

    def self.re_conform(p, data)
      x, *xs = data

      if data.empty?
        return :invalid.ns(self) unless accept_nil?(p)

        return_value = preturn(p)

        if return_value == :nil.ns(self)
          nil
        else
          return_value
        end
      else
        dp = deriv(p, x)
        if dp
          re_conform(dp, xs)
        else
          :invalid.ns(self)
        end
      end
    end

    def self.accept_nil?(p)
      p = reg_resolve!(p)
      return unless regex?(p)

      case p[:op.ns(self)]
      when :accept.ns(self) then true
      when :pcat.ns(self)   then p[:predicates].all? { |p| accept_nil?(p) }
      when :alt.ns(self)    then p[:predicates].any? { |p| accept_nil?(p) }
      when :rep.ns(self)    then (p[:p1] == p[:p2]) or accept_nil?(p[:p1])
      else
        raise "Balls #{p.inspect}"
      end
    end

    def self.preturn(p)
      p = reg_resolve!(p)
      return unless regex?(p)

      p0, *pr = p[:predicates]
      k, *ks = p[:keys]

      case p[:op.ns(self)]
      when :accept.ns(self) then p[:return_value]
      when :pcat.ns(self)   then add_ret(p0, p[:return_value], k)
      when :rep.ns(self)    then add_ret(p[:p1], p[:return_value], k)
      when :alt.ns(self)
        ps, ks = filter_alt(p[:predicates], p[:keys], method(:accept_nil?))

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
        raise "Balls #{p.inspect}"
      end
    end

    def self.filter_alt(ps, ks, f)
      if ks
        pks = Array(ps).zip(ks).select { |xs| f.call(xs.first) }
        [pks.map(&:first), pks.map(&:last)]
      else
        [ps.select(&f), ks]
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

      case predicate[:op.ns(self)]
      when :accept.ns(self) then nil
      when :pcat.ns(self)
        return_value = predicate[:return_value]

        pred, *rest_preds = predicate[:predicates]

        keys = predicate[:keys]
        key, *rest_keys = keys

        alt2(
          pcat(H[predicates: [deriv(pred, value), *rest_preds],
                 keys: keys, return_value: return_value]),
          (deriv(pcat(H[predicates: rest_preds, keys: rest_keys, return_value: add_ret(pred, return_value, key)]), value) if accept_nil?(pred))
        )
      when :alt.ns(self)
        _alt(predicate[:predicates].map { |p| deriv(p, value) }, predicate[:keys])
      when :rep.ns(self)
        alt2(
          rep(deriv(predicate[:p1], value), predicate[:p2], predicate[:return_value], predicate[:splice]),
          (deriv(rep(predicate[:p2], predicate[:p2], add_ret(predicate[:p1], predicate[:return_value], nil), value)) if accept_nil?(predicate[:p1]))
        )
      else
        raise "Balls #{predicate.inspect}, #{value}"
      end
    end

    def self.dt(pred, x)
      return x unless pred

      spec = the_spec(pred)

      if spec
        spec.conform(x)
      else
        if pred.respond_to?(:conform)
          pred.conform(x)
        else
          pred === x ? x : :invalid.ns(self)
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
      spec = ((Symbol === spec_or_key) and reg_resolve!(spec_or_key)) or
        spec?(spec_or_key) or
        regex?(spec_or_key) or
        nil

      if regex?(spec)
        RegexSpec.new(spec)
      else
        spec
      end
    end

    def self.add_ret(p, r, k)
      p = reg_resolve!(p)
      return r unless regex?(p)

      prop = -> do
        return_value = preturn(p)

        if return_value.empty?
          r
        else
          if p[:splice]
            if k
              r + H[k => return_value]
            else
              r + return_value
            end
          else
            if k
              r.merge(k => return_value)
            else
              r.add(return_value)
            end
          end
        end
      end

      case p[:op.ns(self)]
      when :accept.ns(self), :alt.ns(self)
        return_value = preturn(p)

        if return_value == :nil.ns(self)
          r
        else
          if k
            r.merge(k => return_value)
          else
            r.add(return_value)
          end
        end
      when :pcat.ns(self), :rep.ns(self) then prop.call
      else
        raise "Balls #{p.inspect}"
      end
    end
  end
end
