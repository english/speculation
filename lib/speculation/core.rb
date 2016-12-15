require 'concurrent/atom'
require 'concurrent/delay'
require 'hamster/hash'
require 'hamster/vector'

module Speculation
  Hash = Hamster::Hash
  Vector = Hamster::Vector

  module Core
    REGISTRY = Concurrent::Atom.new(Hash[])

    class Spec
      attr_writer :name
      def [](k)
        nil
      end
    end

    class PredicateSpec < Spec
      def initialize(predicate)
        @predicate = predicate
      end

      def conform(value)
        if @predicate.call(value)
          value
        else
          :"Speculation::Core/invalid"
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

          if Speculation::Core.invalid?(value)
            return :"Speculation::Core/invalid"
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

          unless Speculation::Core.invalid?(conformed)
            return [@keys[index], value]
          end
        end

        :"Speculation::Core/invalid"
      end
    end

    class RegexpSpec < Spec
      def initialize(regexp)
        @regexp = regexp
      end

      def conform(value)
        if value.nil? or value.respond_to?(:each)
          re_conform(@regexp, value.each.to_a)
        else
          :"Speculation::Core/invalid"
        end
      end

      private

      def re_conform(p, data)
        x, *xs = data

        if data.empty?
          if accept_nil?(p)
            ret = preturn(p)

            if ret == :"Speculation::Core/invalid"
              nil
            else
              ret
            end
          else
            :"Speculation::Core/invalid"
          end
        else
          dp = deriv(p, x)

          if dp
            re_conform(dp, xs)
          else
            :"Speculation::Core/invalid"
          end
        end
      end

      def accept_nil?(p)
        p = Speculation::Core.reg_resolve!(p)

        case p[:op]
        when :"Speculation::Core/accept" then true
        when nil then nil
        # when :"Speculation::Core/amp" nooop
        when :"Speculation::Core/pcat" then p[:ps].all? { |p| accept_nil?(p) }
        else raise "Balls #{}"
        end
      end

      def preturn(p)
        p = Speculation::Core.reg_resolve!(p)
        p0, *pr = p[:ps]
        k, *ks = [:keys]

        case p[:op]
        when :"Speculation::Core/accept" then p[:ret]
        when nil then nil
        when :"Speculation::Core/pcat" then add_ret(p[:p1], p[:ret], k)
        end
      end

      def deriv(p, x)
        p = Speculation::Core.reg_resolve!(p)
        return unless p

        case p[:op]
        when :"Speculation::Core/accept" then nil
        when nil
          ret = dt(p, x)
          if !Speculation::Core.invalid?(ret)
            Speculation::Core.accept(ret)
          end
        when :"Speculation::Core/pcat"
          ret = p[:ret]

          ps = p[:ps]
          p0, *pr = ps

          ks = p[:ks]
          k0, *kr = ks

          Speculation::Core.alt2(
            Speculation::Core.pcat(Hash[ps: Vector[deriv(p0, x), *pr], ks: ks, ret: ret]),
            (accept_nil?(p0) and deriv(pcat(Hash[ps: pr, ks: kr, ret: add_ret(p0, ret, k0)]), x))
          )
        end
      end

      def dt(spec, x)
        if spec
          spec = Speculation::Core.reg_resolve!(spec)
          spec.conform(x)
        else
          x
        end
      end

      def add_ret(p, r, k)
        p = Speculation::Core.reg_resolve!(p)
        prop = -> do
          ret = preturn(p)
          if ret.empty?
            r
          else
            if p[:splice]
              if k
                r.add(Hash[k => ret])
              else
                r.add(ret)
              end
            else
              if k
                r.merge(k, ret)
              else
                r.add(ret)
              end
            end
          end
        end

        op = p && p[:op]

        case op
        when nil then r
        when :"Speculation::Core/accept"
          ret = preturn(p)
          if ret == :nil
            r
          else
            if k
              r.merge(k, ret)
            else
              r.add(ret)
            end
          end
        when :"Speculation::Core/pcat"
          prop.call
        end
      end
    end

    def self.registry
      REGISTRY
    end

    def self.def(name, spec)
      unless spec.is_a?(Spec)
        # More cases here!
        spec = PredicateSpec.new(spec)
      end

      spec.name = name

      registry.swap { |reg| reg.put(name, spec) }

      name
    end

    def self.reset_registry!
      registry.swap { Hash[] }
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
      value.equal?(:"Speculation::Core/invalid")
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

      regexp = pcat(Hash[ks: keys, ps: predicates, ret: Hash[]])
      RegexpSpec.new(regexp)
    end

    ######## crazy shit ########

    def self.pcat(hash)
      ps = hash[:ps]
      p1, *pr = ps

      ks = hash[:ks]
      k1, *kr = ks

      ret = hash[:ret]
      rep_plus = hash[:rep_plus]

      return unless ps.all?

      unless accept?(p1)
        return Hash[op: :"Speculation::Core/pcat", ps: ps, ret: ret, ks: ks, rep_plus: rep_plus]
      end

      rp = p1[:ret]
      ret = if ks # any?
              ret.put(k1, rp)
            else
              ret.merge(rp)
            end

      if pr
        pcat(Hash[ps: pr, ks: kr, ret: ret])
      else
        accept(ret)
      end
    end

    def regex?(x)
      x[:op] and x
    end

    def self.accept(x)
      Hash[op: :"Speculation::Core.accept", ret: x]
    end

    def self.accept?(hash)
      if hash.is_a?(Hash)
        hash[:op] == :"Speculation::Core.accept"
      end
    end

    def self.reg_resolve!(key)
      if key.is_a?(Symbol)
        registry.value.fetch(key)
      else
        key
      end
    end

    ### private ###

    def self.specize(spec)
      case spec
      when Speculation::Core::Spec then spec
      when Symbol then reg_resolve!(spec)
      else
        if spec.respond_to?(:call)
          PredicateSpec.new(spec)
        else
          raise ArgumentError,
            "spec: #{spec} must be a Spec, Symbol or callable, given #{spec.class}"
        end
      end
    end

    def self.alt2(p1, p2)
      if p1 and p2
        alt([p1, p2], nil)
      else
        p1 or p2
      end
    end

    def self.alt(ps, ks)
      return unless ps

      ret = Hash[op: :"Speculation::Core/alt", ps: ps, ks: ks]
      return ret if pr.nil?
      return p1 unless k1
      return ret unless accept?(p1)

      accept([k1, p1[:ret]])
    end
  end
end
