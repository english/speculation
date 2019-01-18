# frozen_string_literal: true

# This is a Ruby translation of clojure.spec:
#   https://github.com/clojure/clojure/blob/master/src/clj/clojure/spec.clj
# All credit belongs with Rich Hickey and contributors for their original work.

module Speculation
  # @private
  class HashSpec < Spec
    include NamespacedSymbols
    S = Speculation

    attr_reader :id

    def initialize(req, opt, req_un, opt_un, gen = nil, name = nil)
      @id = SecureRandom.uuid
      @req = req
      @opt = opt
      @req_un = req_un
      @opt_un = opt_un
      @gen = gen
      @name = name

      req_keys     = req.flat_map(&method(:extract_keys))
      req_un_specs = req_un.flat_map(&method(:extract_keys))

      all_keys = req_keys + req_un_specs + opt + opt_un
      unless all_keys.all? { |s| s.is_a?(Symbol) && NamespacedSymbols.namespace(s) }
        raise "all keys must be namespaced Symbols"
      end

      req_specs = req_keys + req_un_specs
      req_keys += req_un_specs.map(&method(:unqualify_key))

      pred_exprs = [Predicates.method(:hash?)]
      pred_exprs.push(->(v) { parse_req(req, v, Utils.method(:itself)).empty? }) if req.any?
      pred_exprs.push(->(v) { parse_req(req_un, v, method(:unqualify_key)).empty? }) if req_un.any?

      @req_keys        = req_keys
      @req_specs       = req_specs
      @opt_keys        = opt + opt_un.map(&method(:unqualify_key))
      @opt_specs       = opt + opt_un
      @keys_pred       = ->(v) { pred_exprs.all? { |p| p.call(v) } }
      @key_to_spec_map = Hash[Utils.conj(req_keys, @opt_keys).zip(Utils.conj(req_specs, @opt_specs))]
    end

    def conform(value)
      return :"Speculation/invalid" unless @keys_pred.call(value)

      reg = S.registry

      value.reduce(value) do |ret, (key, v)|
        spec_name = key_to_spec_name(key)
        spec = reg[spec_name]

        if spec
          conformed_value = S.conform(spec, v)

          if S.invalid?(conformed_value)
            break :"Speculation/invalid"
          elsif conformed_value.equal?(v)
            ret
          else
            ret.merge(key => conformed_value)
          end
        else
          ret
        end
      end
    end

    def unform(value)
      reg = S.registry

      value.reduce(value) do |ret, (key, conformed_value)|
        if reg.key?(key_to_spec_name(key))
          unformed_value = S.unform(key_to_spec_name(key), conformed_value)

          if conformed_value.equal?(unformed_value)
            ret
          else
            ret.merge(key => unformed_value)
          end
        else
          ret
        end
      end
    end

    def explain(path, via, inn, value)
      unless Predicates.hash?(value)
        return [{ :path => path, :pred => [Predicates.method(:hash?), [value]], :val => value, :via => via, :in => inn }]
      end

      reg = S.registry
      problems = []

      if @req.any?
        failures = parse_req(@req, value, Utils.method(:itself))

        failures.each do |failure_sexp|
          pred = [Predicates.method(:key?), [failure_sexp]]
          problems << { :path => path, :pred => pred, :val => value, :via => via, :in => inn }
        end
      end

      if @req_un.any?
        failures = parse_req(@req_un, value, method(:unqualify_key))

        failures.each do |failure_sexp|
          pred = [Predicates.method(:key?), [failure_sexp]]
          problems << { :path => path, :pred => pred, :val => value, :via => via, :in => inn }
        end
      end

      problems += value.flat_map { |(k, v)|
        next unless reg.key?(key_to_spec_name(k))
        next if S.pvalid?(key_to_spec_name(k), v)

        S.explain1(key_to_spec_name(k), Utils.conj(path, k), via, Utils.conj(inn, k), v)
      }

      problems.compact
    end

    def with_gen(gen)
      self.class.new(@req, @opt, @req_un, @opt_un, gen, @name)
    end

    def with_name(name)
      self.class.new(@req, @opt, @req_un, @opt_un, @gen, name)
    end

    def gen(overrides, path, rmap)
      return @gen.call if @gen

      rmap = S.inck(rmap, @id)

      reqs = @req_keys.zip(@req_specs).
        reduce({}) { |m, (k, s)|
          m.merge(k => S.gensub(s, overrides, Utils.conj(path, k), rmap))
        }

      opts = @opt_keys.zip(@opt_specs).
        reduce({}) { |m, (k, s)|
          if S.recur_limit?(rmap, @id, path, k)
            m
          else
            m.merge(k => Gen.delay { S.gensub(s, overrides, Utils.conj(path, k), rmap) })
          end
        }

      if opts.count > 0
        Radagen.bind(Radagen.tuple(Radagen.choose(0, opts.count), Radagen.shuffle(opts.keys))) do |count, shuffled_opts_keys|
          key_gen_map = Hash[shuffled_opts_keys.take(count).map { |k| [k, opts[k]] }].merge(reqs)
          Radagen.hash(key_gen_map)
        end
      else
        Radagen.hash(reqs)
      end
    end

    private

    def extract_keys(symbol_or_arr)
      if symbol_or_arr.is_a?(Array)
        symbol_or_arr[1..-1].flat_map(&method(:extract_keys))
      else
        symbol_or_arr
      end
    end

    def unqualify_key(x)
      NamespacedSymbols.namespaced_name(x).to_sym
    end

    def parse_req(ks, v, f)
      key, *ks = ks

      ret = if key.is_a?(Array)
              op, *kks = key
              case op
              when :"Speculation/or"
                if kks.any? { |k| parse_req([k], v, f).empty? }
                  []
                else
                  transform_keys([key], f)
                end
              when :"Speculation/and"
                if kks.all? { |k| parse_req([k], v, f).empty? }
                  []
                else
                  transform_keys([key], f)
                end
              else
                raise "Expected Speculation/or, Speculation/and, got #{op}"
              end
            elsif v.key?(f.call(key))
              []
            else
              [f.call(key)]
            end

      if ks.any?
        ret + parse_req(ks, v, f)
      else
        ret
      end
    end

    def transform_keys(keys, f)
      keys.map { |key|
        case key
        when Array then transform_keys(key, f)
        when :"Speculation/and", :"Speculation/or" then key
        else f.call(key)
        end
      }
    end

    def key_to_spec_name(k)
      @key_to_spec_map.fetch(k, k)
    end
  end
end
