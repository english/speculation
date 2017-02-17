# frozen_string_literal: true
module Speculation
  using Speculation::NamespacedSymbols.refine(self)
  using Conj

  # @private
  class HashSpec < SpecImpl
    S = Speculation

    attr_reader :id

    def initialize(req, opt, req_un, opt_un)
      @id = SecureRandom.uuid
      @req = req
      @opt = opt
      @req_un = req_un
      @opt_un = opt_un

      req_keys     = req.flat_map(&method(:extract_keys))
      req_un_specs = req_un.flat_map(&method(:extract_keys))

      unless (req_keys + req_un_specs + opt + opt_un).all? { |s| s.is_a?(Symbol) && s.namespace }
        raise "all keys must be namespaced Symbols"
      end

      req_specs = req_keys + req_un_specs
      req_keys += req_un_specs.map(&method(:unqualify_key))

      pred_exprs = [Utils.method(:hash?)]
      pred_exprs.push(->(v) { parse_req(req, v, :itself.to_proc).empty? }) if req.any?
      pred_exprs.push(->(v) { parse_req(req_un, v, method(:unqualify_key)).empty? }) if req_un.any?

      @req_keys        = req_keys
      @req_specs       = req_specs
      @opt_keys        = opt + opt_un.map(&method(:unqualify_key))
      @opt_specs       = opt + opt_un
      @keys_pred       = ->(v) { pred_exprs.all? { |p| p.call(v) } }
      @key_to_spec_map = Hash[req_keys.concat(@opt_keys).zip(req_specs.concat(@opt_specs))]
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

    def explain(path, via, inn, value)
      unless Utils.hash?(value)
        return [{ :path => path, :pred => :hash?, :val => value, :via => via, :in => inn }]
      end

      problems = []

      if @req.any?
        failures = parse_req(@req, value, :itself.to_proc)

        failures.each do |failure_sexp|
          pred = [:key?, sexp_to_rb(failure_sexp)]
          problems << { :path => path, :pred => pred, :val => value, :via => via, :in => inn }
        end
      end

      if @req_un.any?
        failures = parse_req(@req_un, value, method(:unqualify_key))

        failures.each do |failure_sexp|
          pred = [:key?, sexp_to_rb(failure_sexp)]
          problems << { :path => path, :pred => pred, :val => value, :via => via, :in => inn }
        end
      end

      problems += value.flat_map { |(k, v)|
        next unless S.registry.key?(@key_to_spec_map[k])

        unless S.pvalid?(@key_to_spec_map.fetch(k), v)
          S.explain1(@key_to_spec_map.fetch(k), path.conj(k), via, inn.conj(k), v)
        end
      }

      problems.compact
    end

    def specize
      self
    end

    def gen(overrides, path, rmap)
      return @gen if @gen

      rmap = S.inck(rmap, @id)

      reqs = @req_keys.zip(@req_specs).
        reduce({}) { |m, (k, s)|
          m.merge(k => S.gensub(s, overrides, path.conj(k), rmap))
        }

      opts = @opt_keys.zip(@opt_specs).
        reduce({}) { |m, (k, s)|
          if S.recur_limit?(rmap, @id, path, k)
            m
          else
            m.merge(k => Gen.delay { S.gensub(s, overrides, path.conj(k), rmap) })
          end
        }

      ->(rantly) do
        count = rantly.range(0, opts.count)
        opts = opts.to_a.shuffle.take(count).to_h

        reqs.merge(opts).each_with_object({}) { |(k, spec_gen), h|
          h[k] = spec_gen.call(rantly)
        }
      end
    end

    private

    def sexp_to_rb(sexp, level = 0)
      if sexp.is_a?(Array)
        op, *keys = sexp
        rb_string = String.new

        rb_string << "(" unless level.zero?

        keys.each_with_index do |key, i|
          unless i.zero?
            rb_string << " #{op.name} "
          end

          rb_string << sexp_to_rb(key, level + 1).to_s
        end

        rb_string << ")" unless level.zero?

        rb_string
      else
        sexp
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

    def parse_req(ks, v, f)
      key, *ks = ks

      ret = if key.is_a?(Array)
              op, *kks = key
              case op
              when :or.ns
                if kks.one? { |k| parse_req([k], v, f).empty? }
                  []
                else
                  [key]
                end
              when :and.ns
                if kks.all? { |k| parse_req([k], v, f).empty? }
                  []
                else
                  [key]
                end
              else
                raise "Expected or, and, got #{op}"
              end
            elsif v.key?(f.call(key))
              []
            else
              [key]
            end

      if ks.any?
        ret + parse_req(ks, v, f)
      else
        ret
      end
    end
  end
end
