module Speculation
  using Speculation::NamespacedSymbols.refine(self)
  using Conj

  class HashSpec < SpecImpl
    S = Speculation
    H = Hamster::Hash

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
end
