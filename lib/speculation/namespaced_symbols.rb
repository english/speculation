# frozen_string_literal: true

module Speculation
  module NamespacedSymbols
    # @param [#to_s] namespace
    # @param [#to_s] name
    # @return [Symbol] concatenation of `namespace` and `name`
    # @example
    #     ns(Foo::Bar, :foo)
    #     # => :"Foo::Bar/baz"
    def ns(name_or_namespace, name = nil)
      if name
        namespace = name_or_namespace
      else
        name = name_or_namespace
        namespace = is_a?(Module) ? self : self.class
      end

      NamespacedSymbols.symbol(namespace, name)
    end

    def self.symbol(ns, name)
      ns = ns.name if ns.is_a?(Module)

      :"#{ns}/#{name}"
    end

    def self.name(sym)
      sym.to_s.split("/").last
    end

    def self.namespace(sym)
      parts = sym.to_s.split("/")
      parts.first if parts.count == 2
    end
  end
end
