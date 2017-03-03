# frozen_string_literal: true

module Speculation
  module NamespacedSymbols
    def self.refine(namespace)
      Module.new do
        refine Symbol do
          define_method(:ns) do |mod = nil|
            mod ||= namespace
            NamespacedSymbols.symbol(mod, self)
          end

          def name
            NamespacedSymbols.name(self)
          end

          def namespace
            NamespacedSymbols.namespace(self)
          end
        end
      end
    end

    def self.symbol(ns, name)
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
