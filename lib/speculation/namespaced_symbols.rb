# frozen_string_literal: true

module Speculation
  module NamespacedSymbols
    def self.refine(namespace)
      Module.new do
        refine Symbol do
          define_method(:ns) do |mod = nil|
            if mod
              :"#{mod}/#{self}"
            else
              :"#{namespace}/#{self}"
            end
          end

          def name
            to_s.split("/").last
          end

          def namespace
            parts = to_s.split("/")
            parts.first if parts.count == 2
          end
        end
      end
    end
  end
end
