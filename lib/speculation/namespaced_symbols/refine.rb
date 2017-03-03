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
  end
end
