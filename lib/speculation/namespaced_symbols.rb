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

          def namespaced?
            to_s.include?("/")
          end

          def unnamespaced
            to_s.split("/").last.to_sym
          end
        end
      end
    end
  end
end
