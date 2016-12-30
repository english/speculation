module Speculation
  def self.namespaced_symbols(namespace)
    Module.new do
      refine Symbol do
        define_method(:ns) do |x = nil|
          if x
            :"#{x}/#{self}"
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

require "speculation/version"
require "speculation/core"
