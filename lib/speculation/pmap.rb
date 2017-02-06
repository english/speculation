# frozen_string_literal: true
require "concurrent"

module Speculation
  module Pmap
    refine Array do
      if RUBY_PLATFORM == "java"
        def pmap(&block)
          Pmap.pmap_jruby(self, &block)
        end
      else
        alias pmap map
      end
    end

    def self.pmap_jruby(array, &block)
      pool = Concurrent::FixedThreadPool.new(
        [1, Concurrent.processor_count - 1].max,
        :auto_terminate  => true,
        :fallback_policy => :abort)

      array.
        map { |x| Concurrent::Future.execute(:executor => pool) { block.call(x) } }.
        map { |f| f.value || f.reason }
    ensure
      pool.shutdown
    end
  end
end
