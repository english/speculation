# frozen_string_literal: true

require "concurrent"

module Speculation
  # @private
  module Pmap
    refine Set do
      if RUBY_PLATFORM == "java"
        def pmap(&block)
          Pmap.pmap_jruby(self, &block)
        end
      else
        alias_method :pmap, :map
      end
    end

    def self.pmap_jruby(array, &block)
      thread_count = [1, Concurrent.processor_count - 1].max
      pool = Concurrent::FixedThreadPool.new(thread_count, :auto_terminate  => true,
                                                           :fallback_policy => :abort)

      array.
        map { |x| Concurrent::Future.execute(:executor => pool) { block.call(x) } }.
        map { |f| f.value || f.reason }
    ensure
      pool.shutdown
    end
  end
end
