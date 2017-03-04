# frozen_string_literal: true
require "concurrent"

module Speculation
  # @private
  module Pmap
    if RUBY_PLATFORM == "java"
      def pmap(coll, &block)
        Pmap.pmap_jruby(coll, &block)
      end
    else
      def pmap(coll, &block)
        coll.map(&block)
      end
    end

    def self.pmap_jruby(coll, &block)
      thread_count = [1, Concurrent.processor_count - 1].max
      pool = Concurrent::FixedThreadPool.new(thread_count, :auto_terminate  => true,
                                                           :fallback_policy => :abort)

      coll.
        map { |x| Concurrent::Future.execute(:executor => pool) { block.call(x) } }.
        map { |f| f.value || f.reason }
    ensure
      pool.shutdown
    end
  end
end
