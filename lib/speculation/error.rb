# frozen_string_literal: true
require "pp"

module Speculation
  class Error < StandardError
    attr_reader :data

    def initialize(message, data)
      super(message)
      @data = data.merge(:cause => message)
    end

    def to_s
      PP.pp(@data, "")
    end
  end
end
