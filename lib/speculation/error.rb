# frozen_string_literal: true
require "pp"

module Speculation
  class Error < StandardError
    attr_reader :data, :message

    def initialize(message, data)
      super(message)
      @data = data
      @message = message
    end

    def to_s
      "#{@message} #{PP.pp(@data, String.new)}"
    end
  end
end
