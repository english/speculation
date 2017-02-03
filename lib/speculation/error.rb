require 'pp'

module Speculation
  class Error < StandardError
    attr_reader :data

    def initialize(message, data)
      super(message)
      @data = data.merge(:cause => message)
    end

    def to_s
      PP.pp(@data, String.new)
    end
  end
end
