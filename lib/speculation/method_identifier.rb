# frozen_string_literal: true

module Speculation
  # @private
  class MethodIdentifier
    attr_reader :namespace, :name

    def initialize(namespace, name, is_instance_method)
      @namespace = namespace
      @name = name
      @is_instance_method = is_instance_method
    end

    def instance_method?
      @is_instance_method
    end

    def get_method
      @is_instance_method ? @namespace.instance_method(@name) : @namespace.method(@name)
    end

    def redefine_method!(new_method)
      if @is_instance_method
        name = @name
        @namespace.class_eval { define_method(name, new_method) }
      else
        @namespace.define_singleton_method(@name, new_method)
      end
    end

    def hash
      [@namespace, @name, @is_instance_method].hash
    end

    def ==(other)
      self.class === other &&
        other.hash == hash
    end
    alias eql? ==

    def to_s
      sep = @is_instance_method ? "#" : "."
      "#{@namespace}#{sep}#{@name}"
    end
    alias inspect to_s
  end
end
