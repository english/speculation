# coding: utf-8
# frozen_string_literal: true
lib = File.expand_path("../lib", __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require "speculation/version"

Gem::Specification.new do |spec|
  spec.name          = "speculation"
  spec.version       = Speculation::VERSION
  spec.authors       = ["Jamie English"]
  spec.email         = ["jamienglish@gmail.com"]

  spec.summary       = %q{Ruby port of clojure.spec}
  spec.description   = %q{Ruby port of clojure.spec}
  spec.homepage      = "https://github.com/english/speculation"
  spec.license       = "MIT"

  spec.files         = `git ls-files -z`.split("\x0").reject do |f|
    f.match(%r{^(test|spec|features)/})
  end
  spec.bindir        = "exe"
  spec.executables   = spec.files.grep(%r{^exe/}) { |f| File.basename(f) }
  spec.require_paths = ["lib"]

  spec.add_dependency "concurrent-ruby", "~> 1.0"
  spec.add_dependency "radagen", "~> 0.3.5"

  spec.add_development_dependency "bundler", "~> 1.13"
  spec.add_development_dependency "rake", "~> 10.0"
  spec.add_development_dependency "yard", "~> 0.9"
  spec.add_development_dependency "minitest", "~> 5.0"
  spec.add_development_dependency "pry", "~> 0.10"

  if RUBY_PLATFORM == "java"
    spec.add_development_dependency "pry-nav", "~> 0.2"
  else
    spec.add_development_dependency "pry-byebug", "~> 3.4"
  end

  spec.add_development_dependency "pry-doc", "~> 0.9"

  if Gem::Version.new(RUBY_VERSION) >= Gem::Version.new("2.1.0")
    spec.add_development_dependency "rubocop", '~> 0.51.0'
  end
end
