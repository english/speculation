# coding: utf-8
lib = File.expand_path('../lib', __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require 'speculation/version'

Gem::Specification.new do |spec|
  spec.name          = "speculation"
  spec.version       = Speculation::VERSION
  spec.authors       = ["Jamie English"]
  spec.email         = ["jamienglish@gmail.com"]

  spec.summary       = %q{Ruby port of clojure.spec}
  spec.description   = %q{Ruby port of clojure.spec}
  spec.homepage      = "TODO: Put your gem's website or public repo URL here."
  spec.license       = "MIT"

  spec.files         = `git ls-files -z`.split("\x0").reject do |f|
    f.match(%r{^(test|spec|features)/})
  end
  spec.bindir        = "exe"
  spec.executables   = spec.files.grep(%r{^exe/}) { |f| File.basename(f) }
  spec.require_paths = ["lib"]

  spec.add_dependency "hamster", "~> 3.0.0"
  spec.add_dependency "concurrent-ruby", "~> 1.0.2"

  spec.add_development_dependency "bundler", "~> 1.13"
  spec.add_development_dependency "rake", "~> 10.0"
  spec.add_development_dependency "minitest", "~> 5.0"
  spec.add_development_dependency "pry", "~> 0.10"
  spec.add_development_dependency "pry-byebug", "~> 3.4.2"
  spec.add_development_dependency "pry-state", "~> 0.1.8"
  spec.add_development_dependency "pry-inline", "~> 1.0.2"
  spec.add_development_dependency "pry-doc", "~> 0.9.0"
end
