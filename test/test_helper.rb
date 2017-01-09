$LOAD_PATH.unshift File.expand_path('../../lib', __FILE__)
require 'minitest/autorun'
require 'pry'
require 'hamster'
require 'set'
require 'speculation/namespaced_symbols'
require 'speculation/identifier'
require 'speculation/utils'
require 'speculation/test'
require 'speculation/gen'
require 'speculation/core'
require 'speculation/specs'

Speculation::Test.instrument
