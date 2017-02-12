# Speculation

A Ruby port of Clojure's `clojure.spec`. See [clojure.spec - Rationale and Overview](https://clojure.org/about/spec).

## Installation

Add this line to your application's Gemfile:

```ruby
gem 'speculation'
```

And then execute:

    $ bundle

Or install it yourself as:

    $ gem install speculation

## Usage

The API is more-or-less the same as `clojure.spec`.

## Development

After checking out the repo, run `bin/setup` to install dependencies. Then, run `rake` to run rubocop and the tests. You can also run `bin/console` for an interactive prompt that will allow you to experiment.

## Contributing

Bug reports and pull requests are welcome on GitHub at https://github.com/english/speculation.

## TODO

- tidy up tests
- write up comparison with clojure.spec
- write guide

### clojure.spec features

- [`unform`](https://clojuredocs.org/clojure.spec/unform)
- [`form`](https://clojuredocs.org/clojure.spec/form)
- [`abbrev`](https://clojuredocs.org/clojure.spec/abbrev)
- [`describe`](https://clojuredocs.org/clojure.spec/describe)

### Improvements

- Find/build an alternative to Rantly
- Profile and optimise

## License

The gem is available as open source under the terms of the [MIT License](http://opensource.org/licenses/MIT).
