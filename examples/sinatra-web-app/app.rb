require 'bundler/setup'
require 'sinatra'
require 'sequel'
require 'bcrypt'
require 'sqlite3'
require 'speculation'
require 'speculation/gen'
require 'json'

S = Speculation
Gen = S::Gen
DB = Sequel.sqlite

DB.create_table :users do
  primary_key :id
  String :username
  String :email
  String :hashed_password
end

post '/users' do
  user_attrs = symbolize_keys(params["user"])

  if User.valid?(user_attrs)
    User.create!(DB, user_attrs)
  else
    User.serialize_validation_errors(user_attrs).to_json
  end
end

get '/users' do
  users = User.all(DB)
  users.to_json
end

get '/fake-user' do
  User.fake
end

def symbolize_keys(hash)
  hash.map { |k, v| [k.to_sym, v] }.to_h
end

module User
  extend Speculation::NamespacedSymbols

  def self.valid?(user)
    S.valid?(ns(:user), user)
  end

  def self.create!(db, user)
    hashed_password = BCrypt::Password.create(user[:password])

    db[:users].insert(:email           => user[:email],
                      :username        => user[:username],
                      :hashed_password => hashed_password)

    "success!"
  end

  def self.all(db)
    db[:users].all
  end

  def self.serialize_validation_errors(user)
    data = S.explain_data(ns(:user), user)
    data[:problems].map { |problem| Validation.serialize_problem(problem) }
  end

  def self.fake
    Gen.generate(S.gen(ns(User, :user))).to_json
  end

  module Generators
    def self.string(char_gen, opts)
      Radagen.fmap(Radagen.array(char_gen, opts), &:join)
    end

    def self.email
      local_part_gen = string(Radagen.char_alphanumeric, :min => 1, :max => 64)
      subdomain_gen = string(Radagen.char_alphanumeric, :min => 1, :max => 10)
      tld_gen = string(Radagen.char_alpha, :count => 3)

      Radagen.fmap(Radagen.tuple(local_part_gen, subdomain_gen, tld_gen)) { |local_part, subdomain, tld|
        "#{local_part}@#{subdomain}.#{tld}"
      }
    end

    def self.username
      string(Radagen.char, :min => 5, :max => 20)
    end

    def self.string_within_codepoints(lower, upper, opts)
      Radagen.fmap(Radagen.array(Radagen.choose(lower, upper), opts)) { |codepoints|
        codepoints.map(&:chr).join
      }
    end

    def self.password
      uppercase = string_within_codepoints(65, 90, :min => 2, :max => 5)
      lowercase = string_within_codepoints(97, 122, :min => 2, :max => 5)
      digits = string_within_codepoints(48, 57, :min => 2, :max => 5)
      punctuation = Radagen.elements(%w[, . ? : ; ' " ! @])

      gen = Radagen.bind(Radagen.tuple(uppercase, lowercase, digits, punctuation)) { |*genned|
        Radagen.fmap(Radagen.shuffle(genned.join.split("")), &:join)
      }
    end
  end

  module Validation
    def self.serialize_problem(problem)
      path = problem[:path]
      predicate, args = problem[:pred]

      message = USER_ERROR_MESSAGE_MAP.fetch(path).fetch(predicate)
      message = message.call(args) if message.respond_to?(:call)

      { :error => message }
    end

    def self.validate_username_length(username)
      username.length.between?(5, 20)
    end

    def self.validate_password_length(password)
      password.length.between?(8, 50)
    end

    def self.validate_password_complexity(password)
      [/[A-Z]/, /[a-z]/, /\d/, /\W/].all? { |re| password.match(re) }
    end

    EMAIL_REGEX = /^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$/

    USER_ERROR_MESSAGE_MAP = {
      [] => {
        S::Predicates.method(:key?) => ->(args) {
          key = args.first
          if key == S.or_keys(:email, :username)
            "email or username is required"
          else
            "#{key} is required"
          end
        }
      },
      [:username] => {
        String                            => "username must be a string",
        method(:validate_username_length) => "username must be between 5 and 20 characters",
      },
      [:email] => {
        String      => "email must be a string",
        EMAIL_REGEX => "email must be a valid email address"
      },
      [:password] => {
        String                                => "password must be a string",
        method(:validate_password_length)     => "password must be between 8 and 50 characters",
        method(:validate_password_complexity) => "password must contain at least one of each: upper case, lower case, numeric and special characters"
      }
    }
  end

  S.def ns(:email), S.with_gen(S.and(String, Validation::EMAIL_REGEX)) { Generators.email }
  S.def ns(:username), S.with_gen(S.and(String, Validation.method(:validate_username_length))) { Generators.username }
  S.def ns(:password), S.with_gen(S.and(String, Validation.method(:validate_password_length), Validation.method(:validate_password_complexity))) { Generators.password }
  S.def ns(:user), S.keys(:req_un => [S.or_keys(ns(:email), ns(:username)), ns(:password)])
end
