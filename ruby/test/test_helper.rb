if ENV["COVERAGE"] == "yes"
  require 'simplecov'
  SimpleCov.start
end

require 'minitest/autorun'
require 'mutant/minitest/coverage'

require_relative File.join(File.dirname(__FILE__), '..', 'lib', 'kata.rb')

pattern = File.join(File.dirname(__FILE__), 'support', '**', '*.rb')
Dir[pattern].each { |filepath| require_relative filepath }
