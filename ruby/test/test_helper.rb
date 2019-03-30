require 'minitest/autorun'

pattern = File.join(File.dirname(__FILE__), '..', 'lib', '**', '*.rb')
Dir[pattern].each { |filepath| require_relative filepath }

require_relative './fake_catalog'
