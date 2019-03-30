require 'minitest/autorun'

pattern = File.join(File.dirname(__FILE__), '..', 'lib', '**', '*.rb')
Dir[pattern].each { |filepath| require_relative filepath }

pattern = File.join(File.dirname(__FILE__), 'support', '**', '*.rb')
Dir[pattern].each { |filepath| require_relative filepath }
