module Kata
end

pattern = File.join(File.dirname(__FILE__), 'kata', '**', '*.rb')
Dir[pattern].each { |filepath| require_relative filepath }
