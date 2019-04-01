Product = Struct.new(:name, :unit) do

  undef :name=, :unit=

end
