class ProductQuantity

  attr_reader :product, :quantity

  def initialize(product, weight)
    @product = product
    @quantity = weight
  end

end
