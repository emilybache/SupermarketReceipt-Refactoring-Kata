class Discount

  attr_reader :product, :description, :discount_amount

  def initialize(product, description, discount_amount)
    @product = product
    @description = description
    @discount_amount = discount_amount
  end

end
