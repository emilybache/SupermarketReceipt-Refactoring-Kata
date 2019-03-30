class FakeCatalog < SupermarketCatalog

  def initialize
    @products = {}
    @prices = {}
  end

  def add_product(product, price)
    @products[product.name] = product
    @prices[product.name] = price
  end

  def unit_price(p)
    @prices.fetch(p.name)
  end

end
