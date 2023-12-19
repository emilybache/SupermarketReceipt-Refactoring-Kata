defprotocol Supermarket.Model.SupermarketCatalog do
  def add_product(catalog, product, price)
  def get_unit_price(catalog, product)
end
