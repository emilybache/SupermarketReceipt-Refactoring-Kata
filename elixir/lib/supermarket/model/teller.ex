defmodule Supermarket.Model.Teller do
  alias Supermarket.Model.SupermarketCatalog
  alias Supermarket.Model.ShoppingCart
  alias Supermarket.Model.Receipt

  defstruct [:catalog, :offers]

  def new(catalog) do
    %__MODULE__{catalog: catalog, offers: %{}}
  end

  def checks_out_articles_from(teller, the_cart) do
    receipt = Receipt.new()
    product_quantities = the_cart.items

    receipt =
      Enum.reduce(product_quantities, receipt, fn pq, receipt ->
        p = pq.product
        quantity = pq.quantity
        unit_price = SupermarketCatalog.get_unit_price(teller.catalog, p)
        price = quantity * unit_price
        Receipt.add_product(receipt, p, quantity, unit_price, price)
      end)

    ShoppingCart.handle_offers(the_cart, receipt, teller.offers, teller.catalog)
  end

  @java """
  package dojo.supermarket.model;

  import java.util.HashMap;
  import java.util.List;
  import java.util.Map;

  public class Teller {

      private final SupermarketCatalog catalog;
      private final Map<Product, Offer> offers = new HashMap<>();

      public Teller(SupermarketCatalog catalog) {
          this.catalog = catalog;
      }

      public void addSpecialOffer(SpecialOfferType offerType, Product product, double argument) {
          offers.put(product, new Offer(offerType, product, argument));
      }

      public Receipt checksOutArticlesFrom(ShoppingCart theCart) {
          Receipt receipt = new Receipt();
          List<ProductQuantity> productQuantities = theCart.getItems();
          for (ProductQuantity pq: productQuantities) {
              Product p = pq.getProduct();
              double quantity = pq.getQuantity();
              double unitPrice = catalog.getUnitPrice(p);
              double price = quantity * unitPrice;
              receipt.addProduct(p, quantity, unitPrice, price);
          }
          theCart.handleOffers(receipt, offers, catalog);

          return receipt;
      }
  }
  """
end
