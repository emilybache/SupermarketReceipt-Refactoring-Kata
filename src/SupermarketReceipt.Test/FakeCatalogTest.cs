using System;
using Xunit;

namespace SupermarketReceipt.Test;

public class FakeCatalogTest
{
    [Fact]
    public void AddProductStoresTheProductPrice()
    {
        var catalog = new FakeCatalog();
        var product = new Product("toothbrush", ProductUnit.Each);

        catalog.AddProduct(product, 0.99);

        Assert.Equal(0.99, catalog.GetUnitPrice(product));
    }

    [Fact]
    public void AddProductRejectsDuplicateProductNames()
    {
        var catalog = new FakeCatalog();
        var product = new Product("toothbrush", ProductUnit.Each);

        catalog.AddProduct(product, 0.99);

        Assert.Throws<ArgumentOutOfRangeException>(() => catalog.AddProduct(product, 1.49));
    }
}
