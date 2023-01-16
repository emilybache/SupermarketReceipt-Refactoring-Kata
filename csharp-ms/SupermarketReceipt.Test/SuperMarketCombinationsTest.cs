using System;
using ApprovalTests.Combinations;
using ApprovalTests.Reporters;
using SupermarketReceipt;
using Xunit;

namespace Supermarket.Test
{
    public class SupermarketCombinationsTest
    {
        [Fact]
        [UseReporter(typeof(DiffReporter))]
        public void ApproveReceiptCalculation()
        {
            var itemQuantities = new double[] {
                1,
                5
            };
            var productUnits = new[] {
                ProductUnit.Each,
                ProductUnit.Kilo
            };
            var specialOfferTypes = new[] {
                SpecialOfferType.TenPercentDiscount,
                SpecialOfferType.FiveForAmount,
                SpecialOfferType.ThreeForTwo,
                SpecialOfferType.TwoForAmount
            };
            var specialOfferArguments = new[] {
                10.0
            };

            CombinationApprovals.VerifyAllCombinations(
            CallCheckoutAndGetReceipt,
            itemQuantities,
            productUnits,
            specialOfferTypes,
            specialOfferArguments);
        }

        private string CallCheckoutAndGetReceipt(
            double itemQuantity,
            ProductUnit productUnit,
            SpecialOfferType specialOfferType,
            double specialOfferArgument)
        {
            var catalog = new FakeCatalog();
            var cart = new ShoppingCart();
            var teller = new Teller(catalog);

            var product = new Product($"chocolate", productUnit);
            catalog.AddProduct(product, 1.00);
            cart.AddItem(product);
            cart.AddItemQuantity(product, itemQuantity);
            teller.AddSpecialOffer(specialOfferType, product, specialOfferArgument);

            var receipt = teller.ChecksOutArticlesFrom(cart);
            var receiptPrinter = new ReceiptPrinter();
            receiptPrinter.StartWithNewLine();
            receiptPrinter.WrapReceiptOutputWith("-");

            var receiptOutput = receiptPrinter.PrintReceipt(receipt);
            return receiptOutput;
        }
    }
}