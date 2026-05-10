namespace SupermarketReceipt.Printing;

public interface IReceiptPrinter
{
    string PrintReceipt(Receipt receipt);
}
