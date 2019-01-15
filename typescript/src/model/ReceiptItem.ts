import {Product} from "./Product"

export class ReceiptItem {

    public constructor(private readonly product: Product,
                       private readonly quantity: number,
                       private readonly price: number,
                       private totalPrice: number) {
    }

    public getPrice(): number {
        return this.price;
    }

    public getProduct(): Product {
        return this.product;
    }

    public getQuantity(): number {
        return this.quantity;
    }

    public getTotalPrice(): number {
        return this.totalPrice;
    }


}
