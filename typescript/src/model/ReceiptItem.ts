import {Product} from "./Product"

export class ReceiptItem {

    public constructor(public readonly product: Product,
                       public readonly quantity: number,
                       public readonly price: number,
                       public totalPrice: number) {
    }
}
