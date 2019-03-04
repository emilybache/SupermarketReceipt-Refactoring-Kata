import {Product} from "../../src/model/Product"
import {SupermarketCatalog} from "../../src/model/SupermarketCatalog"

export class FakeCatalog implements SupermarketCatalog {
    private products: {[key: string]: Product} = {};
    private prices: {[key: string]: number} = {};

    public addProduct(product: Product, price: number): void {
        this.products[product.getName()] = product;
        this.prices[product.getName()] = price;
    }

    public getUnitPrice(p: Product): number {
        return this.prices[p.getName()];
    }
}
