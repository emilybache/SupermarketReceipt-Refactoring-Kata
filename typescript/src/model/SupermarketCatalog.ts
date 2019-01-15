import {Product} from "./Product"

export interface SupermarketCatalog {
    addProduct(product: Product , price: number): void;

    getUnitPrice(product: Product): number;

}
