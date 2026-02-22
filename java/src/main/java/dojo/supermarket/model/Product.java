package dojo.supermarket.model;

import java.util.Objects;

public record Product(String name, ProductUnit unit, String quantityType) {

    public Product(String name, ProductUnit unit) {
        this(name, unit, "");
    }

    public Product withQuantityType(String s) {
        return new Product(name, unit, s);
    }

}
