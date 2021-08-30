package dojo.supermarket.model;

import java.util.Objects;

public class Product {
    private final String name;
    private final ProductUnit unit;
    private String quantityType = "";

    public Product(String name, ProductUnit unit) {
        this.name = name;
        this.unit = unit;
    }

    public String getName() {
        return name;
    }


    public ProductUnit getUnit() {
        return unit;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Product product = (Product) o;
        return Objects.equals(name, product.name) &&
                unit == product.unit;
    }

    @Override
    public int hashCode() {

        return Objects.hash(name, unit);
    }

    public String getQuantityType() {
        return quantityType;
    }

    public void setQuantityType(String s) {
        this.quantityType = s;
    }
}
