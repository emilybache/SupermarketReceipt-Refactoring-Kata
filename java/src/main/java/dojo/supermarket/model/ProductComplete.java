package dojo.supermarket.model;

import java.math.BigDecimal;

public class ProductComplete {
	
	private String name;
	private ProductUnit unit;
	private BigDecimal amount;
	
	public ProductComplete(String name, ProductUnit unit, BigDecimal amount) {
		this.name = name;
		this.unit = unit;
		
		if(amount == null || amount.compareTo(BigDecimal.ZERO)<=0) {
			throw new IllegalArgumentException("Quantidade deve ser maior que zero");
		}
		if(unit.isDiscrete() && amount.stripTrailingZeros().scale() > 0) {
			throw new IllegalArgumentException("Quantidades unitárias não podem receber frações");
		}
		
	}
	
	
	
	
	

}
