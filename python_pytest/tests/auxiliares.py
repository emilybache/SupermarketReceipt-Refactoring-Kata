"""Funcoes auxiliares compartilhadas pelos testes de caracterização e de combos."""
from shopping_cart import ShoppingCart
from tests.receipt_printer import ReceiptPrinter


def montar_carrinho(*itens):
    """Cria um carrinho a partir do produtoe quantidade."""
    carrinho = ShoppingCart()
    for produto, quantidade in itens:
        if quantidade:
            carrinho.add_item_quantity(produto, quantidade)
    return carrinho


def fechar_compra(teller, *itens):
    """Monta o carrinho e executa o checkout, devolvendo o recibo."""
    return teller.checks_out_articles_from(montar_carrinho(*itens))


def total_descontos(recibo):
    """Soma dos descontos do recibo"""
    return sum(d.discount_amount for d in recibo.discounts)


def imprimir(recibo, colunas=40):
    return ReceiptPrinter(colunas).print_receipt(recibo)
