
import pytest

from model_objects import (
    Discount, Offer, Product, ProductQuantity, ProductUnit, SpecialOfferType,
)
from receipt import Receipt
from shopping_cart import ShoppingCart
from teller import Teller
from tests.auxiliares import fechar_compra, imprimir, montar_carrinho, total_descontos
from tests.fake_catalog import FakeCatalog

TRES_POR_DOIS = SpecialOfferType.THREE_FOR_TWO
DEZ_POR_CENTO = SpecialOfferType.TEN_PERCENT_DISCOUNT
DOIS_POR_VALOR = SpecialOfferType.TWO_FOR_AMOUNT
CINCO_POR_VALOR = SpecialOfferType.FIVE_FOR_AMOUNT


# Carrinho sem ofertas 

@pytest.mark.parametrize("quantidade, total_esperado", [(1, 10.00), (2, 20.00), (7, 70.00)])
def test_produto_sem_oferta_custa_quantidade_vezes_preco(teller, escova, quantidade, total_esperado):
    recibo = fechar_compra(teller, (escova, quantidade))

    assert recibo.total_price() == pytest.approx(total_esperado)
    assert recibo.discounts == []


def test_produto_por_peso_sem_oferta_cobra_valor_exato_da_fracao(teller, maca):
    recibo = fechar_compra(teller, (maca, 2.5))

    assert recibo.total_price() == pytest.approx(5.00)
    assert recibo.items[0].quantity == 2.5
    assert recibo.items[0].price == 2.00
    assert recibo.items[0].total_price == pytest.approx(5.00)


def test_carrinho_vazio_gera_recibo_zerado(teller):
    recibo = teller.checks_out_articles_from(ShoppingCart())

    assert recibo.items == []
    assert recibo.discounts == []
    assert recibo.total_price() == 0


def test_total_e_soma_dos_itens_de_produtos_diferentes(teller, escova, creme_dental, maca):
    recibo = fechar_compra(teller, (escova, 2), (creme_dental, 1), (maca, 1.5))

    assert recibo.total_price() == pytest.approx(20.00 + 15.00 + 3.00)
    assert [i.product for i in recibo.items] == [escova, creme_dental, maca]


def test_preco_unitario_vem_do_catalogo_no_momento_do_checkout(catalogo, teller, escova):
    carrinho = montar_carrinho((escova, 2))
    catalogo.add_product(escova, 12.00)

    recibo = teller.checks_out_articles_from(carrinho)

    assert recibo.items[0].price == 12.00
    assert recibo.total_price() == pytest.approx(24.00)


def test_carrinho_agrega_quantidades_mas_recibo_lista_uma_linha_por_inclusao(teller, escova):
    carrinho = ShoppingCart()
    carrinho.add_item(escova)
    carrinho.add_item(escova)
    carrinho.add_item_quantity(escova, 2)

    recibo = teller.checks_out_articles_from(carrinho)

    assert carrinho.product_quantities[escova] == 4.0
    assert [i.quantity for i in recibo.items] == [1.0, 1.0, 2]
    assert recibo.total_price() == pytest.approx(40.00)


# Ofertas: escopo de aplicação 

def test_oferta_so_vale_para_o_produto_cadastrado(teller, escova, creme_dental):
    teller.add_special_offer(TRES_POR_DOIS, escova, None)

    recibo = fechar_compra(teller, (creme_dental, 3))

    assert recibo.discounts == []
    assert recibo.total_price() == pytest.approx(45.00)


def test_oferta_de_produto_ausente_do_carrinho_nao_gera_desconto(teller, escova, creme_dental):
    teller.add_special_offer(DEZ_POR_CENTO, escova, 10.0)

    recibo = fechar_compra(teller, (creme_dental, 1))

    assert recibo.discounts == []


def test_novo_cadastro_para_o_mesmo_produto_substitui_a_oferta_anterior(teller, escova):
    teller.add_special_offer(DEZ_POR_CENTO, escova, 10.0)
    teller.add_special_offer(TRES_POR_DOIS, escova, None)

    recibo = fechar_compra(teller, (escova, 3))

    assert len(teller.offers) == 1
    assert [d.description for d in recibo.discounts] == ["3 for 2"]
    assert recibo.total_price() == pytest.approx(20.00)


def test_oferta_cinco_por_valor_gera_um_desconto_no_checkout(teller, escova):
    teller.add_special_offer(CINCO_POR_VALOR, escova, 40.0)

    recibo = fechar_compra(teller, (escova, 12))

    assert len(recibo.discounts) == 1


def test_quantidades_de_inclusoes_separadas_somam_para_a_oferta(teller, escova):
    teller.add_special_offer(TRES_POR_DOIS, escova, None)
    carrinho = ShoppingCart()
    for _ in range(3):
        carrinho.add_item(escova)

    recibo = teller.checks_out_articles_from(carrinho)

    assert total_descontos(recibo) == pytest.approx(-10.00)
    assert recibo.total_price() == pytest.approx(20.00)


# 3 por 2 

@pytest.mark.parametrize("quantidade, desconto_esperado", [
    (1, 0.0),     # abaixo do limite
    (2, 0.0),     # limite inferior: 2 unidades ainda não ativam a oferta
    (3, -10.0),   # limite exato
    (4, -10.0),
    (5, -10.0),   # resto 2 pago integralmente
    (6, -20.0),   # dois grupos
    (7, -20.0),
])
def test_tres_por_dois(teller, escova, quantidade, desconto_esperado):
    teller.add_special_offer(TRES_POR_DOIS, escova, None)

    recibo = fechar_compra(teller, (escova, quantidade))

    assert total_descontos(recibo) == pytest.approx(desconto_esperado)
    assert recibo.total_price() == pytest.approx(10.00 * quantidade + desconto_esperado)
    if desconto_esperado:
        assert [d.description for d in recibo.discounts] == ["3 for 2"]
    else:
        assert recibo.discounts == []

# Desconto percentual 

@pytest.mark.parametrize("percentual, descricao, desconto_esperado", [
    (10.0, "10.0% off", -3.00),
    (20, "20% off", -6.00),
    (0, "0% off", 0.0),
])
def test_desconto_percentual_incide_sobre_toda_a_quantidade(
        teller, escova, percentual, descricao, desconto_esperado):
    teller.add_special_offer(DEZ_POR_CENTO, escova, percentual)

    recibo = fechar_compra(teller, (escova, 3))

    assert total_descontos(recibo) == pytest.approx(desconto_esperado)
    assert recibo.total_price() == pytest.approx(30.00 + desconto_esperado)
    assert [d.description for d in recibo.discounts] == [descricao]


def test_desconto_percentual_nao_exige_quantidade_minima(teller, escova):
    teller.add_special_offer(DEZ_POR_CENTO, escova, 10.0)

    recibo = fechar_compra(teller, (escova, 1))

    assert total_descontos(recibo) == pytest.approx(-1.00)


def test_desconto_percentual_usa_a_fracao_exata_em_produto_por_peso(teller, maca):
    teller.add_special_offer(DEZ_POR_CENTO, maca, 10.0)

    recibo = fechar_compra(teller, (maca, 2.5))

    assert total_descontos(recibo) == pytest.approx(-0.50)
    assert recibo.total_price() == pytest.approx(4.50)


# 2 por valor fixo

def test_dois_por_valor_com_uma_unidade_nao_gera_desconto(teller, escova):
    teller.add_special_offer(DOIS_POR_VALOR, escova, 15.0)

    recibo = fechar_compra(teller, (escova, 1))

    assert recibo.discounts == []
    assert recibo.total_price() == pytest.approx(10.00)


@pytest.mark.parametrize("quantidade, desconto_esperado, total_esperado", [
    (2, -5.0, 15.0),
    (4, -10.0, 30.0),
    (6, -15.0, 45.0),
])
def test_dois_por_valor_com_quantidade_par(teller, escova, quantidade, desconto_esperado, total_esperado):
    teller.add_special_offer(DOIS_POR_VALOR, escova, 15.0)

    recibo = fechar_compra(teller, (escova, quantidade))

    assert total_descontos(recibo) == pytest.approx(desconto_esperado)
    assert recibo.total_price() == pytest.approx(total_esperado)
    assert [d.description for d in recibo.discounts] == ["2 for 15.0"]


@pytest.mark.parametrize("quantidade, desconto_legado, total_legado", [
    (3, +2.5, 32.5),   # o "desconto" é positivo: o cliente paga MAIS que sem a oferta (30,00)
    (5, -2.5, 47.5),
    (7, -7.5, 62.5),
])
def test_dois_por_valor_com_quantidade_impar_defeito_legado(
        teller, escova, quantidade, desconto_legado, total_legado):
        
    """Defeito legado: determinadas quantidades ímpares fazem a oferta
    aumentar o valor final da compra.

    Com 3 unidades a R$ 10,00 e a oferta "2 por R$ 15,00",
    o cliente deveria pagar R$ 25,00, considerando duas unidades
    promocionais e uma unidade restante.

    Entretanto, o legado cobra R$ 32,50, valor superior aos
    R$ 30,00 que seriam cobrados sem a oferta.
    """
    teller.add_special_offer(DOIS_POR_VALOR, escova, 15.0)

    recibo = fechar_compra(teller, (escova, quantidade))

    assert total_descontos(recibo) == pytest.approx(desconto_legado)
    assert recibo.total_price() == pytest.approx(total_legado)


# 5 por valor fixo 

@pytest.mark.parametrize("quantidade, desconto_esperado, total_esperado", [
    (4, 0.0, 40.0),     # abaixo do limite
    (5, -10.0, 40.0),   # limite exato
    (6, -10.0, 50.0),
    (9, -10.0, 80.0),   # resto 4 pago integralmente
    (10, -20.0, 80.0),  # dois grupos
    (11, -20.0, 90.0),
])
def test_cinco_por_valor(teller, escova, quantidade, desconto_esperado, total_esperado):
    teller.add_special_offer(CINCO_POR_VALOR, escova, 40.0)

    recibo = fechar_compra(teller, (escova, quantidade))

    assert total_descontos(recibo) == pytest.approx(desconto_esperado)
    assert recibo.total_price() == pytest.approx(total_esperado)
    if desconto_esperado:
        assert [d.description for d in recibo.discounts] == ["5 for 40.0"]
    else:
        assert recibo.discounts == []


# Quantidades fracionárias em promoções por grupo

@pytest.mark.parametrize("oferta, argumento, quantidade, desconto_legado, total_legado", [
    (TRES_POR_DOIS, None, 2.9, 0.0, 5.80),     # parte inteira 2: não ativa a oferta
    (TRES_POR_DOIS, None, 3.5, -3.0, 4.00),
    (DOIS_POR_VALOR, 3.0, 2.5, -2.0, 3.00),
    (CINCO_POR_VALOR, 8.0, 5.5, -3.0, 8.00),
])
def test_promocao_por_grupo_com_quantidade_fracionaria_legado(
        teller, maca, oferta, argumento, quantidade, desconto_legado, total_legado):
    """Defeito legado: nas promoções por grupo efetivamente ativadas,
    a fração restante pode deixar de ser cobrada.

    O legado utiliza a parte inteira da quantidade para calcular
    os grupos promocionais e, em determinadas situações, não
    considera o restante fracionário no valor final.

    Exemplo: em 3,5 kg a R$ 2,00, com a oferta "3 por 2",
    o cliente paga R$ 4,00, embora os 0,5 kg restantes
    devessem acrescentar R$ 1,00 ao valor da compra.
    """

    teller.add_special_offer(oferta, maca, argumento)

    recibo = fechar_compra(teller, (maca, quantidade))

    assert total_descontos(recibo) == pytest.approx(desconto_legado)
    assert recibo.total_price() == pytest.approx(total_legado)


# Várias ofertas na mesma compra e consistência do recibo

def test_ofertas_de_produtos_diferentes_coexistem_na_mesma_compra(
        teller, escova, creme_dental, fio_dental, maca):
    teller.add_special_offer(TRES_POR_DOIS, escova, None)
    teller.add_special_offer(DEZ_POR_CENTO, creme_dental, 20.0)
    teller.add_special_offer(CINCO_POR_VALOR, fio_dental, 20.0)

    recibo = fechar_compra(teller, (escova, 3), (creme_dental, 2), (fio_dental, 5), (maca, 1))

    assert [d.product for d in recibo.discounts] == [escova, creme_dental, fio_dental]
    assert [d.discount_amount for d in recibo.discounts] == pytest.approx([-10.0, -6.0, -5.0])
    assert recibo.total_price() == pytest.approx(30.0 + 30.0 + 25.0 + 2.0 - 21.0)


def test_descontos_seguem_a_ordem_da_primeira_inclusao_do_produto(teller, escova, creme_dental):
    teller.add_special_offer(TRES_POR_DOIS, escova, None)
    teller.add_special_offer(DEZ_POR_CENTO, creme_dental, 10.0)

    recibo = fechar_compra(teller, (creme_dental, 1), (escova, 3))

    assert [d.product for d in recibo.discounts] == [creme_dental, escova]


def test_total_do_recibo_e_soma_dos_itens_mais_descontos_negativos(teller, escova, creme_dental):
    teller.add_special_offer(TRES_POR_DOIS, escova, None)

    recibo = fechar_compra(teller, (escova, 3), (creme_dental, 1))

    soma_itens = sum(i.total_price for i in recibo.items)
    assert all(d.discount_amount < 0 for d in recibo.discounts)
    assert recibo.total_price() == pytest.approx(soma_itens + total_descontos(recibo))
    assert recibo.total_price() == pytest.approx(35.00)


def test_mesmo_carrinho_e_mesmas_ofertas_geram_o_mesmo_recibo(teller, escova, creme_dental):
    teller.add_special_offer(TRES_POR_DOIS, escova, None)
    carrinho = montar_carrinho((escova, 4), (creme_dental, 2))

    primeiro = teller.checks_out_articles_from(carrinho)
    segundo = teller.checks_out_articles_from(carrinho)

    assert imprimir(primeiro) == imprimir(segundo)


def test_alterar_o_carrinho_depois_de_um_checkout_recalcula_as_ofertas(teller, escova):
    teller.add_special_offer(TRES_POR_DOIS, escova, None)
    carrinho = montar_carrinho((escova, 2))
    assert teller.checks_out_articles_from(carrinho).discounts == []

    carrinho.add_item(escova)
    recibo = teller.checks_out_articles_from(carrinho)

    assert total_descontos(recibo) == pytest.approx(-10.00)
    assert recibo.total_price() == pytest.approx(20.00)

# Recibo impresso 

def test_recibo_impresso_com_todas_as_modalidades_de_oferta():
    """Texto completo do recibo, com preços de centavos para fixar o arredondamento."""
    catalogo = FakeCatalog()
    escova = Product("escova", ProductUnit.EACH)
    creme = Product("creme dental", ProductUnit.EACH)
    maca = Product("maca", ProductUnit.KILO)
    arroz = Product("arroz", ProductUnit.EACH)
    for produto, preco in [(escova, 0.99), (creme, 1.79), (maca, 1.99), (arroz, 2.49)]:
        catalogo.add_product(produto, preco)
    teller = Teller(catalogo)
    teller.add_special_offer(DEZ_POR_CENTO, arroz, 10.0)
    teller.add_special_offer(TRES_POR_DOIS, escova, None)
    teller.add_special_offer(DOIS_POR_VALOR, creme, 3.0)
    teller.add_special_offer(CINCO_POR_VALOR, maca, 7.5)

    recibo = fechar_compra(teller, (escova, 3), (arroz, 1), (maca, 5.5), (creme, 4), (arroz, 1))

    assert imprimir(recibo) == "\n".join([
        "escova                              2.97",
        "  0.99 * 3",
        "arroz                               2.49",
        "maca                               10.95",
        "  1.99 * 5.500",
        "creme dental                        7.16",
        "  1.79 * 4",
        "arroz                               2.49",
        "3 for 2 (escova)                   -0.99",
        "10.0% off (arroz)                  -0.50",
        "5 for 7.5 (maca)                   -3.45",
        "2 for 3.0 (creme dental)           -1.16",
        "",
        "Total:                             19.96",
        "",
    ])


def test_recibo_impresso_de_carrinho_vazio(teller):
    recibo = teller.checks_out_articles_from(ShoppingCart())

    assert imprimir(recibo) == "\nTotal:                              0.00\n"


def test_recibo_impresso_so_detalha_quantidade_diferente_de_um(teller, escova):
    um = imprimir(fechar_compra(teller, (escova, 1)))
    dois = imprimir(fechar_compra(teller, (escova, 2)))

    assert "*" not in um
    assert "  10.00 * 2\n" in dois


# Contratos públicos que devem permanecer compatíveis 

def test_tipos_de_oferta_existentes_e_seus_valores():
    assert {t.name: t.value for t in SpecialOfferType} == {
        "THREE_FOR_TWO": 1,
        "TEN_PERCENT_DISCOUNT": 2,
        "TWO_FOR_AMOUNT": 3,
        "FIVE_FOR_AMOUNT": 4,
    }
    assert {u.name: u.value for u in ProductUnit} == {"EACH": 1, "KILO": 2}


def test_add_special_offer_guarda_um_offer_com_tipo_produto_e_argumento(teller, escova):
    teller.add_special_offer(DEZ_POR_CENTO, escova, 15.0)

    oferta = teller.offers[escova]
    assert isinstance(oferta, Offer)
    assert (oferta.offer_type, oferta.product, oferta.argument) == (DEZ_POR_CENTO, escova, 15.0)


def test_product_with_name_devolve_o_produto_ou_none(teller, escova):
    assert teller.product_with_name("escova") is escova
    assert teller.product_with_name("inexistente") is None


def test_handle_offers_continua_aceitando_receipt_ofertas_e_catalogo(catalogo, escova):
    """Interface usada pelo Teller: ShoppingCart.handle_offers(receipt, offers, catalog)."""
    carrinho = montar_carrinho((escova, 3))
    recibo = Receipt()
    ofertas = {escova: Offer(TRES_POR_DOIS, escova, None)}

    carrinho.handle_offers(recibo, ofertas, catalogo)

    assert len(recibo.discounts) == 1
    assert recibo.discounts[0].product is escova
    assert recibo.discounts[0].discount_amount == pytest.approx(-10.00)


def test_receipt_soma_itens_e_descontos_e_devolve_copias_das_listas(escova):
    recibo = Receipt()
    recibo.add_product(escova, 2, 10.00, 20.00)
    recibo.add_discount(Discount(escova, "teste", -3.00))

    assert recibo.total_price() == pytest.approx(17.00)
    recibo.items.clear()
    recibo.discounts.clear()
    assert len(recibo.items) == 1
    assert len(recibo.discounts) == 1


def test_carrinho_expoe_itens_como_product_quantity(escova):
    carrinho = montar_carrinho((escova, 2))

    assert len(carrinho.items) == 1
    assert isinstance(carrinho.items[0], ProductQuantity)
    assert carrinho.items[0].product is escova
    assert carrinho.items[0].quantity == 2
