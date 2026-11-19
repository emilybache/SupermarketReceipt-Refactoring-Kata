import pytest

from model_objects import Product, ProductUnit
from teller import Teller
from tests.fake_catalog import FakeCatalog


def _cadastrar(catalogo, nome, unidade, preco):
    produto = Product(nome, unidade)
    catalogo.add_product(produto, preco)
    return produto


@pytest.fixture
def catalogo():
    return FakeCatalog()


@pytest.fixture
def teller(catalogo):
    return Teller(catalogo)


# Preços redondos para que os valores esperados possam ser conferidos à mão.
# Escova e creme dental seguem o exemplo do enunciado (R$ 10,00 e R$ 15,00).

@pytest.fixture
def escova(catalogo):
    return _cadastrar(catalogo, "escova", ProductUnit.EACH, 10.00)


@pytest.fixture
def creme_dental(catalogo):
    return _cadastrar(catalogo, "creme dental", ProductUnit.EACH, 15.00)


@pytest.fixture
def fio_dental(catalogo):
    return _cadastrar(catalogo, "fio dental", ProductUnit.EACH, 5.00)


@pytest.fixture
def shampoo(catalogo):
    return _cadastrar(catalogo, "shampoo", ProductUnit.EACH, 20.00)


@pytest.fixture
def condicionador(catalogo):
    return _cadastrar(catalogo, "condicionador", ProductUnit.EACH, 12.00)


@pytest.fixture
def maca(catalogo):
    """Produto vendido por peso (KILO), R$ 2,00 por kg."""
    return _cadastrar(catalogo, "maca", ProductUnit.KILO, 2.00)
