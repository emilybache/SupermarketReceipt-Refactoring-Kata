# Registro de Defeitos do Sistema Legado

Este documento registra os defeitos encontrados no Supermarket Receipt durante o diagnóstico técnico e os testes de caracterização. Para cada defeito, ele descreve como o problema foi encontrado, sua causa e seu impacto, e explica **como a equipe decidiu resolvê-lo**.

> **Decisão da equipe:** todos os defeitos confirmados serão **corrigidos**. As correções são feitas **depois** da refatoração estrutural, em commits isolados do tipo `fix:`, um por defeito. Cada correção é acompanhada de testes que fixam o comportamento correto e de uma justificativa neste registro.

---

## 1. Resumo

| ID | Defeito | Onde | Severidade | Quem é prejudicado | Decisão |
|---|---|---|---|---|---|
| DEF-01 | "2 por valor" com quantidade ímpar cobra mais do que sem a oferta | `shopping_cart.py`, linha 45 | **Alta** | Cliente | Corrigir |
| DEF-02 | Nas ofertas por grupo, a fração restante de produtos por peso sai de graça | `shopping_cart.py`, linhas 45, 54–55 e 63–64 | **Média** | Supermercado | Corrigir |
| DEF-03 | `Teller.product_with_name` depende de um atributo que não faz parte do contrato do catálogo | `teller.py`, linhas 28–29 | **Baixa** (latente) | Operação do sistema | Corrigir |

**Critério de severidade:**
- **Alta:** o valor cobrado contraria a regra da oferta, contra o cliente.
- **Média:** o valor cobrado contraria a regra, contra o supermercado, em um cenário restrito.
- **Baixa:** a funcionalidade falha apenas fora do ambiente de testes e não afeta valores.

---

## 2. Defeitos em detalhe

### DEF-01: "2 por valor" com quantidade ímpar

| Campo | Descrição |
|---|---|
| **Como foi encontrado** | Testes de caracterização: `test_dois_por_valor_com_quantidade_impar_defeito_legado` |
| **Regra de negócio** | A cada par, paga-se o preço promocional. A unidade que sobra é cobrada a preço cheio |
| **Onde** | [`python_pytest/src/shopping_cart.py`, linha 45](../python_pytest/src/shopping_cart.py) |
| **Severidade** | Alta: a oferta **encarece** a compra |

**Reprodução:** escova a R$ 10,00, oferta "2 por R$ 15,00", carrinho com 3 escovas.

| Quantidade | Sem a oferta | Legado (observado) | Correto |
|---|---|---|---|
| 3 | R$ 30,00 | **R$ 32,50** | R$ 25,00 |
| 5 | R$ 50,00 | R$ 47,50 | R$ 40,00 |
| 7 | R$ 70,00 | R$ 62,50 | R$ 55,00 |
| 4 (par) | R$ 40,00 | R$ 30,00 | R$ 30,00 (sem mudança) |

No caso de 3 unidades, o recibo chega a exibir um "desconto" **positivo**: `2 for 15.0 (escova)   2.50`.

**Causa raiz:** o código usa divisão real (`/`) onde deveria usar divisão inteira (`//`):

```python
# legado: linha 45
total = offer.argument * (quantity_as_int / x) + quantity_as_int % 2 * unit_price
#                                          ^ 3 / 2 = 1.5 grupos, quando deveria ser 1
```

Com 3 unidades, o código cobra 1,5 × R$ 15,00 = R$ 22,50 pelos "pares" **e** R$ 10,00 pela unidade restante. A sobra acaba cobrada duas vezes: metade pelo preço do par e a unidade inteira pelo preço cheio.

**Por que corrigir:**
- O comportamento contraria a própria descrição da oferta.
- Ele prejudica o cliente, já que pagar mais com a oferta do que sem ela não tem justificativa comercial.
- A política de convivência com combos depende de saber quantas unidades cada oferta consumiu, e com 1,5 grupos esse número não faz sentido.

---

### DEF-02: Fração gratuita nas ofertas por grupo

| Campo | Descrição |
|---|---|
| **Como foi encontrado** | Testes de caracterização: `test_promocao_por_grupo_com_quantidade_fracionaria_legado` e `test_recibo_impresso_com_todas_as_modalidades_de_oferta` |
| **Regra de negócio** | Produtos por peso são cobrados pelo valor exato. Nas ofertas por grupo, o número de grupos usa a parte inteira da quantidade, e **toda a sobra** (unidades e fração) é cobrada a preço cheio |
| **Onde** | [`python_pytest/src/shopping_cart.py`](../python_pytest/src/shopping_cart.py): linha 45 (2 por valor), linhas 54–55 (3 por 2) e 63–64 (5 por valor) |
| **Ofertas afetadas** | 3 por 2, 2 por valor e 5 por valor, apenas quando a oferta **ativa** com quantidade fracionária. O desconto percentual não é afetado |
| **Severidade** | Média: o supermercado deixa de cobrar parte do produto, mas só em produtos por peso com oferta por grupo |

**Reprodução:** maçã a R$ 2,00 por kg.

| Oferta | Quantidade | Legado (observado) | Correto | Diferença |
|---|---|---|---|---|
| 3 por 2 | 2,9 kg | R$ 5,80 | R$ 5,80 | Nenhuma (a oferta não ativa) |
| 3 por 2 | 3,5 kg | R$ 4,00 | **R$ 5,00** | 0,5 kg não era cobrado |
| 2 por R$ 3,00 | 2,5 kg | R$ 3,00 | **R$ 4,00** | 0,5 kg não era cobrado |
| 5 por R$ 8,00 | 5,5 kg | R$ 8,00 | **R$ 9,00** | 0,5 kg não era cobrado |

**Causa raiz:** o preço cheio usa a quantidade real (com fração), mas o preço com a oferta usa só a parte inteira na sobra:

```python
# legado: linhas 54–55 (3 por 2); o mesmo padrão aparece nas linhas 45 e 63–64
discount_amount = quantity * unit_price - (
    (number_of_x * 2 * unit_price) + quantity_as_int % 3 * unit_price)
#   \______ usa 3,5 kg ______/                  \__ usa 3 kg: a fração 0,5 some __/
```

A diferença entre as duas quantidades vira desconto: a fração entra integralmente no desconto e nunca é cobrada.

**Por que corrigir:**
- O comportamento contraria a regra de cobrar produtos por peso pelo valor exato.
- O desconto concedido fica maior do que a oferta anuncia (em 3,5 kg de "3 por 2", o desconto deveria ser de 1 kg, mas o legado dá 1,5 kg).
- O defeito está replicado em três blocos de código, e a refatoração com Template Method permite corrigi-lo **em um único lugar**.

**Impacto para o cliente:** nos cenários afetados, o cliente passa a pagar a fração que antes saía de graça. A mudança é intencional, segue a regra anunciada da oferta e deve constar na comunicação da entrega.

---

### DEF-03: `product_with_name` fora do contrato do catálogo

| Campo | Descrição |
|---|---|
| **Como foi encontrado** | Diagnóstico técnico, na análise do DIP (Dependency Inversion Principle) |
| **Regra esperada** | O `Teller` deve funcionar com qualquer catálogo que siga o contrato de `SupermarketCatalog` |
| **Onde** | [`python_pytest/src/teller.py`, linhas 28–29](../python_pytest/src/teller.py) |
| **Severidade** | Baixa (latente): os testes não detectam o problema, porque o `FakeCatalog` tem o atributo `products` |

**Reprodução:**

```python
from catalog import SupermarketCatalog
from teller import Teller

Teller(SupermarketCatalog()).product_with_name("escova")
# AttributeError: 'SupermarketCatalog' object has no attribute 'products'
```

**Causa raiz:** o `Teller` acessa diretamente `self.catalog.products`, um atributo que existe apenas na implementação de teste (`FakeCatalog`) e não está declarado em `SupermarketCatalog`:

```python
# legado: teller.py, linhas 28–29
def product_with_name(self, name):
    return self.catalog.products.get(name, None)
```

**Por que corrigir:**
- Qualquer catálogo real que siga o contrato quebra essa funcionalidade.
- O `Teller` fica acoplado a um detalhe de implementação, o que viola o DIP.

---

## 3. Como cada defeito será corrigido

### 3.1 Princípios

1. **Refatorar primeiro, corrigir depois.** As Fases 4.1 a 4.6 do plano de execução mudam só a estrutura, e todos os testes de caracterização continuam passando sem alteração. Isso prova que a refatoração não mudou comportamento. As correções vêm em seguida, em commits separados.
2. **Um commit `fix:` por defeito**, com o ID do defeito na mensagem. Isso facilita a revisão e permite reverter cada correção isoladamente.
3. **Teste antes da correção.** Para cada defeito:
   1. Criar o teste do comportamento **correto**, marcado com `@pytest.mark.xfail(strict=True, reason="DEF-0X")`. Ele falha, o que prova que o defeito existe.
   2. No commit `fix:`, aplicar a correção, remover o `xfail` e atualizar ou remover o teste que fixava o comportamento defeituoso.
   3. Rodar a suíte inteira e salvar o log como evidência.

   Com `strict=True`, se a correção for aplicada e alguém esquecer de remover o `xfail`, o teste passa a falhar. Isso impede que a marcação fique esquecida.

### 3.2 DEF-01 e DEF-02: correção na classe `GroupPriceOffer`

Depois da refatoração (Fase 4.3), o algoritmo das ofertas por grupo existe em **um único lugar**, a classe base `GroupPriceOffer` (Template Method). As duas correções são feitas ali:

```python
# depois da correção (GroupPriceOffer.apply)
groups = int(quantity) // self.group_size()          # DEF-01: divisão inteira
if groups == 0:
    return PolicyResult([], {})
remainder = quantity - groups * self.group_size()    # DEF-02: a sobra inclui a fração
price_with_offer = groups * self.group_price(unit_price) + remainder * unit_price
discount_amount = quantity * unit_price - price_with_offer
```

| Commit | Mensagem | Escopo |
|---|---|---|
| 4.7 | `fix(DEF-01): cobra a sobra do 2 por valor a preço cheio` | Divisão inteira no cálculo de grupos |
| 4.8 | `fix(DEF-02): cobra a fração restante nas ofertas por grupo` | Sobra calculada com a quantidade real |

### 3.3 DEF-03: busca por nome no contrato do catálogo

| Arquivo | Alteração |
|---|---|
| `src/catalog.py` | Declarar `product_with_name(name)` em `SupermarketCatalog`. Ele lança a mesma exceção de acesso a banco que os outros métodos |
| `tests/fake_catalog.py` | Implementar `product_with_name(name)` com `self.products.get(name)` |
| `src/teller.py` | `Teller.product_with_name` passa a delegar para `self.catalog.product_with_name(name)` |

| Commit | Mensagem |
|---|---|
| 4.9 | `fix(DEF-03): busca de produto por nome via contrato do catálogo` |

A interface pública `Teller.product_with_name(name)` continua igual. O teste de contrato `test_product_with_name_devolve_o_produto_ou_none` continua passando sem alteração.

---

## 4. Impacto nos testes de caracterização

As correções alteram **7 casos de teste**, de propósito. Todos os outros testes da suíte de caracterização continuam com os mesmos valores esperados.

| Teste | Caso | Antes (legado) | Depois (corrigido) | Defeito |
|---|---|---|---|---|
| `test_dois_por_valor_com_quantidade_impar_defeito_legado` → renomear para `test_dois_por_valor_com_quantidade_impar_cobra_sobra_a_preco_cheio` | 3 un. | desconto +2,50; total 32,50 | desconto −5,00; total 25,00 | DEF-01 |
| | 5 un. | −2,50; total 47,50 | −10,00; total 40,00 | DEF-01 |
| | 7 un. | −7,50; total 62,50 | −15,00; total 55,00 | DEF-01 |
| `test_promocao_por_grupo_com_quantidade_fracionaria_legado` → renomear para `test_promocao_por_grupo_cobra_a_fracao_restante` | 3 por 2, 2,9 kg | 0,00; total 5,80 | **sem mudança** | — |
| | 3 por 2, 3,5 kg | −3,00; total 4,00 | −2,00; total 5,00 | DEF-02 |
| | 2 por 3,00, 2,5 kg | −2,00; total 3,00 | −1,00; total 4,00 | DEF-02 |
| | 5 por 8,00, 5,5 kg | −3,00; total 8,00 | −2,00; total 9,00 | DEF-02 |
| `test_recibo_impresso_com_todas_as_modalidades_de_oferta` | linha da maçã e total | `5 for 7.5 (maca) -3.45` e `Total: 19.96` | `5 for 7.5 (maca) -2.45` e `Total: 20.96` | DEF-02 |

**Teste novo:**

| Teste | Verifica | Defeito |
|---|---|---|
| `test_product_with_name_funciona_com_qualquer_catalogo_do_contrato` | Um catálogo que implementa só os métodos do contrato, sem o atributo `products`, funciona com `Teller.product_with_name` | DEF-03 |

Esses valores foram conferidos executando as duas versões do cálculo, a legada e a corrigida, sobre os mesmos carrinhos. Os casos com quantidades inteiras e o "2 por valor" com quantidade par deram resultados idênticos nas duas versões.

**Evidência de antes e depois:**
- O log [`pytest_antes_ubuntu.log`](evidencias/pytest_antes_ubuntu.log) registra o comportamento legado, com os valores defeituosos fixados.
- O log da suíte depois das correções deve ser salvo em `docs/evidencias/testes/`.
- O diff de cada commit `fix:` mostra exatamente quais valores esperados mudaram.

---

## 5. Impacto na política de combos

- **A garantia "cadastrar um combo nunca encarece o carrinho"** passa a ser medida contra **o mesmo carrinho sem combos, no sistema já corrigido**, e não contra o legado com defeitos. Nos cenários do DEF-02, o total corrigido é maior que o legado por causa da correção, não por causa dos combos.
- **O consumo de unidades fica coerente.** Com o DEF-01 corrigido, "2 por valor" com 3 unidades consome exatamente 2 unidades e libera 1 para os combos, e o cálculo de preço segue a mesma conta.
- **A fração de produtos por peso** nunca forma combo (o número de combos usa a parte inteira) e, com o DEF-02 corrigido, é cobrada a preço cheio.

---

## 6. Pontos avaliados e **não** classificados como defeito

Os pontos abaixo foram analisados e mantidos como estão. Cada um tem a justificativa ao lado.

| Ponto | Comportamento atual | Por que não é tratado como defeito |
|---|---|---|
| `TEN_PERCENT_DISCOUNT` aceita qualquer percentual | Com `argument = 20`, aplica 20% | O cálculo está correto; o problema é o nome. Renomear o enum quebraria a interface pública (penalidade do edital). Fica documentado |
| Oferta de 0% | Gera a linha `0% off (produto)   0.00` no recibo | O comportamento é coerente com o cadastro feito. Foi preservado e está coberto por teste |
| Oferta mal configurada (preço do grupo maior que o preço cheio, como "2 por R$ 25" com unidade a R$ 10) | Gera desconto positivo | O cálculo segue a oferta cadastrada. Validar o cadastro mudaria o contrato de `add_special_offer` e está fora do escopo |
| Uso de `float` para valores monetários | Possível imprecisão em centavos | Nenhum caso de erro foi observado nos testes, e a impressão arredonda para 2 casas. Fica registrado como **risco remanescente** (RNF09) |
| `ShoppingCart.items` devolve a lista interna | Quem chama pode alterar o carrinho por fora | É uma fragilidade de encapsulamento, sem efeito observado nos valores. Fica registrada no diagnóstico como ponto de melhoria |
| Quantidade zero ou negativa é aceita | É incluída no carrinho e no recibo | Não há regra de negócio definida para o caso. Criar uma validação mudaria o comportamento sem requisito que a justifique |

---

## 7. Rastreabilidade

| Defeito | Requisito relacionado | Risco do [plano de contingência](plano_contingencia.md#4-riscos) | Teste | Commit |
|---|---|---|---|---|
| DEF-01 | RF05 (ofertas por grupo): critério [DEF-01] do "2 por valor" | R03 (aplicação incorreta dos descontos); R10 (correção além do previsto) | `test_dois_por_valor_com_quantidade_impar_cobra_sobra_a_preco_cheio` | 4.7 |
| DEF-02 | RF03 (valor = preço × quantidade) e RF05: critério [DEF-02] da fração nas ofertas por grupo | R02 (cálculo incorreto dos valores); R03 (aplicação incorreta dos descontos); R10 (correção além do previsto) | `test_promocao_por_grupo_cobra_a_fracao_restante`, `test_recibo_impresso_com_todas_as_modalidades_de_oferta` | 4.8 |
| DEF-03 | RNF01 (manutenção sem dependências desnecessárias): critério [DEF-03]; interface `Teller.product_with_name` | R10 (correção além do previsto); R13 (quebra de interface) | `test_product_with_name_funciona_com_qualquer_catalogo_do_contrato`, `test_product_with_name_devolve_o_produto_ou_none` | 4.9 |
