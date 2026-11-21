# Diagnóstico Técnico

## Sumário

1. [Contextualização](#1-contextualização)
2. [Metodologia](#2-metodologia)
3. [Análise de code smells](#3-análise-de-code-smells)
4. [Análise dos princípios SOLID](#4-análise-dos-princípios-solid)
5. [Defeitos encontrados](#5-defeitos-encontrados)
6. [Análise dos padrões de documentação](#6-análise-dos-padrões-de-documentação)
7. [Análise da testabilidade](#7-análise-da-testabilidade)
8. [Síntese do diagnóstico técnico](#8-síntese-do-diagnóstico-técnico)
9. [Priorização dos pontos para refatoração](#9-priorização-dos-pontos-para-refatoração)

> As referências de linha valem para a versão legada do código, marcada pela tag `legado-baseline` (commit `58df732`). Todos os caminhos são relativos a `python_pytest/`.

---

## 1. Contextualização

Foi realizada uma análise técnica da implementação do projeto Supermarket Receipt Refactoring Kata, com o objetivo de identificar pontos que possam comprometer a manutenção, a evolução e a testabilidade do sistema.

A avaliação concentrou-se na estrutura dos componentes, na distribuição de responsabilidades, na implementação das regras promocionais e na organização dos testes existentes. Os problemas identificados foram registrados considerando seus impactos e as oportunidades de melhoria para a etapa de refatoração.

## 2. Metodologia

A análise foi realizada por meio da inspeção dos arquivos do diretório `src/` e dos testes já presentes no projeto, seguida da execução da suíte automatizada com pytest.

Foram avaliados:

- a organização e a responsabilidade das classes e métodos;
- a complexidade das regras de negócio e das estruturas condicionais;
- possíveis code smells e problemas de modelagem;
- as dependências entre os componentes;
- a cobertura e a capacidade de validação dos testes existentes.

As ocorrências foram relacionadas aos respectivos arquivos e trechos de código, servindo de base para o diagnóstico e para a definição das prioridades de refatoração. Os defeitos de comportamento confirmados estão detalhados no [registro de defeitos](registro_de_defeitos.md).

---

## 3. Análise de code smells

A inspeção do código identificou os principais problemas estruturais no processamento das ofertas promocionais, especialmente nos arquivos `shopping_cart.py` e `model_objects.py`.

Para referência, este é o método `handle_offers()` completo, citado na maior parte das seções a seguir:

```python
# src/shopping_cart.py, linhas 30–68
30  def handle_offers(self, receipt, offers, catalog):
31      for p in self._product_quantities.keys():
32          quantity = self._product_quantities[p]
33          if p in offers.keys():
34              offer = offers[p]
35              unit_price = catalog.unit_price(p)
36              quantity_as_int = int(quantity)
37              discount = None
38              x = 1
39              if offer.offer_type == SpecialOfferType.THREE_FOR_TWO:
40                  x = 3
41
42              elif offer.offer_type == SpecialOfferType.TWO_FOR_AMOUNT:
43                  x = 2
44                  if quantity_as_int >= 2:
45                      total = offer.argument * (quantity_as_int / x) + quantity_as_int % 2 * unit_price
46                      discount_n = unit_price * quantity - total
47                      discount = Discount(p, "2 for " + str(offer.argument), -discount_n)
48
49              if offer.offer_type == SpecialOfferType.FIVE_FOR_AMOUNT:
50                  x = 5
51
52              number_of_x = math.floor(quantity_as_int / x)
53              if offer.offer_type == SpecialOfferType.THREE_FOR_TWO and quantity_as_int > 2:
54                  discount_amount = quantity * unit_price - (
55                              (number_of_x * 2 * unit_price) + quantity_as_int % 3 * unit_price)
56                  discount = Discount(p, "3 for 2", -discount_amount)
57
58              if offer.offer_type == SpecialOfferType.TEN_PERCENT_DISCOUNT:
59                  discount = Discount(p, str(offer.argument) + "% off",
60                                      -quantity * unit_price * offer.argument / 100.0)
61
62              if offer.offer_type == SpecialOfferType.FIVE_FOR_AMOUNT and quantity_as_int >= 5:
63                  discount_total = unit_price * quantity - (
64                              offer.argument * number_of_x + quantity_as_int % 5 * unit_price)
65                  discount = Discount(p, str(x) + " for " + str(offer.argument), -discount_total)
66
67              if discount:
68                  receipt.add_discount(discount)
```

### 3.1 Long Method

O método `handle_offers()`, em `src/shopping_cart.py` (linhas 30 a 68), concentra a identificação das ofertas, a verificação de suas condições e o cálculo dos descontos. A implementação reúne regras promocionais diferentes em um único método, o que dificulta sua leitura, manutenção e evolução.

O principal impacto aparece na alteração ou na inclusão de promoções: qualquer mudança exige mexer em uma operação extensa, aumentando sua complexidade.

**Recomendação:** separar os cálculos promocionais em componentes específicos, um por modalidade de oferta.

### 3.2 Switch Statements

No mesmo método, entre as linhas 39 e 65, há uma sequência extensa de condicionais para identificar e executar o cálculo de cada tipo de promoção definido em `SpecialOfferType`. A sequência também mistura `if`/`elif` (linhas 39 e 42) com `if` independentes (linhas 49, 53, 58 e 62), de modo que o mesmo `offer_type` é testado mais de uma vez em pontos diferentes do método.

Com a implementação atual, a inclusão de novas modalidades exige ampliar a sequência de condições, aumentando os caminhos de execução e a complexidade do método.

**Recomendação:** substituir a seleção condicional por comportamentos específicos associados a cada modalidade de oferta (polimorfismo).

### 3.3 Duplicação de código

As três ofertas por grupo ("2 por valor", "3 por 2" e "5 por valor") seguem a **mesma fórmula**: o preço com a oferta é *número de grupos × preço do grupo + unidades que sobram × preço unitário*, e o desconto é a diferença para o preço cheio. Essa fórmula foi escrita três vezes, com pequenas variações:

```python
# "2 por valor": linha 45
total = offer.argument * (quantity_as_int / x) + quantity_as_int % 2 * unit_price
discount_n = unit_price * quantity - total

# "3 por 2": linhas 54–55
discount_amount = quantity * unit_price - (
            (number_of_x * 2 * unit_price) + quantity_as_int % 3 * unit_price)

# "5 por valor": linhas 63–64
discount_total = unit_price * quantity - (
            offer.argument * number_of_x + quantity_as_int % 5 * unit_price)
```

| Elemento da fórmula | 2 por valor | 3 por 2 | 5 por valor |
|---|---|---|---|
| Número de grupos | `quantity_as_int / x` (divisão real) | `number_of_x` | `number_of_x` |
| Preço do grupo | `offer.argument` | `2 * unit_price` | `offer.argument` |
| Unidades que sobram | `quantity_as_int % 2` | `quantity_as_int % 3` | `quantity_as_int % 5` |
| Nome do resultado | `discount_n` | `discount_amount` | `discount_total` |

As variações não são intencionais: são cópias que divergiram. A consequência concreta é que **os defeitos também foram copiados**. A divisão real da linha 45 só existe na cópia do "2 por valor" (DEF-01), e o uso de `quantity_as_int` na sobra, que deixa a fração de produtos por peso sem cobrança, está nas três cópias (DEF-02). Ver a [seção 5](#5-defeitos-encontrados).

**Recomendação:** extrair a fórmula para um único lugar (Template Method), em que cada oferta informe apenas o tamanho do grupo e o preço do grupo. Assim, as correções de DEF-01 e DEF-02 são feitas uma única vez.

### 3.4 Lógica do "3 por 2" dividida em dois blocos

A regra do "3 por 2" está espalhada em dois pontos separados do método, intercalados com o código de outras ofertas:

```python
# linhas 39–40: define o tamanho do grupo
if offer.offer_type == SpecialOfferType.THREE_FOR_TWO:
    x = 3

# ... linhas 42–52: código do "2 por valor", do "5 por valor" e cálculo de number_of_x ...

# linhas 53–56: calcula o desconto
if offer.offer_type == SpecialOfferType.THREE_FOR_TWO and quantity_as_int > 2:
    discount_amount = quantity * unit_price - (
                (number_of_x * 2 * unit_price) + quantity_as_int % 3 * unit_price)
    discount = Discount(p, "3 for 2", -discount_amount)
```

O mesmo acontece com o "5 por valor" (linhas 49–50 e 62–65). Para entender uma única oferta, é preciso ler o método inteiro e acompanhar o valor de `x` ao longo dele. Além disso, a condição de ativação da linha 53 (`> 2`) e o tamanho do grupo da linha 40 (`3`) expressam a mesma regra de formas diferentes.

**Recomendação:** reunir toda a regra de cada oferta em uma única unidade de código (ver 3.1 e 3.2).

### 3.5 Feature Envy

O método `handle_offers()` pertence a `ShoppingCart`, mas usa muito mais os dados de outros objetos do que os do próprio carrinho:

| Objeto | O que o método usa |
|---|---|
| `ShoppingCart` (o próprio) | apenas `self._product_quantities` (linhas 31–32) |
| `Offer` | `offer.offer_type` (seis comparações) e `offer.argument` (linhas 45, 47, 59, 60, 64 e 65) |
| `SupermarketCatalog` | `catalog.unit_price(p)` (linha 35) |
| `Receipt` | `receipt.add_discount(...)` (linha 68) |
| `Discount` | criação do objeto e montagem da descrição (linhas 47, 56, 59 e 65) |

O método conhece os detalhes internos da oferta e decide sozinho como cada uma deve ser calculada, enquanto a classe `Offer` não tem comportamento (ver 3.6). É o sinal clássico de que o comportamento está na classe errada.

**Recomendação:** mover o cálculo do desconto para as abstrações de oferta, deixando ao carrinho apenas a responsabilidade de fornecer as quantidades.

### 3.6 Data Class

Em `src/model_objects.py`, a classe `Offer` (linhas 27 a 31) armazena o tipo da promoção, o produto e o argumento, mas não tem comportamento próprio para interpretar ou calcular os descontos:

```python
# src/model_objects.py, linhas 27–31
class Offer:
    def __init__(self, offer_type, product, argument):
        self.offer_type = offer_type
        self.product = product
        self.argument = argument
```

Essa característica está ligada à concentração dos cálculos em `ShoppingCart` (Feature Envy, seção 3.5) e indica uma modelagem orientada apenas ao armazenamento de dados. Com essa abordagem, os componentes consumidores precisam conhecer os detalhes de cada modalidade promocional para interpretar os dados da oferta.

**Recomendação:** incorporar os comportamentos promocionais às abstrações responsáveis por cada tipo de oferta.

### 3.7 Primitive Obsession

O atributo `argument` da classe `Offer` (linhas 27 a 31 de `model_objects.py`) representa valores com significados diferentes conforme a promoção:

| Tipo de oferta | Significado de `argument` |
|---|---|
| `THREE_FOR_TWO` | nenhum (o valor é ignorado) |
| `TEN_PERCENT_DISCOUNT` | percentual de desconto |
| `TWO_FOR_AMOUNT` | preço promocional de 2 unidades |
| `FIVE_FOR_AMOUNT` | preço promocional de 5 unidades |

A interpretação desse atributo depende do tipo da oferta, o que reduz a clareza da modelagem e favorece verificações condicionais nos componentes consumidores. O nome `TEN_PERCENT_DISCOUNT` reforça o problema: o enum aceita qualquer percentual em `argument` (com 20, aplica 20%), então o nome não descreve o que a oferta faz.

**Recomendação:** usar representações específicas para os parâmetros de cada modalidade promocional. O enum `SpecialOfferType` e a assinatura de `add_special_offer` devem ser mantidos, porque fazem parte da interface pública.

### 3.8 Nomes pouco expressivos

Vários nomes do método `handle_offers()` não revelam a intenção do código:

| Nome | Onde | O que representa | Problema |
|---|---|---|---|
| `p` | linhas 31–35, 47, 56, 59, 65 | o produto | letra única |
| `x` | linhas 38, 40, 43, 45, 50, 52, 65 | o tamanho do grupo da oferta | letra única, com valor que muda ao longo do método |
| `number_of_x` | linhas 52, 55, 64 | o número de grupos completos | depende de entender o que é `x` |
| `discount_n` | linhas 46–47 | o valor do desconto | sufixo `_n` sem significado |
| `discount_n`, `discount_amount`, `discount_total` | linhas 46, 54, 63 | o mesmo conceito | três nomes diferentes para a mesma coisa |
| `total` | linha 45 | o preço com a oferta | pode ser confundido com o total do recibo |
| `pq` | `teller.py`, linhas 17–19 | um `ProductQuantity` | abreviação |

**Recomendação:** renomear as variáveis locais para nomes que expressem o domínio (`product`, `group_size`, `number_of_groups`, `discount_amount`). Os nomes públicos ficam como estão, para não quebrar a interface.

### 3.9 Números mágicos

As regras das ofertas estão codificadas em literais espalhados pelo método, sem nome que explique seu significado:

| Literal | Onde | Significado |
|---|---|---|
| `3` | linhas 40 e 55 (`% 3`) | tamanho do grupo do "3 por 2" |
| `2` | linhas 43, 44 (`>= 2`), 45 (`% 2`), 53 (`> 2`) e 55 (`* 2`) | tamanho do grupo do "2 por valor"; condição de ativação do "3 por 2"; unidades pagas no "3 por 2" |
| `5` | linhas 50, 62 (`>= 5`) e 64 (`% 5`) | tamanho do grupo do "5 por valor" |
| `100.0` | linha 60 | conversão de percentual para fração |
| `"2 for "` | linha 47 | descrição montada com o literal, enquanto a linha 65 usa `str(x)` para o mesmo fim |

O mesmo número aparece com papéis diferentes (o `2` significa três coisas distintas), e o mesmo papel aparece com formas diferentes (o tamanho do grupo ora vem de `x`, ora é repetido como literal no `%`). Alterar uma regra exige encontrar e mudar todas as ocorrências de forma coerente.

**Recomendação:** cada oferta deve declarar seu tamanho de grupo em um único lugar, e os demais cálculos devem derivar desse valor.

### 3.10 Encapsulamento das coleções do carrinho

A propriedade `ShoppingCart.items` devolve a própria lista interna, e `product_quantities` devolve o próprio dicionário interno:

```python
# src/shopping_cart.py, linhas 12–21
@property
def items(self):
    return self._items                   # a lista interna, não uma cópia

@property
def product_quantities(self):
    return self._product_quantities      # o dicionário interno, não uma cópia
```

Quem recebe essas coleções pode alterar o carrinho por fora, sem passar por `add_item_quantity()`. Isso permite deixar `_items` e `_product_quantities` inconsistentes entre si: uma unidade incluída diretamente em `items` aparece no recibo, mas não é considerada no cálculo das ofertas, que usa `_product_quantities`. A classe `Receipt`, no mesmo projeto, já resolve isso devolvendo cópias (`return self._items[:]`, linhas 29–35 de `receipt.py`).

Nenhum efeito sobre os valores foi observado nos testes, então o ponto é tratado como fragilidade de projeto, e não como defeito (ver a seção 6 do [registro de defeitos](registro_de_defeitos.md)).

**Recomendação:** avaliar a devolução de cópias ou de visões somente leitura, seguindo o padrão de `Receipt`, desde que o tipo dos elementos (`ProductQuantity`) e o comportamento observado pelos consumidores sejam preservados.

### 3.11 `float` para valores monetários

Preços, quantidades, descontos e totais são todos representados como `float`, que não representa valores decimais com exatidão:

```python
>>> 0.99 + 0.99 + 0.99
2.9699999999999998
>>> 2.5 * 1.99          # 2,5 kg de maçã a R$ 1,99
4.975                   # meio centavo: o valor não existe em reais
```

O `Receipt.total_price()` soma esses valores sem arredondar, e o arredondamento para 2 casas só acontece na impressão do recibo. Por isso o teste original precisa de `pytest.approx` para comparar totais. Nenhum valor errado foi observado nos cenários testados, mas a margem de erro cresce com o número de itens e de descontos.

**Recomendação:** não alterar o tipo durante a refatoração, porque isso mudaria valores observados pelos testes de caracterização e pela interface pública. O ponto fica registrado como **risco remanescente** (RNF09), e a comparação de totais na política de combos deve ser feita em centavos.

### 3.12 Síntese dos code smells

| Smell | Onde | Seção |
|---|---|---|
| Long Method | `shopping_cart.py`, linhas 30–68 | 3.1 |
| Switch Statements | `shopping_cart.py`, linhas 39–65 | 3.2 |
| Duplicação de código | `shopping_cart.py`, linhas 45, 54–55 e 63–64 | 3.3 |
| Lógica de uma oferta dividida | `shopping_cart.py`, linhas 39–40 e 53–56 (3 por 2); 49–50 e 62–65 (5 por valor) | 3.4 |
| Feature Envy | `shopping_cart.py`, `handle_offers()` | 3.5 |
| Data Class | `model_objects.py`, linhas 27–31 | 3.6 |
| Primitive Obsession | `model_objects.py`, `Offer.argument` | 3.7 |
| Nomes pouco expressivos | `shopping_cart.py`, `handle_offers()`; `teller.py`, linhas 17–19 | 3.8 |
| Números mágicos | `shopping_cart.py`, linhas 40–65 | 3.9 |
| Coleções internas expostas | `shopping_cart.py`, linhas 12–21 | 3.10 |
| `float` para dinheiro | todo o `src/` | 3.11 |

De maneira geral, os principais pontos de atenção estão ligados à concentração das regras promocionais em `ShoppingCart` e à representação genérica das ofertas. A refatoração deve priorizar a simplificação do processamento promocional e a organização dessas regras.

---

## 4. Análise dos princípios SOLID

A análise considerou os cinco princípios SOLID, com o objetivo de identificar oportunidades de melhoria na organização das responsabilidades, na extensibilidade e nas dependências entre os componentes.

### 4.1 SRP: Single Responsibility Principle (Princípio da Responsabilidade Única)

O principal ponto de atenção está na classe `ShoppingCart`, em `src/shopping_cart.py`. Além de gerenciar os produtos e suas quantidades, a classe também interpreta e executa as regras de cálculo das ofertas promocionais, concentradas no método `handle_offers()`, nas linhas 30 a 68.

A classe acumula responsabilidades distintas, o que torna sua manutenção mais complexa e aumenta a quantidade de motivos que podem exigir alterações. De certo ponto de vista, essa violação é a causa do Long Method e do Feature Envy apontados nas seções 3.1 e 3.5.

**Sugestão para a refatoração:** separar a gestão do carrinho do cálculo das ofertas, deixando cada modalidade promocional sob a responsabilidade de uma abstração própria.

### 4.2 OCP: Open/Closed Principle (Princípio Aberto/Fechado)

O método `handle_offers()`, em `src/shopping_cart.py`, usa estruturas condicionais para reconhecer cada modalidade definida em `SpecialOfferType`, em `src/model_objects.py` (linhas 21–25).

Nesse contexto, incluir uma nova modalidade promocional exige alterar a lógica existente, ampliando as condicionais e aumentando o risco de afetar comportamentos já implementados. A funcionalidade de combos, prevista para este projeto, é um exemplo direto dessa situação.

**Sugestão para a refatoração:** estruturar as ofertas por meio de abstrações que permitam adicionar novas modalidades sem modificar o processamento central.

### 4.3 LSP: Liskov Substitution Principle (Princípio da Substituição de Liskov)

Foram analisados alguns pontos da classe `FakeCatalog`, em `tests/fake_catalog.py`, que herda de `SupermarketCatalog` (`src/catalog.py`) e implementa os métodos de cadastro e consulta de preços.

Não há evidência suficiente para afirmar uma violação do princípio de substituição. O catálogo falso é uma alternativa em memória para os testes e pode ser modificado e aproveitado nos testes posteriores à refatoração, com o cuidado de manter um conjunto de testes que atenda adequadamente às regras de negócio.

A única diferença relevante entre as implementações, o atributo `products`, que existe só em `FakeCatalog`, é tratada no DIP (seção 4.5) e no DEF-03.

**Sugestão:** preservar a compatibilidade entre as implementações de catálogo e garantir que futuras alterações no contrato sejam respeitadas por todas elas.

### 4.4 ISP: Interface Segregation Principle (Princípio da Segregação de Interfaces)

A abstração `SupermarketCatalog`, em `src/catalog.py`, disponibiliza as operações `add_product()` e `unit_price()`.

A implementação analisada não apresenta evidência suficiente de que os consumidores sejam obrigados a depender de operações que não usam. Por isso, a refatoração manterá as abstrações enxutas e avaliará sua segregação somente se novas funcionalidades ampliarem o contrato com operações desnecessárias a parte dos consumidores.

### 4.5 DIP: Dependency Inversion Principle (Princípio da Inversão de Dependência)

Há um ponto de atenção na relação entre `Teller` e o catálogo. Embora o fluxo de checkout use um objeto de catálogo recebido externamente, o método `product_with_name()`, em `src/teller.py` (linhas 28–29), acessa diretamente o atributo `products`:

```python
# src/teller.py, linhas 28–29
def product_with_name(self, name):
    return self.catalog.products.get(name, None)
```

Esse atributo existe em `FakeCatalog`, mas não está declarado no contrato de `SupermarketCatalog`. O acesso direto a uma estrutura específica da implementação reduz a independência em relação à abstração e impede a substituição do catálogo por outra implementação. Como esse acoplamento causa falha real com qualquer catálogo que siga o contrato, ele também foi registrado como defeito (DEF-03, seção 5).

**Sugestão para a refatoração:** disponibilizar a consulta por nome por meio de uma operação definida no contrato do catálogo, evitando o acesso direto a atributos específicos de uma implementação.

### 4.6 Síntese da análise SOLID

| Princípio | Situação | Onde |
|---|---|---|
| SRP | Violação | `ShoppingCart.handle_offers()` |
| OCP | Violação | `ShoppingCart.handle_offers()` e `SpecialOfferType` |
| LSP | Sem violação concreta | `FakeCatalog` / `SupermarketCatalog` |
| ISP | Sem violação concreta | `SupermarketCatalog` |
| DIP | Violação (DEF-03) | `Teller.product_with_name()` |

A avaliação apontou oportunidades de melhoria principalmente nos princípios SRP e OCP, relacionadas à concentração das regras promocionais na classe `ShoppingCart`, além do ponto de atenção do DIP no acesso de `Teller` a um atributo que não faz parte da abstração do catálogo.

Não foram constatadas violações concretas dos princípios LSP e ISP nos trechos examinados. Recomenda-se preservar as características positivas dessas estruturas durante a refatoração, evitando alterações arquiteturais sem necessidade comprovada.

---

## 5. Defeitos encontrados

Além dos problemas de estrutura, o diagnóstico e os testes de caracterização confirmaram três defeitos de comportamento no legado. Esta seção resume cada um. A reprodução completa, a causa raiz, o impacto nos testes e a forma de correção estão no [registro de defeitos](registro_de_defeitos.md).

| ID | Defeito | Onde | Severidade | Quem é prejudicado |
|---|---|---|---|---|
| DEF-01 | "2 por valor" com quantidade ímpar cobra mais do que sem a oferta | `shopping_cart.py`, linha 45 | Alta | Cliente |
| DEF-02 | Nas ofertas por grupo, a fração restante de produtos por peso sai de graça | `shopping_cart.py`, linhas 45, 54–55 e 63–64 | Média | Supermercado |
| DEF-03 | `Teller.product_with_name` acessa `catalog.products`, que não faz parte do contrato do catálogo | `teller.py`, linhas 28–29 | Baixa (latente) | Operação do sistema |

### DEF-01: "2 por valor" com quantidade ímpar

O código usa divisão real (`/`) onde deveria usar divisão inteira (`//`):

```python
# src/shopping_cart.py, linha 45
total = offer.argument * (quantity_as_int / x) + quantity_as_int % 2 * unit_price
#                                          ^ 3 / 2 = 1,5 grupos, quando deveria ser 1
```

Com 3 escovas a R$ 10,00 em "2 por R$ 15,00", o legado cobra 1,5 × R$ 15,00 pelos "pares" e mais R$ 10,00 pela unidade que sobra: **R$ 32,50**, mais caro que os R$ 30,00 sem a oferta. O correto é R$ 25,00. O recibo chega a exibir um "desconto" positivo de R$ 2,50.

Este defeito é consequência direta da duplicação (seção 3.3): as outras duas cópias da fórmula usam `number_of_x`, calculado com `math.floor`, e não têm o problema.

### DEF-02: fração gratuita nas ofertas por grupo

O preço cheio usa a quantidade real (com fração), mas o preço com a oferta usa só a parte inteira para calcular a sobra:

```python
# src/shopping_cart.py, linhas 54–55 (3 por 2); o mesmo padrão aparece nas linhas 45 e 63–64
discount_amount = quantity * unit_price - (
    (number_of_x * 2 * unit_price) + quantity_as_int % 3 * unit_price)
#   \______ usa 3,5 kg ______/                  \__ usa 3 kg: a fração 0,5 some __/
```

Com 3,5 kg de maçã a R$ 2,00/kg em "3 por 2", o legado cobra **R$ 4,00**. O correto é R$ 5,00: os 0,5 kg que sobram não são cobrados. O defeito só se manifesta quando a oferta ativa com quantidade fracionária, e está replicado nas três cópias da fórmula.

### DEF-03: `product_with_name` fora do contrato do catálogo

Ver o código na seção 4.5. Com o catálogo do contrato, a chamada falha:

```python
Teller(SupermarketCatalog()).product_with_name("escova")
# AttributeError: 'SupermarketCatalog' object has no attribute 'products'
```

O defeito é latente: os testes não o detectam porque o `FakeCatalog` tem o atributo `products`.

### Decisão

Os três defeitos serão **corrigidos**, depois da refatoração estrutural, em commits `fix:` isolados, um por defeito, cada um precedido de um teste do comportamento correto. A justificativa e o plano de cada correção estão no [registro de defeitos](registro_de_defeitos.md#3-como-cada-defeito-será-corrigido).

---

## 6. Análise dos padrões de documentação

Também foi avaliada a documentação disponível no projeto, considerando o `README.md` e a documentação presente nos módulos de produção.

O `README.md` apresenta uma descrição geral do sistema e de suas modalidades promocionais, mas não detalha a organização dos componentes, suas responsabilidades e o fluxo de processamento das compras.

Nos módulos de produção não foram encontradas docstrings que expliquem os métodos responsáveis pelas principais regras de negócio. Essa ausência é mais relevante em `shopping_cart.py`, especialmente no método `handle_offers()`, que reúne diferentes critérios de aplicação e cálculo de promoções. Também não há documentação do significado de `Offer.argument` para cada tipo de oferta (seção 3.7).

**Para a refatoração:** o README será complementado com informações sobre a estrutura do projeto, e será adotado um padrão de docstrings, como o PEP 257, para documentar classes, métodos e regras de negócio.

---

## 7. Análise da testabilidade

A análise da suíte original identificou uma cobertura limitada das regras de negócio. O arquivo `tests/test_supermarket.py` tinha apenas um teste automatizado (`test_ten_percent_discount`), que cadastra uma promoção de 10% para a escova de dente, mas adiciona ao carrinho apenas maçãs:

```python
# tests/test_supermarket.py, linhas 17–20
teller.add_special_offer(SpecialOfferType.TEN_PERCENT_DISCOUNT, toothbrush, 10.0)

cart = ShoppingCart()
cart.add_item_quantity(apples, 2.5)
```

Dessa forma, o cenário não exercita a aplicação do desconto anunciado e verifica apenas o cálculo do valor dos produtos sem promoção. Além disso, as comparações usam `pytest.approx(valor, 0.01)`, em que o segundo argumento é uma **tolerância relativa de 1%**: para um total de R$ 4,975, diferenças de até cerca de R$ 0,05 seriam aceitas.

Não havia testes específicos para as demais modalidades promocionais, o que reduzia a segurança para alterar as regras de cálculo. Nenhum dos três defeitos da seção 5 era detectado pela suíte original.

Como ponto positivo, o projeto já tinha o `FakeCatalog` no diretório de testes, que simula o catálogo em memória e evita a dependência de um banco de dados real durante os testes.

**Situação após a caracterização.** A partir deste diagnóstico, a suíte foi ampliada com testes de caracterização (`tests/test_comportamento_legado.py`), que fixam o comportamento atual de cada modalidade promocional, incluindo condições de aplicação e não aplicação dos descontos, quantidades fracionárias, combinações de produtos e o recibo impresso. A suíte passou de 1 para **57 testes, todos passando**, com **99% de cobertura de ramos** em `src/` (as duas linhas não cobertas são as exceções de acesso a banco de `SupermarketCatalog`). A evidência está em [`evidencias/pytest_antes_ubuntu.log`](evidencias/pytest_antes_ubuntu.log).

---

## 8. Síntese do diagnóstico técnico

O diagnóstico identificou como principal ponto de melhoria a concentração das regras promocionais na classe `ShoppingCart`. Essa estrutura, associada à modelagem genérica da classe `Offer`, produz um método longo, com condicionais por tipo, fórmulas duplicadas, números mágicos e nomes pouco expressivos, o que dificulta a manutenção e a inclusão de novas modalidades de promoção.

A duplicação tem consequência concreta: dois dos três defeitos confirmados (DEF-01 e DEF-02) estão nas cópias da fórmula das ofertas por grupo. O terceiro (DEF-03) vem da dependência de `Teller` em relação a um detalhe de implementação do catálogo. Também foram identificados pontos de atenção no encapsulamento das coleções do carrinho, no uso de `float` para valores monetários e na documentação dos componentes.

Quanto à testabilidade, a suíte original tinha cobertura insuficiente das regras de negócio, o que tornou necessária a ampliação dos cenários de teste antes das alterações estruturais. O catálogo falso foi uma base adequada para essa evolução, permitindo validar os comportamentos sem dependência de um banco de dados real.

Diante desses resultados, a refatoração priorizará a separação das regras promocionais, a eliminação da duplicação, a melhoria da distribuição de responsabilidades e a redução do acoplamento entre os componentes. As alterações estruturais serão feitas antes das correções de defeitos, com a suíte de caracterização passando a cada passo, de modo a preservar o comportamento esperado e facilitar a manutenção e a evolução do sistema.

---

## 9. Priorização dos pontos para refatoração

| Prioridade | Ponto de melhoria | Arquivo principal | Seções |
|---|---|---|---|
| Alta | Separação e organização das regras promocionais | `shopping_cart.py` | 3.1, 3.2, 3.4, 3.5, 4.1, 4.2 |
| Alta | Eliminação da fórmula duplicada das ofertas por grupo | `shopping_cart.py` | 3.3 |
| Alta | Melhoria da modelagem de `Offer` | `model_objects.py` | 3.6, 3.7 |
| Alta | Ampliação e correção dos testes das promoções | `tests/` | 7 |
| Alta | Correção de DEF-01 e DEF-02, depois da refatoração estrutural | `shopping_cart.py` | 5 |
| Média | Revisão das dependências e do contrato do catálogo (DEF-03) | `teller.py` / `catalog.py` | 4.5, 5 |
| Média | Nomes expressivos e remoção de números mágicos | `shopping_cart.py` | 3.8, 3.9 |
| Média | Melhoria do encapsulamento das coleções | `shopping_cart.py` | 3.10 |
| Média | Complementação da documentação | `README.md` e módulos de produção | 6 |
| Baixa | `float` para valores monetários: registrar como risco remanescente, sem alterar o tipo | todo o `src/` | 3.11 |
