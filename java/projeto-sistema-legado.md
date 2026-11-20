# Projeto: Evolução e Modernização de um Sistema Legado

![Logotipo da instituição](img/logo.png)

## Etapa 1: Escolha e Descrição do Sistema Legado (Peso: 1,0 ponto)

### Formação das Equipes e Regras de Colaboração

#### Apresentação da Equipe

| Nome | Matrícula | E-mail | Usuário GitHub |
|---|---|---|---|
| Sérgio Lopes Júnior | 200026817 | sergio.lopes@ucsal.edu.br | sergiolj |

#### Origem do Sistema Legado

O sistema legado escolhido faz parte do acervo de projetos para refatoração criados pela consultora independente e desenvolvedora de software Emily Bache. Emily é graduada em Engenharia pela Universidade de Cambridge no Reino Unido e  trabalha com consultoria com foco em refatoração contínua e técnicas como TDD (Test Driven Design).

Um dos algoritmos de refatoração disponíveis no github da autora (https://github.com/emilybache/) foi escolhido pela equipe para o desenvolvimento deste trabalho. Trata-se do algoritmo SupermarketReceipt-Refactoring-Kata (https://github.com/emilybache/SupermarketReceipt-Refactoring-Kata). O algoritmo possui versões em diversas linguagens de programação como cpp, csharp, php, Kotlin, Python e Java, entre outras.

A linguagem de programação escolhida foi o Java.

#### Ficha Técnica do sistema

- **Nome:** SupermarketReceipt-Refactoring-Kata
- **Fonte:** https://github.com/emilybache/SupermarketReceipt-Refactoring-Kata
- **Objetivo:** Simular um sistema de supermercado com grande rotatividade de ofertas de produtos além de grande variedade de promoções baseadas em diferentes tipos de ofertas como percentuais de desconto no valor dos produtos, promoções de compre três pelo preço de dois ou compre cinco produtos por um valor determinado, entre outras que devem ser criadas como novos recursos.
- **Tecnologias:** Java 17 e Junit 5.11.4
- **Justificativa:** O projeto foi desenvolvido com uma arquitetura procedural disfarçada de orientação a objetos. Embora possua objetos característicos de OO esses possuem forte acoplamento, métodos longos e com vários if-else encadeados, assim como baixo encapsulamento e vários objetos apenas como records ou classes anêmicas e sem inteligência, concentrando toda a regra de negócio em umas poucas classes. Além disso usa variáveis primitivas como double para representar valores monetários, o que dificulta a precisão e testes e cria erros de arredondamento. Além disso, os nomes de métodos e atributos são pouco expressivos e estão em língua inglesa o que dificulta a compreensão do fluxo do código.
- **Exemplos de code smell**
  - Método **ShoppingCart.handle.Offers** concentra toda a lógica do cálculo de promoções em um único bloco if/else if criando uma alta complexidade ciclomática
  - Atributo double **unitPrice** primitivo para representar e manipular dinheiro criando erros de arredondamento
  - Atributo double **quantity** primitivo para representar tanto unidades inteiras como representações de peso
  - Classes anêmicas como **Product, ProductQuantity, Offer e Discount** que não possuem nenhuma inteligência ou regra de negócio.

## Etapa 2: Engenharia Reversa e Diagnóstico Técnico (Peso: 2,0 pontos)

### Mapeamento da arquitetura atual e identificação de módulos/componentes.

Segundo uma pesquisa usando o [Claude.ai](http://Claude.ai) existem diversas técnicas disponíveis para o mapeamento de arquitetura e identificação de módulos em softwares legados. Alguns métodos, voltados para sistemas maiores utilizam ferramentas de software específicas como o SonarQube/SonarLint, PMD e CK (no ambiente Java), essas ferramentas medem a complexidade ciclomática e cognitiva através de métodos e métricas próprias e geram relatórios quantitativos além de permitirem a comparação do antes e depois da refatoração, o problema, nesse caso, é que nosso projeto é pequeno com algumas poucas classes e linhas de código o que irá gerar resultados pouco expressivos com a maioria das classes aparecendo como de baixa complexidade e umas poucas de alta, fato que pode ser percebido apenas com a leitura do código e análise estática manual, que é uma das técnicas mais básicas.

A técnica adotada deve ser adequada ao tamanho do problema. Sendo assim foram consideradas as seguintes abordagens: 1- Leitura de código e análise estática manual, 2- Recuperação de arquitetura por modelos (usando diagramas UML via engenharia reversa), 3- Cartões CRC(Class-Responsibility-Collaborator), 4- Detecção de code smells (catálogo de Fowler - Refactoring) e 4- Análise Dinâmica de testes de caracterização.

Existem diversas outras técnicas, mas as listadas acima parecem se adequar mais ao tamanho do problema apresentado. Dessas as abordagens adotadas foram a 1, 2, 3 e a 4 (Relatório de Débito Técnico) que foi incluída como parte do passo 5 da etapa 1, além da técnica 5 será executada diretamente no repositório a ser refaturado para dar segurança a integridade do algoritmo durante a refatoração.

#### 3.1 Leitura do código e análise manual

Para a leitura manual do código poder ser rastreável é importante a definição de um roteiro a ser seguido com passos definidos e claros. O roteiro escolhido abrange cinco passos com a produção de artefatos específicos a cada passo assim caracterizados:

**Passo 1:**

**Preparação do roteiro** - O repositório original foi clonado e foram executados os testes automatizados disponíveis para verificar seu funcionamento, a seguir foi feito o levantamento bruto dos arquivos.

**Registro:**

Versão Java, branch main, commit c72d68abd77a417f84193b72a3b8d14177336066

Repositório possui:

- 11 classes em apenas um package model (src/main/java/dojo.supermarket.model)
- 1 classe de teste (src/test/java/dojo.supermarket.model)
- 1 classe de implementação de interface (src/test/java/dojo.supermarket.model)
- 1 classe de funcionalidade de impressão (src/test/java/dojo.supermarket)
- 1 classe de relatório sem funcionalidade ((src/test/java/dojo.supermarket)
- Número total de linhas de código: 345

**Passo 2:**

**Leitura panorâmica** - Visualização e registro inicial das funcionalidades de cada classe e seu nível de complexidade ou primeiras impressões

**Registro:**

> **Legenda:** 🟦 record · 🟥 enum · 🟨 classe · 🟪 interface · 🟩 classe de teste
> *(no documento original, as linhas são coloridas por tipo)*

| **Classe** | **Tipo** | **Funcionalidade** | **Primeiras impressões** |
|---|---|---|---|
| `Discount` | 🟦 record | armazena produto e desconto | Simples e sem responsabilidades |
| `Offer` | 🟦 record | armazena produto e tipo de oferta | Simples e sem responsabilidades |
| `Product` | 🟦 record | armazena produto e tipo de unidade | Simples e sem responsabilidades |
| `ProductQuantity` | 🟦 record | armazena produto e quantidade | Simples e sem responsabilidades |
| `ProductUnit` | 🟥 enum | classifica o tipo da unidade | Simples e sem responsabilidades |
| `Receipt` | 🟨 classe | armazena itens e descontos e calcula totais da nota fiscal | Média complexidade com responsabilidades confusas |
| `ReceiptItem` | 🟦 record | armazena produto quantidade, preço unitário e total | Simples e sem responsabilidades |
| `ShoppingCart` | 🟨 classe | processa lista de produtos e descontos | Alta complexidade com responsabilidades confusas |
| `SpecialOfferType` | 🟥 enum | classifica tipos de ofertas | Simples e sem responsabilidades |
| `SuperMarketCatalog` | 🟪 interface | define métodos para um catálogo de produtos | Simples |
| `Teller` | 🟨 classe | recebe um catálogo de produtos e emite uma nota fiscal | Média complexidade e com responsabilidades confusas |
| `PackageSettings` | 🟨 classe | arquivo de configuração para um tipo de relatório de testes | Simples e sem implementação |
| `ReceiptPrinter` | 🟨 classe | processa a nota fiscal para gerar uma saída através de métodos encadeados | Alta complexidade pelo encadeamento de métodos (Method Chaining code smell) |
| `FakeCatalog` | 🟩 classe (test) | implementador da interface SuperMarketCatalog usado para os testes | Simples |
| `SuperMarketTest` | 🟩 classe (test) | testa o resultado da nota fiscal após os produtos serem processados por outras classes | Média complexidade e com apenas um teste de oferta implementado. |

**Passo 3:**

**Encontrar o ponto de entrada -** Compreender como se dá o fluxo de dados no algoritmo, onde se inicia.

**Registro:**

No algoritmo observado não existe um main e os testes concentram o que se pode observar do fluxo esperado para o sistema, mais especificamente a classe SupermarketTest.

**Passo 4:**

**Leitura guiada de um cenário concreto** - Como no exemplo existe apenas um teste implementado e não há outras classes que implementam fluxo o registro irá se concentrar nesse teste.

<table>
  <thead>
    <tr><th rowspan="2">#</th><th colspan="2">Processo acompanhado</th><th rowspan="2">Observação</th></tr>
    <tr><th>Onde acontece (Classe.Método)</th><th>O que acontece</th></tr>
  </thead>
  <tbody>
    <tr><td>1</td><td><code>SupermarketTest.tenPercentDiscount</code></td><td>cria um catálogo através da classe FakeCatalog(), cria dois produtos: pasta de dente preço 0.99 e maçãs com preço 1.99 e adiciona ambos ao catálogo</td><td></td></tr>
    <tr><td>2</td><td><code>Teller</code></td><td>cria uma caixa registradora passando como argumento o catálogo com os produtos</td><td></td></tr>
    <tr><td>3</td><td><code>Teller.addSpecialOffer</code></td><td>adiciona à caixa registradora uma oferta especial com tipo da oferta, o produto em oferta e um argument (double) que são adicionados em uma interface Map HashMap</td><td>O nome do atributo argument ficou confuso. Não fica claro sua função.</td></tr>
    <tr><td>3</td><td><code>ShoppingCart</code></td><td>cria o carrinho de compras e adiciona o produto maçã com a quantidade (double 2.5)</td><td></td></tr>
    <tr><td>4</td><td><code>Receipt</code></td><td>cria a nota fiscal passando como referência o método da caixa registradora que processa o carrinho de compras</td><td></td></tr>
    <tr><td>4.1</td><td><code>teller.checkOutArticlesFrom(ShoppingCart)</code></td><td>a classe nota fiscal irá receber a referência da  nota fiscal de retorno do método da caixa registradora tendo como base o carrinho de compras</td><td></td></tr>
    <tr><td>4.2</td><td><code>theCart.handleOffers(Receipt, Map&lt;Product, Offers&gt;, SupermarketCatalog)</code></td><td>O carrinho de compras recebe a nota fiscal, a lista de ofertas e o catálogo de produtos para processamento</td><td>O carrinho de compras é que vai processar as ofertas através de if/else encadeados fazendo alterações diretamente na nota fiscal violando princípio da responsabilidade única.</td></tr>
    <tr><td>4.3</td><td><code>teller.checkOutArticlesFrom(ShoppingCart)</code></td><td>a classe nota fiscal recebe a referência da nota fiscal de retorno processada pela classe carrinho</td><td></td></tr>
    <tr><td>5</td><td><code>assertEquals</code> <code>receipt.getTotalPrice</code></td><td>testa o valor calculado do preço total sem descontos</td><td>double para valor dificulta o teste de arredondamento e demanda o uso de um valor de arredondamento</td></tr>
    <tr><td>6</td><td><code>ReceiptPrinter.print Receipt(Receipt)</code></td><td>não aparece no teste mas a finalização do fluxo seria com a impressão da nota fiscal</td><td></td></tr>
  </tbody>
</table>

**Passo 5:**

**Leitura aprofundada de cada classe e método** - O método mais complexo do algoritmo está na classe ShoppingCart que processa as ofertas, chamado de ShoppingCart.handleOffers, mas todas as classes foram avaliadas para verificar alguma inconsistência que posteriormente foi categorizada com base no Refactoring de Fowler.

| **ID** | **Local** | **Achado** | **Categoria** | **Confirmar com** |
|---|---|---|---|---|
| L-01 | `Offer` double argument | `argument`: nome do atributo não deixa claro o que representa |  |  |
| L-02 | `ShoppingCart.addItem` (linha 18) | Método específico para adicionar um produto com apenas uma unidade (não parece fácil identificar o que faz) |  |  |
| L-03 | `ShoppingCart.addItemQuantity` (linha 26) | quantidade de produtos com primitiva em double tanto para unidade quanto para volume |  |  |
| L-04 | `ShoppingCart.handleOffers` (linha 35) | Método longo com muitos if/else encadeados |  |  |
| L-05 | `ShoppingCart.handleOffers` (linha 41) | casting da quantidade em double para int |  |  |
| L-06 | `ShoppingCart.handleOffers` (linha 43) | atributo com nome genérico primitiva com o nome “x” |  |  |
| L-07 | `ShoppingCart.handleOffers` (linha 44-45/55-56) | processamento da oferta THREE_FOR_TWO FIVE_FOR_AMOUNT de difícil leitura, aparentam não estar implementadas |  |  |
| L-08 | `ShoppingCart.handleOffers` (linha 50) | atributo com nome argument é usado para determinar o total do valor dos produtos |  |  |
| L-09 | `ShoppingCart.handleOffers` (linha 50) | `quantityAsInt` / `x`: nomes de atributos não deixam claro o que representam |  |  |
| L-10 | `ShoppingCart.handleOffers` (linha 52) | pelo modo de concatenação percebe-se que o atributo argument parece ser um valor fixo passado na oferta |  |  |
| L-11 | `ShoppingCart.handleOffers` (linha 52) | Atributo com nome genérico numberOfXs |  |  |
| L-12 | `ShoppingCart.handleOffers` (linha 70) | Se o desconto não for nulo a nota fiscal recebe a informação sobre o desconto se não ela recebe um objeto nulo? |  |  |
| L-13 | `Product` e `ProductQuantity` | Não parece fazer sentido esses dois records para a mesma coisa |  |  |
| L-14 | `Receipt` (linha 9) | Porque a classe de objetos que representa os produtos na nota fiscal se chama ReceiptItem ao invés de Product? |  |  |
| L-15 | `Receipt.getTotalPrice()` (linha 12-15) | Porque o preço total é o resultado da soma de todos os produtos “mais” a soma de todos os descontos? O desconto é um valor negativo? |  |  |
| L-16 | `Receipt.addProduct()` (linha 17-19) | O método recebe muitos atributos que são convertidos para criar um novo objeto da classe não seria mais fácil receber um ReceitpItem? |  |  |
| L-17 | `ReceiptItem` | importe desnecessário |  |  |
| L-18 | `Teller.addSpecialOffer` (linhas 16-17) | A caixa tem que receber a informação de que existe uma oferta de um produto, mas isso não deveria ser parte do catálogo, porque assim cada caixa tem que receber todas as ofertas uma a uma. |  |  |

#### 3.2 Recuperação de arquitetura por modelos

Foi utilizado o recurso de exibição de diagramas de classes no Intellij para representar todas as classes, seus atributos e métodos.

![Diagrama de classes gerado no IntelliJ](img/diagrama-classes.png)

#### 3.3 Cartões CRC (Class-Responsibility-Collaborator)

Os cartões CRC foram criados por Kent Beck e Ward Cunningham em 1989 e servem para complementar os diagramas de UML. Cada cartão mostra o que a classe faz e não apenas suas estruturas de atributos e métodos assim como as interações entre elas. Cada classe deve ser representada por um cartão com três campos: o nome, as responsabilidades (o que ela sabe e faz) e os colaboradores (as classes de que precisa para cumprir essas responsabilidades).

Embora a técnica tenha sido criada para projetar sistemas, ela pode ser usada no sentido inverso para representar o estado atual do código, auxiliando na compreensão de sua estrutura e expondo acoplamentos e comportamentos diversos do que os esperados para cada classe.

Ao olhar para cada cartão você enxerga visualmente o problema arquitetural sem precisar ler o código: ShoppingCart tem colaboradores demais e responsabilidades visivelmente misturadas, enquanto Offer não tem responsabilidade nenhuma.

| **Teller - Responsabilidades** | **Colaboradores** |
|---|---|
| Armazena ofertas especiais cadastradas | Offer, Product |
| Armazena o catálogo de produtos do supermercado | SupermarketCatalog |
| Orquestra o processo de checkout de um carrinho e gera um nota fiscal | SupermarketCatalog, ShoppingCart, Receipt |

| **Discount - Responsabilidades** | **Colaboradores** |
|---|---|
| Estrutura dados de descontos | Product |

| **Offer - Responsabilidades** | **Colaboradores** |
|---|---|
| Estrutura dados de ofertas | SpecialOfferType, Product |

| **Product - Responsabilidades** | **Colaboradores** |
|---|---|
| Estrutura dados de produtos | ProductUnit |

| **ProductQuantity - Responsabilidades** | **Colaboradores** |
|---|---|
| Estrutura dados de quantidade de produtos | Product |

| **ProductUnit - Responsabilidades** | **Colaboradores** |
|---|---|
| Classifica dados de unidades de produtos | Nenhum |

| **Receipt - Responsabilidades** | **Colaboradores** |
|---|---|
| Armazena lista de descontos | Discount |
| Armazena lista de produtos da nota fiscal | ReceiptItem |
| Calcula a soma de preços de produtos e descontos e retorna o total da nota | ReceiptItem, Discount |

| **ReceiptItem - Responsabilidades** | **Colaboradores** |
|---|---|
| Estrutura os dados de produtos da nota fiscal | Product |

| **ShoppingCart - Responsabilidades** | **Colaboradores** |
|---|---|
| Armazena lista com quantidade de produtos | ProductQuantity |
| Armazena lista de produtos e suas quantidades | Product |
| Possui a regra de negócio para a aplicação das ofertas | Receipt, SupermarketCatalog, SpecialOfferType, Discount, Product |

| **SpecialOfferType - Responsabilidades** | **Colaboradores** |
|---|---|
| Classifica tipos de ofertas | Nenhum |

| **SupermarketCatalog - Responsabilidades** | **Colaboradores** |
|---|---|
| Define a interface para uma estrutura de catálogo de produtos | Nenhum |

| **ReceiptPrinter - Responsabilidades** | **Colaboradores** |
|---|---|
| Imprime a nota fiscal | Receipt, Discount, ReceiptItem, ProductUnit |
| Imprime um produto | ReceiptItem |
| Imprime o desconto | Discount |
| Imprime o total da nota | Receipt |

## Etapa 3: Refatoração Prática e Evolução Aplicada (Obrigatória) (Peso: 3,0 pontos)

- **Entregáveis:**
  - **Execução Obrigatória da Refatoração:** Repositório refatorado funcional com contribuições registradas de todos os membros no GitHub.
  - Plano de atualização tecnológica e estratégias para evolução futura (ex: quebra de monolito, migração de banco, etc.).
  - Plano de modularização e melhorias de legibilidade/complexidade.
  - **Evidências de Código:** Trechos de código comparativos no padrão **"Antes" vs. "Depois"** com a devida explicação técnica das melhorias e testes automatizados.

## Etapa 4: Relatório Técnico Formal (Peso: 2,0 pontos)

- **Diretrizes Estritas:**
  1. Formatação seguindo as normas da ABNT.
  2. Extensão mínima de **8 páginas** com linguagem técnica, formal e objetiva.
- **Estrutura Obrigatória:**
  1. Capa e Introdução
  2. Fundamentação Teórica (Conceitos de evolução, refatoração, etc.)
  3. Desenvolvimento (Diagnóstico do sistema + Refatoração aplicada)
  4. Conclusão e Referências

## Etapa 5: Apresentação Final e Demonstração (Peso: 1,0 ponto)

- **Formato:** Presencial.
- **Tempo Limite:** Entre 10 e 15 minutos.
- **Requisito Obrigatório:** Demonstração prática do código refatorado rodando em comparação com a versão legada inicial (este pode ser em video)

## 4. Matriz de Avaliação (Resumo)

A nota final será calculada estritamente com base nos seguintes critérios:

| **Critério de Avaliação** | **Foco Principal** | **Pontuação Máxima** |
|---|---|---|
| Escolha e Análise do Sistema | Contextualização e Justificativa do Legado | 1,0 |
| Engenharia Reversa e Diagnóstico | Identificação de Problemas Estruturais - Analise de requisitos do sistema legado | 2,0 |
| Refatoração Obrigatória e Evolução | Qualidade, Execução Prática e Registro de Contribuição no GitHub por todos os membros | 3,0 |
| Viabilidade e Análise Crítica | Visão de Negócio, Prazo, Riscos | 1,0 |
| Relatório Técnico | Rigor Acadêmico (ABNT), escrita e estrutura (Mín. 8 Páginas) - Mostrando o que foi alterado e melhorado | 2,0 |
| Apresentação e Demonstração | Postura, Clareza, Gestão de Tempo e Demonstração Prática | 1,0 |
| **TOTAL** |  | **10,0** |

## Referências (sugestão)

- FOWLER, Martin. *Refatoração: Aperfeiçoando o Design de Código Existente*. 2. ed. São Paulo: Novatec Editora, 2020.
  - *Uso no projeto:* Base conceitual para a Etapa 3 (técnicas de refatoração, padronização "Antes vs. Depois" e catálogo de refatorações).
- MARTIN, Robert C. *Código Limpo: Habilidades Práticas do Agile Software*. Rio de Janeiro: Alta Books, 2011.
  - *Uso no projeto:* Identificação de *code smells*, nomenclatura, funções pequenas e boas práticas de legibilidade no desenvolvimento.
- FEATHERS, Michael. *Trabalho Eficaz com Código Legado*. Porto Alegre: Bookman, 2013.
  - *Uso no projeto:* Estratégias para cobrir código antigo com testes de regressão antes de refatorar (essencial para a Etapa 3 e 4).
- SOMMERVILLE, Ian. *Engenharia de Software*. 10. ed. São Paulo: Pearson Education do Brasil, 2019.
  - *Uso no projeto:* Fundamentação teórica da Etapa 4 sobre evolução de software, reengenharia, manutenção preventiva e débito técnico.
  - Link: Pearson Brasil
