# Plano de Contingência: Supermarket Receipt Refactoring Kata

## Sumário

1. [Introdução](#1-introdução)
2. [Invariantes de negócio](#2-invariantes-de-negócio)
3. [Interfaces que devem permanecer compatíveis](#3-interfaces-que-devem-permanecer-compatíveis)
4. [Riscos](#4-riscos)
5. [Ordem das alterações](#5-ordem-das-alterações)
6. [Critérios de aceitação](#6-critérios-de-aceitação)
7. [Estratégia de reversão](#7-estratégia-de-reversão)
8. [Decisões registradas](#8-decisões-registradas)

---

## 1. Introdução

Este documento complementa o [diagnóstico técnico](diagnostico_tecnico.md) do projeto Supermarket Receipt Refactoring Kata. O diagnóstico identificou como principais pontos de melhoria a concentração das regras promocionais em `ShoppingCart.handle_offers()`, a duplicação das fórmulas das ofertas por grupo, a modelagem genérica da classe `Offer`, o acesso de `Teller` a um atributo que não faz parte do contrato do catálogo e a cobertura insuficiente de testes. Também foram confirmados três defeitos de comportamento, descritos no [registro de defeitos](registro_de_defeitos.md).

O plano parte de um princípio: **nenhuma alteração no código começa antes de existir uma rede de segurança de testes que fixe o comportamento atual**. A refatoração muda a estrutura sem mudar o comportamento. As únicas mudanças de comportamento são as correções dos defeitos DEF-01, DEF-02 e DEF-03, feitas depois da refatoração estrutural, em commits isolados, e a inclusão dos combos, que é uma funcionalidade nova.

O documento define os invariantes de negócio, as interfaces que devem permanecer compatíveis, os riscos das alterações, a ordem de implementação, os critérios de aceitação, a estratégia de reversão e as decisões tomadas pela equipe. Os requisitos citados estão em [requisitos.md](requisitos.md), e o passo a passo de cada fase está no [plano de execução](plano_de_execucao.md).

---

## 2. Invariantes de negócio

Comportamentos que não devem mudar durante a refatoração. Os invariantes marcados como **[corrigido]** descrevem o comportamento correto de um defeito do legado:

- **Até a Fase 4.6** (refatoração estrutural), vale o comportamento legado, fixado pelos testes de caracterização.
- **A partir das Fases 4.7 a 4.9**, vale o comportamento corrigido.

| # | Invariante | Requisito |
|---|---|---|
| 1 | Produtos sem oferta cadastrada são cobrados por quantidade × preço unitário, sem desconto. | RF03 |
| 2 | O preço unitário vem sempre do catálogo no momento do checkout. Ele não é registrado no carrinho. | RF01 |
| 3 | **3 por 2:** a cada 3 unidades, pagam-se 2. As unidades restantes (resto da divisão por 3) são cobradas a preço cheio. Abaixo de 3 unidades, não há desconto. | RF05 |
| 4 | **Desconto percentual** (`TEN_PERCENT_DISCOUNT`): desconto = quantidade × preço × percentual / 100, aplicado a toda a quantidade do produto e com o percentual cadastrado, que não precisa ser 10%. | RF04 |
| 5 | **2 por valor fixo** **[corrigido: DEF-01]**: a cada par, paga-se o preço promocional, e a unidade restante é cobrada a preço cheio. Com menos de 2 unidades, não há desconto. *No legado, com quantidade ímpar, a sobra é cobrada duas vezes (3 escovas em "2 por R$ 15,00" custam R$ 32,50, e não R$ 25,00).* | RF05 |
| 6 | **5 por valor fixo:** mesma lógica do 2 por valor fixo, com grupos de 5. | RF05 |
| 7 | A oferta se aplica somente ao produto para o qual foi cadastrada. Um novo cadastro para o mesmo produto substitui a oferta anterior. | RF04, RF05 |
| 8 | Quantidades fracionárias (produtos por peso) são cobradas pelo valor exato. Nas ofertas por grupo, o número de grupos usa a parte inteira da quantidade. **[corrigido: DEF-02]** Toda a sobra, unidades e fração, é cobrada a preço cheio. *No legado, a fração que sobra sai de graça (3,5 kg em "3 por 2" custam R$ 4,00, e não R$ 5,00).* | RF03, RF05 |
| 9 | Total do recibo = soma dos itens + soma dos descontos (os descontos são valores negativos). | RF14 |
| 10 | O desconto só é registrado quando a condição de aplicação foi satisfeita. | RF04, RF05 |
| 11 | O recibo lista os itens na ordem de inclusão, uma linha por inclusão. Os descontos de ofertas individuais seguem a ordem da primeira inclusão de cada produto, e os de combo vêm depois. O texto e a formatação do recibo impresso permanecem idênticos. | RF12, RF13 |
| 12 | **Cada unidade recebe no máximo um desconto**, seja de oferta individual, seja de combo. | RF11 |
| 13 | O mesmo carrinho com as mesmas ofertas produz o mesmo recibo, e cada checkout recalcula as promoções. | RF14 |
| 14 | **Cadastrar um combo nunca encarece um carrinho**, em comparação com o mesmo carrinho sem combos no sistema já corrigido. | RF11, RF16 |
| 15 | **[corrigido: DEF-03]** `Teller.product_with_name(name)` funciona com qualquer catálogo que cumpra o contrato de `SupermarketCatalog`. | RNF01 |

### 2.1 Regras de combo

1. Um combo é um conjunto predefinido de produtos e respectivas quantidades. O desconto só é concedido quando o carrinho contém todos os produtos nas quantidades exigidas pelo combo (RF06).
2. O desconto é de **10%** sobre o preço original dos produtos de cada combo completo (RF07). O percentual pode ser informado no cadastro, mas o padrão é 10%.
3. O combo é cadastrado por uma API nova e aditiva, `teller.add_bundle_offer(products, percent=10.0)`. Um produto repetido na lista significa quantidade maior que 1. O enum `SpecialOfferType`, o dicionário `teller.offers` e o método `add_special_offer` não mudam (D5).
4. O número de combos completos é o menor valor de ⌊disponível ÷ exigido⌋ entre os produtos do combo (RF09). A fração de produtos por peso nunca forma combo.

   ```
   nº de combos = menor valor de ⌊disponível[p] / exigido[p]⌋ entre os produtos do combo
   desconto     = 10% × nº de combos × (soma dos preços dos produtos do combo)
   ```

5. As unidades que não completam um combo continuam disponíveis para as ofertas individuais ou são cobradas a preço cheio (RF10).
6. Um carrinho sem nenhum combo completo produz exatamente o mesmo resultado que produziria sem combos cadastrados (RF08, RF11).
7. Quando dois combos disputam o mesmo produto, as unidades vão para o combo cadastrado primeiro (D6, RF11).
8. No recibo, cada produto de um combo gera uma linha de desconto com a descrição `combo 10% off` (D4, RF13). As classes `Discount` e `ReceiptPrinter` não mudam.

### 2.2 Regras de convivência entre combos e ofertas

Um produto pode ter uma oferta individual e também fazer parte de um combo. Dar sempre prioridade ao combo encarece alguns carrinhos: com a escova em "3 por 2", o carrinho 3 escovas + 1 creme custa R$ 35,00 no legado, mas custaria R$ 42,50 se o combo fosse aplicado primeiro. Comparar só os percentuais também engana, porque o combo dá desconto nos dois produtos, inclusive no que não tinha oferta.

Por isso, a equipe adotou a política **"o melhor dos dois, com empate para as ofertas"** (D1, RF16):

1. Cada oferta individual informa quantas unidades **consumiu**:

   | Oferta | Unidades consumidas |
   |---|---|
   | 3 por 2 | `(qtd_inteira // 3) × 3`, se ativou |
   | 2 por valor | `(qtd_inteira // 2) × 2`, se ativou |
   | 5 por valor | `(qtd_inteira // 5) × 5`, se ativou |
   | Percentual | todas as unidades |
   | Oferta que não ativou | 0 |

2. O carrinho é calculado de duas formas:
   - **Ordem C (ofertas → combos):** as ofertas individuais são aplicadas primeiro, como seriam sem combos, e os combos usam só as unidades que sobraram.
   - **Ordem B (combos → ofertas):** os combos são aplicados primeiro, e as ofertas individuais usam só as unidades que sobraram.
3. Fica a ordem de **menor total**. **Em empate, vence a ordem C**, e as ofertas individuais são mantidas no recibo. O combo só substitui uma oferta quando é estritamente melhor para o cliente.
4. Os totais são comparados **em centavos** (arredondados para 2 casas), para que diferenças de ponto flutuante não decidam o desempate.

| Cenário | Ordem C | Ordem B | Escolhida |
|---|---|---|---|
| Escova R$ 10 com "3 por 2" + creme R$ 15; carrinho 3 + 1 | **R$ 35,00** | R$ 42,50 | C (oferta) |
| Escova R$ 10 com 20% + creme R$ 15; carrinho 1 + 1 | R$ 23,00 | **R$ 22,50** | B (combo) |
| Escova R$ 10 com "5 por 40" + creme R$ 15; carrinho 5 + 5 | R$ 115,00 | **R$ 112,50** | B (combo) |
| Escova R$ 10 com "2 por 15" + creme R$ 15; carrinho 2 + 2 | R$ 45,00 | R$ 45,00 | C (empate → ofertas) |

**O que a política garante:**
- **Cadastrar um combo nunca encarece o carrinho.** O total escolhido é menor ou igual ao da ordem C, e a ordem C só acrescenta descontos ao carrinho sem combos.
- **Não há desconto duplicado.** Cada unidade é consumida por uma única política.
- **O recibo só muda quando o combo traz ganho real**, porque em empate as ofertas preexistentes são mantidas.
- **É determinística e testável:** são sempre exatamente dois cálculos.

**Limitação documentada:** a escolha vale para o carrinho inteiro. Não são avaliadas distribuições mistas, como "este combo primeiro, aquela oferta primeiro", porque isso exigiria testar todas as combinações e não é pedido pelo edital (antigo RF15, hoje fora de escopo).

**Regra descartada:** a versão anterior deste plano previa validar, no cadastro, que o desconto do combo fosse ao menos 30% maior que o da oferta individual. A validação foi retirada (antigo RF18), porque faria `add_special_offer` rejeitar ofertas que o legado aceita, o que quebra a interface pública (RNF03). Com a política acima, um combo menos vantajoso simplesmente não é escolhido, sem precisar ser proibido.

---

## 3. Interfaces que devem permanecer compatíveis

Nomes, assinaturas e tipos de retorno destas interfaces não podem mudar. Elas são protegidas pelos testes de contrato da caracterização (RNF03).

| Interface | Quem depende |
|---|---|
| `ShoppingCart.add_item(product)` e `add_item_quantity(product, quantity)` | Todos os testes (início de toda compra) |
| `ShoppingCart.items`, uma lista de `ProductQuantity` | `Teller` (montagem dos itens do recibo) e testes |
| `ShoppingCart.product_quantities`, um dicionário produto → quantidade | Testes |
| `ShoppingCart.handle_offers(receipt, offers, catalog)` | `Teller`. Depois da Fase 4.5, passa a ser uma fachada que delega ao `DiscountEngine` |
| `Teller(catalog)` | Testes e fluxo de checkout |
| `Teller.add_special_offer(offer_type, product, argument)` | Testes de promoção |
| `Teller.offers`, um dicionário produto → `Offer` | Testes de contrato |
| `Teller.checks_out_articles_from(the_cart)`, que devolve um `Receipt` | Testes de caracterização |
| `Teller.product_with_name(name)`, que devolve o produto ou `None` | Testes de contrato. A implementação muda no DEF-03, mas a assinatura e o retorno não |
| `SpecialOfferType` e seus quatro membros atuais, com os mesmos valores | Cadastro de ofertas nos testes |
| `Offer(offer_type, product, argument)` | `Teller` e testes |
| `Product`, `ProductUnit`, `ProductQuantity`, `Discount` | Carrinho, recibo, impressão e testes |
| `Receipt`: `total_price()`, `items`, `discounts`, `add_product()` e `add_discount()` | `Teller`, testes e impressor |
| `ReceiptPrinter.print_receipt(receipt)`, em `tests/receipt_printer.py` | Testes do recibo impresso |
| `SupermarketCatalog.add_product()` e `unit_price()` | `Teller` e `FakeCatalog`. O contrato **ganha** `product_with_name()` no DEF-03, sem perder nenhuma operação |

**Interfaces novas (aditivas):** `Teller.add_bundle_offer(products, percent=10.0)` e as classes internas da refatoração (`DiscountPolicy`, `GroupPriceOffer`, `DiscountEngine` etc.). Elas não substituem nenhuma das interfaces acima.

---

## 4. Riscos

| ID | Risco | Probabilidade | Impacto | Mitigação | Requisitos |
|---|---|---|---|---|---|
| R01 | Registro incorreto de produtos ou quantidades no carrinho. | Baixa | Alto | Testes de caracterização para diferentes quantidades, inclusões repetidas do mesmo produto e produtos por peso. | RF01, RF02 |
| R02 | Cálculo incorreto dos valores dos produtos devido a inconsistências nos preços ou nas quantidades. | Média | Alto | Testes com diferentes preços e quantidades, incluindo produtos vendidos por peso. | RF03 |
| R03 | Aplicação incorreta dos descontos das ofertas individuais ou por grupo. | Média | Alto | Testes específicos para cada modalidade de oferta, nos limites de ativação (abaixo, exatamente no limite e acima). | RF04, RF05 |
| R04 | Identificação incorreta dos combos, considerando os produtos e as quantidades exigidos. | Média | Alto | Testes com carrinhos completos, incompletos e com combo que exige mais de uma unidade do mesmo produto. | RF06 |
| R05 | Cálculo incorreto da quantidade de combos completos ou desconto sobre unidades excedentes. | Média | Alto | Testes com múltiplos combos, excedentes e diferentes proporções entre os produtos. | RF07, RF09, RF10 |
| R06 | Desconto em combos incompletos ou cálculo incorreto do percentual. | Média | Alto | Testar a aplicação do percentual exclusivamente sobre os itens de combos completos. | RF07, RF08 |
| R07 | Conflito entre ofertas individuais e combos, gerando desconto duplicado na mesma unidade, ou disputa entre dois combos pelo mesmo produto. | Média | Alto | Cada política informa as unidades que consumiu, e a seguinte só usa o que sobrou. Testes para produtos com oferta e combo ao mesmo tempo, e para dois combos com produto em comum (ordem de cadastro). | RF11 |
| R08 | Escolha errada entre aplicar combos primeiro ou ofertas primeiro, encarecendo o carrinho ou trocando uma oferta por um combo sem ganho real. | Média | Alto | Política de duas ordens com comparação em centavos e empate para as ofertas. Testes com cenários em que vence a oferta, em que vence o combo e de empate, mais um teste parametrizado de "total com combos ≤ total sem combos". | RF16 |
| R09 | Promoções não recalculadas depois de alterações no carrinho. | Baixa | Alto | O cálculo é refeito a cada checkout, sem cache. Teste que altera o carrinho entre dois checkouts. | RF14 |
| R10 | Correção dos defeitos alterar resultados além dos casos previstos. | Média | Alto | Um teste `xfail(strict=True)` do comportamento correto antes de cada correção, um commit `fix:` por defeito e conferência de que só mudaram os 7 casos listados no [registro de defeitos](registro_de_defeitos.md#4-impacto-nos-testes-de-caracterização). | RF05, RNF01, RNF03 |
| R11 | Apresentação incorreta dos produtos, quantidades, valores e descontos no recibo. | Média | Alto | Testes que comparam o texto completo do recibo impresso, verificando conteúdo, ordem e formatação. | RF12, RF13 |
| R12 | Divergência no valor final do recibo por erro na aplicação dos descontos ou na soma dos valores. | Média | Alto | Testes que comparam a soma dos itens, os descontos aplicados e o total final do recibo. | RF14, RNF09 |
| R13 | A refatoração alterar comportamento ou quebrar uma interface pública. | Média | Alto | Commits pequenos, com a suíte de caracterização inteira passando sem alteração em cada um até a Fase 4.6, mais testes de contrato para as interfaces da seção 3. | RNF03 |

**Riscos remanescentes**, aceitos e documentados, sem mitigação no escopo deste trabalho:
- **Uso de `float` para valores monetários (RNF09).** O tipo é mantido para não alterar valores observados. A impressão arredonda para 2 casas, e a política de combos compara totais em centavos.
- **Escolha global da ordem entre combos e ofertas.** Distribuições mistas não são avaliadas (ver a limitação na seção 2.2).

**Alterações em relação à versão anterior:**
- O antigo R10 tratava da validação de proporcionalidade no cadastro, que foi retirada do escopo (antigo RF18). O ID passou a ser usado para o risco das correções de defeitos.
- O R01 não prevê mais "validações na inclusão de produtos", porque elas mudariam o comportamento do legado e não são pedidas.
- O R13 é novo.

---

## 5. Ordem das alterações

Cada etapa começa somente quando a anterior cumpriu a condição para avançar. Cada uma corresponde a uma fase do [plano de execução](plano_de_execucao.md) e é feita em uma branch própria, integrada por PR à `develop`.

| Ordem | Alteração | Objetivo | Condição para avançar | Riscos atendidos |
|---|---|---|---|---|
| 1 | Criar os testes de caracterização (**concluída**) | Registrar o comportamento atual do sistema, incluindo os defeitos. | 57 testes passando e 99% de cobertura de ramos em `src/` | R01, R02, R03, R09, R11, R12 |
| 2 | Complementar a caracterização (Fase 3) | Fixar recibos de carrinhos sem combos, criar os testes `xfail(strict=True)` dos defeitos e salvar a cobertura de baseline. | Suíte passando, `xfail` falhando como esperado, cobertura salva em `evidencias/testes/` | R10, R13 |
| 3 | Refatoração estrutural (Fases 4.1 a 4.6) | Distribuir as responsabilidades promocionais com Strategy, Template Method e Factory. `handle_offers` vira uma fachada para o `DiscountEngine`, e as políticas passam a informar as unidades consumidas. | Suíte de caracterização passando **sem nenhuma alteração** em cada commit, e `handle_offers` sem `if` por tipo de oferta | R03, R13 |
| 4 | Correção dos defeitos (Fases 4.7 a 4.9) | Corrigir DEF-01, DEF-02 e DEF-03, um commit `fix:` por defeito. | Só os 7 casos previstos mudaram de valor esperado, e não resta nenhum `xfail` | R03, R10 |
| 5 | Implementar os combos (Fase 5) | Incluir `BundleOffer`, `Teller.add_bundle_offer` e a escolha entre as ordens C e B no `DiscountEngine`. | Suíte inteira passando, com `teller.offers` e `SpecialOfferType` inalterados | R04, R05, R06, R07, R08 |
| 6 | Testes de regressão e dos combos (Fase 6) | Cobrir os cenários de combo, de convivência com ofertas e de "nunca encarece", e repetir a caracterização com combos cadastrados para outros produtos. | Suíte passando, logs e cobertura "depois" salvos em `evidencias/testes/` | R04 a R08, R11, R12, R13 |
| 7 | Reavaliação de segurança (Fase 7) | Rodar Bandit, pip-audit e Safety sobre a versão final e comparar com a linha de base. | Comparativo de antes e depois registrado, com riscos remanescentes | (RNF07) |
| 8 | Revisão da documentação (Fases 8 e 9) | Garantir a consistência entre requisitos, código, testes e README. | Rastreabilidade de [requisitos.md](requisitos.md) completa | Todos |

---

## 6. Critérios de aceitação

| Critério de aceitação | Condição de validação | Requisitos |
|---|---|---|
| Preservação das funcionalidades existentes | As funcionalidades anteriores à refatoração continuam apresentando os mesmos resultados para as mesmas entradas, exceto nos casos corrigidos de propósito (DEF-01 e DEF-02). | RNF03 |
| Registro de produtos e quantidades | O sistema permite adicionar produtos e informar suas quantidades, e o preço é consultado no catálogo no checkout. | RF01, RF02 |
| Precisão dos cálculos | O valor de cada produto corresponde ao preço unitário multiplicado pela quantidade adquirida, inclusive em produtos por peso. | RF03 |
| Aplicação das ofertas individuais | Os descontos cadastrados são aplicados quando suas condições são atendidas, e as sobras são cobradas a preço cheio. | RF04, RF05 |
| Identificação e aplicação de combos | O sistema reconhece combos completos e concede 10% de desconto sobre os produtos que os compõem. | RF06, RF07, RF09 |
| Tratamento de combos incompletos e excedentes | Produtos que não integram combos completos permanecem com seus preços originais. | RF08, RF10 |
| Compatibilidade entre promoções | Nenhuma unidade recebe desconto duplicado, e combos que disputam o mesmo produto seguem a ordem de cadastro. | RF11 |
| Escolha entre combo e oferta | O sistema aplica a ordem (combos primeiro ou ofertas primeiro) de menor total, comparado em centavos. Em empate, prevalecem as ofertas individuais, e cadastrar um combo nunca encarece o carrinho. | RF16 |
| Atualização das promoções | Alterar o carrinho e fazer um novo checkout gera um recibo com as promoções recalculadas. | RF14 |
| Consistência do recibo | O recibo apresenta os produtos, quantidades, valores e descontos concedidos, no formato original. | RF12, RF13 |
| Precisão do valor final | O total do recibo corresponde à soma dos valores dos produtos mais os descontos aplicáveis, sem divergência de arredondamento. | RF14, RNF09 |
| Compatibilidade das interfaces | As interfaces da seção 3 continuam funcionando com os mesmos nomes, assinaturas e retornos, e o `Teller` funciona com qualquer catálogo do contrato. | RNF01, RNF03 |
| Correção dos defeitos | DEF-01, DEF-02 e DEF-03 estão corrigidos, cada um em seu commit, com os testes do comportamento correto passando. | RF05, RNF01 |
| Aprovação dos testes | Os testes de caracterização, de regressão e dos combos são executados sem falhas e sem `xfail` pendentes. | RNF05; RF01 a RF14 e RF16 |
| Consistência da documentação | Os requisitos documentados correspondem às regras implementadas e aos comportamentos verificados nos testes. | RF01 a RF14 e RF16 |

---

## 7. Estratégia de reversão

O controle de versão é feito com Git, preservando o código original e registrando as modificações em commits pequenos e organizados por etapa. Assim, é possível identificar a alteração responsável por uma falha e desfazê-la sem perder o restante do trabalho.

**Pontos de restauração e fluxo de trabalho:**
- **Versão estável de referência:** a tag `legado-baseline` (commit `58df732`), publicada no repositório remoto. Ela marca o código legado com a suíte de caracterização completa.
- **Branches:** a `develop` concentra o trabalho integrado. Cada fase é feita em uma branch própria, derivada da `develop` (por exemplo, `refactor/politicas-desconto` e `feat/combos`).
- **Integração:** toda integração com a `develop` é feita por **pull request**, com a suíte passando. Nenhum commit é feito diretamente na `develop` ou na `main`.
- **Commits:** seguem o padrão Conventional Commits (`refactor:`, `fix:`, `feat:`, `test:`, `docs:`, `chore:`), e a suíte inteira passa em cada um. As correções de defeitos ficam em commits `fix:` isolados, para que cada uma possa ser revertida sozinha.

| Situação identificada | Procedimento padrão |
|---|---|
| Falha isolada em uma alteração recente | Reverter o commit responsável com `git revert <commit>`, preservando as demais alterações validadas. |
| Regressão em uma etapa de refatoração, ainda na branch da fase | Voltar a branch ao último commit em que a suíte passava e refazer o passo em commits menores. |
| Regressão detectada depois do merge na `develop` | Reverter o merge do PR com `git revert -m 1 <merge>` e corrigir na branch da fase antes de abrir um novo PR. |
| Correção de defeito com efeito além do previsto | Reverter apenas o commit `fix:` do defeito (`git revert`), reabrir o teste `xfail` correspondente e revisar a correção. |
| Incompatibilidade entre os combos e as regras existentes | Reverter os commits da Fase 5. A refatoração e as correções continuam válidas, porque estão em fases anteriores. |
| Falhas generalizadas que comprometam o funcionamento do sistema | Criar uma branch a partir da tag `legado-baseline` (`git switch -c recuperacao legado-baseline`) e reaplicar as fases validadas. |
| Depois de qualquer reversão | Executar novamente a suíte completa (caracterização, regressão e combos) e salvar o log para confirmar a recuperação do comportamento esperado. |

---

## 8. Decisões registradas

| ID | Decisão | Escolha | Justificativa |
|---|---|---|---|
| D1 | Convivência entre combo e oferta | O melhor dos dois, com empate para as ofertas (ordem C), comparando em centavos | Nunca encarece, evita duplicidade e mantém as ofertas preexistentes quando o combo não traz ganho, em linha com o "manter o comportamento anterior" do edital |
| D2 | Defeitos confirmados do legado: DEF-01 ("2 por valor" com quantidade ímpar cobra a mais), DEF-02 (fração gratuita nas ofertas por grupo) e DEF-03 (`product_with_name` fora do contrato do catálogo) | Corrigir todos | DEF-01 cobra do cliente mais do que sem a oferta, DEF-02 concede desconto maior que o anunciado, e DEF-03 quebra com qualquer catálogo real. Detalhes no [registro de defeitos](registro_de_defeitos.md) |
| D3 | Momento e forma das correções | Depois da refatoração estrutural, um commit `fix:` por defeito, cada um precedido de um teste `xfail(strict=True)` do comportamento correto | Separa mudança de estrutura de mudança de comportamento. Com o Template Method, DEF-01 e DEF-02 são corrigidos em um único lugar, e cada correção pode ser revertida isoladamente |
| D4 | Representação do combo no recibo | Um `Discount` por produto do combo, com a descrição `combo 10% off` | Não muda a classe `Discount` nem o `ReceiptPrinter` |
| D5 | API do combo | Nova e aditiva: `teller.add_bundle_offer(products, percent=10.0)` | Não altera o enum `SpecialOfferType`, o `teller.offers` nem o `add_special_offer`, que são protegidos por testes de contrato |
| D6 | Dois combos disputando o mesmo produto | Vence a ordem de cadastro | Simples, determinístico e testável |
