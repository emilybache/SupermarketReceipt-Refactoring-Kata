# Requisitos

## Sumário

1. [Convenções](#1-convenções)
2. [Requisitos funcionais](#2-requisitos-funcionais)
3. [Requisitos não funcionais](#3-requisitos-não-funcionais)
4. [Fora de escopo e requisitos removidos](#4-fora-de-escopo-e-requisitos-removidos)
5. [Rastreabilidade](#5-rastreabilidade)
6. [Histórico de alterações](#6-histórico-de-alterações)

---

## 1. Convenções

- **IDs estáveis.** Os IDs não são renumerados nem reaproveitados. Os requisitos removidos (RF15, RF17 e RF18) estão na [seção 4](#4-fora-de-escopo-e-requisitos-removidos), para que as referências antigas continuem rastreáveis.
- **Valores de exemplo.** Os critérios usam os preços dos testes (`tests/conftest.py`): escova a R$ 10,00, creme dental a R$ 15,00 e maçã a R$ 2,00/kg.
- **Comportamento corrigido.** Os critérios marcados com **[DEF-0X]** descrevem o comportamento **corrigido** de um defeito do legado (ver o [registro de defeitos](registro_de_defeitos.md)). Até a Fase 4.6 do plano, os testes de caracterização fixam o comportamento legado. Esses critérios passam a valer a partir das Fases 4.7 a 4.9.
- **Verificação:** *Teste* (teste automatizado), *Inspeção* (revisão do código ou de documentos) e *Análise* (raciocínio ou cálculo sobre os resultados).

---

## 2. Requisitos funcionais

| ID | Descrição | Critério de aceitação | Verificação |
|---|---|---|---|
| RF01 | Permitir adicionar produtos ao carrinho. | O produto adicionado consta nos itens do carrinho. O preço **não** é registrado no carrinho: o preço unitário é consultado no catálogo no momento do checkout. | Teste |
| RF02 | Permitir informar a quantidade de cada produto adquirido. | A quantidade informada é considerada no processamento da compra. Inclusões separadas do mesmo produto somam para o cálculo das ofertas, e o recibo lista uma linha por inclusão. | Teste |
| RF03 | Calcular o valor total de cada produto a partir do preço unitário e da quantidade. | Valor calculado = preço unitário × quantidade. Produtos por peso são cobrados pelo valor exato da fração: 2,5 kg de maçã custam R$ 5,00. | Teste |
| RF04 | Aplicar o desconto percentual cadastrado ao produto da oferta. | O desconto incide sobre toda a quantidade do produto, sem quantidade mínima: 3 escovas com 20% geram desconto de R$ 6,00 (total R$ 24,00). A oferta não afeta outros produtos do carrinho. | Teste |
| RF05 | Aplicar as ofertas por grupo (3 por 2, 2 por valor e 5 por valor) a cada grupo completo de unidades, cobrando a preço cheio tudo o que sobrar. | **3 por 2:** 7 escovas custam R$ 50,00; 2 escovas não ativam a oferta. **2 por valor:** 4 escovas em "2 por R$ 15,00" custam R$ 30,00; 1 escova não ativa a oferta. **[DEF-01]** 3 escovas em "2 por R$ 15,00" custam R$ 25,00 (a unidade que sobra é cobrada a preço cheio). **5 por valor:** 12 escovas em "5 por R$ 40,00" custam R$ 100,00; 4 escovas não ativam a oferta. **[DEF-02]** Nas três ofertas, a fração que sobra de produtos por peso é cobrada a preço cheio: 3,5 kg de maçã em "3 por 2" custam R$ 5,00. | Teste |
| RF06 | Identificar combos cujos produtos e quantidades estejam no carrinho. | O combo só é completo quando todos os produtos exigidos estão nas quantidades previstas. | Teste |
| RF07 | Conceder 10% de desconto sobre os produtos de cada combo completo. | Escova R$ 10,00 + creme dental R$ 15,00 gera desconto de R$ 2,50. O percentual pode ser informado no cadastro do combo, mas o padrão é 10%. | Teste |
| RF08 | Manter preços originais quando o combo estiver incompleto. | Escova sem creme dental não gera desconto. | Teste |
| RF09 | Identificar quantos combos completos podem ser formados no carrinho. | 2 escovas + 2 cremes dentais formam 2 combos. | Teste |
| RF10 | Manter preço original das unidades que não completam outro combo. | 2 escovas + 1 creme dental: só 1 escova e 1 creme recebem desconto. | Teste |
| RF11 | Preservar ofertas preexistentes e evitar desconto duplicado nas mesmas unidades. | Cada unidade recebe no máximo um desconto, de oferta ou de combo. Um carrinho sem combo completo gera o mesmo recibo que geraria sem combos cadastrados. Quando dois combos disputam o mesmo produto, as unidades vão para o combo cadastrado primeiro. A escolha entre combo e oferta segue o RF16. | Teste e Análise |
| RF12 | Exibir no recibo produtos, quantidades e valores. | Todos os produtos do carrinho aparecem no recibo, no formato original. | Teste e Inspeção |
| RF13 | Exibir no recibo os descontos concedidos. | Descontos concedidos aparecem no recibo no formato original. Cada produto de um combo gera uma linha de desconto com a descrição `combo 10% off`. | Teste e Inspeção |
| RF14 | Calcular o total considerando preços e descontos aplicáveis. | Total = soma dos itens + soma dos descontos, sem duplicidade. O cálculo é refeito a cada checkout: alterar o carrinho e fazer novo checkout gera um recibo com as promoções recalculadas. | Teste e Análise |
| RF16 | Quando combo e oferta individual disputarem as mesmas unidades, aplicar a ordem (combos primeiro ou ofertas primeiro) que resultar no menor total. | A comparação é feita em centavos, e em empate prevalecem as ofertas individuais. Com a escova em "3 por 2", 3 escovas + 1 creme custam R$ 35,00 (vencem as ofertas). Com a escova em 20%, 1 escova + 1 creme custam R$ 22,50 (vence o combo). Com a escova em "2 por R$ 15,00", 2 escovas + 2 cremes custam R$ 45,00 nas duas ordens, e a oferta é mantida no recibo. Cadastrar um combo nunca encarece o carrinho. | Teste e Análise |

---

## 3. Requisitos não funcionais

| ID | Descrição | Critério de aceitação | Verificação |
|---|---|---|---|
| RNF01 | Permitir manutenção sem alterar componentes não relacionados. | A inspeção mostra separação de responsabilidades e ausência de dependências desnecessárias. **[DEF-03]** O `Teller` funciona com qualquer catálogo que cumpra o contrato de `SupermarketCatalog`, inclusive na busca de produto por nome. | Inspeção e Teste |
| RNF02 | Permitir novas modalidades de oferta sem reescrever a precificação. | Os combos entram preservando as funções anteriores e sem reconstruir o processamento de preços. | Inspeção e Análise |
| RNF03 | Preservar resultados e contratos das interfaces públicas existentes. | Os testes de caracterização continuam passando sem alteração após a refatoração estrutural. Só mudam os casos alterados de propósito pelas correções de DEF-01 e DEF-02, listados no [registro de defeitos](registro_de_defeitos.md#4-impacto-nos-testes-de-caracterização). | Teste |
| RNF04 | Produzir cálculos consistentes em qualquer combinação de produtos e promoções. | Cenários sem oferta, com oferta individual, por quantidade e com combos dão os resultados esperados. | Teste e Análise |
| RNF05 | Permitir verificação automatizada para detectar regressões. | Há testes automatizados cobrindo cenários normais, limites e combinações de promoções. | Teste e Inspeção |
| RNF06 | Manter padrão consistente de nomenclatura, organização e escrita. | A inspeção mostra uso consistente das convenções adotadas. | Inspeção |
| RNF07 | Apresentar resultados das análises de segurança do código e das dependências. | Resultados de Bandit, pip-audit e Safety registrados antes e depois, com classificação, ações e limitações. | Inspeção e Análise |
| RNF08 | Permitir reproduzir instalação, configuração e execução em ambiente compatível. | O README permite instalar dependências e rodar os testes em outro ambiente. | Teste e Inspeção |
| RNF09 | Preservar a precisão monetária e as regras de arredondamento originais. | Valores calculados e exibidos nos recibos sem divergência de precisão ou arredondamento. O tipo `float` é mantido para não alterar valores observados, e sua imprecisão fica registrada como risco remanescente. | Teste e Análise |

---

## 4. Fora de escopo e requisitos removidos

| ID | Texto original | Situação | Justificativa |
|---|---|---|---|
| RF15 | Avaliar as combinações possíveis de ofertas e combos aplicáveis aos produtos do carrinho. | Fora de escopo | Avaliar todas as distribuições possíveis entre ofertas e combos aumentaria a complexidade sem exigência do edital. A política adotada (RF16) compara exatamente duas ordens para o carrinho inteiro, o que é determinístico, testável e nunca encarece o carrinho. Ver a *Política de convivência* no [plano de execução](plano_de_execucao.md#política-de-convivência-entre-combos-e-ofertas). |
| RF17 | Recalcular a combinação de promoções sempre que os produtos ou suas quantidades forem alterados no carrinho. | Incorporado ao RF14 | O legado já recalcula as ofertas a cada checkout (teste `test_alterar_o_carrinho_depois_de_um_checkout_recalcula_as_ofertas`). O comportamento virou critério de aceitação do RF14. |
| RF18 | Impedir o cadastro de ofertas ou combos que não atendam à proporcionalidade mínima de descontos (relação inferior a 1,5). | Fora de escopo | A validação faria `add_special_offer` rejeitar ofertas que o legado aceita, o que quebra a interface pública (RNF03). O critério de 1,5 também não tem origem no edital nem na regra de negócio. |

---

## 5. Rastreabilidade

A coluna de testes lista os testes de caracterização que já cobrem o requisito. As colunas são completadas ao longo das Fases 4 a 6 do plano: os arquivos mudam com a refatoração, e os requisitos de combos (RF06 a RF11, RF13 e RF16) ganham testes na Fase 6.

Todos os testes citados estão em `python_pytest/tests/test_comportamento_legado.py`, exceto quando indicado.

| ID | Arquivo / classe | Testes | Evidência |
|---|---|---|---|
| RF01 | `shopping_cart.py` (`ShoppingCart.add_item`, `add_item_quantity`); `teller.py` (`Teller.checks_out_articles_from`) | `test_preco_unitario_vem_do_catalogo_no_momento_do_checkout`, `test_carrinho_expoe_itens_como_product_quantity` | [`pytest_antes_ubuntu.log`](evidencias/pytest_antes_ubuntu.log) |
| RF02 | `shopping_cart.py` (`ShoppingCart.add_item_quantity`) | `test_carrinho_agrega_quantidades_mas_recibo_lista_uma_linha_por_inclusao`, `test_quantidades_de_inclusoes_separadas_somam_para_a_oferta` | [`pytest_antes_ubuntu.log`](evidencias/pytest_antes_ubuntu.log) |
| RF03 | `teller.py` (`Teller.checks_out_articles_from`) | `test_produto_sem_oferta_custa_quantidade_vezes_preco`, `test_produto_por_peso_sem_oferta_cobra_valor_exato_da_fracao`, `test_total_e_soma_dos_itens_de_produtos_diferentes` | [`pytest_antes_ubuntu.log`](evidencias/pytest_antes_ubuntu.log) |
| RF04 | `shopping_cart.py` (`ShoppingCart.handle_offers`) | `test_desconto_percentual_incide_sobre_toda_a_quantidade`, `test_desconto_percentual_nao_exige_quantidade_minima`, `test_desconto_percentual_usa_a_fracao_exata_em_produto_por_peso`, `test_oferta_so_vale_para_o_produto_cadastrado`, `test_oferta_de_produto_ausente_do_carrinho_nao_gera_desconto` | [`pytest_antes_ubuntu.log`](evidencias/pytest_antes_ubuntu.log) |
| RF05 | `shopping_cart.py` (`ShoppingCart.handle_offers`) | `test_tres_por_dois`, `test_dois_por_valor_com_uma_unidade_nao_gera_desconto`, `test_dois_por_valor_com_quantidade_par`, `test_cinco_por_valor`, `test_oferta_cinco_por_valor_gera_um_desconto_no_checkout`. **DEF-01:** `test_dois_por_valor_com_quantidade_impar_defeito_legado`, a renomear para `test_dois_por_valor_com_quantidade_impar_cobra_sobra_a_preco_cheio`. **DEF-02:** `test_promocao_por_grupo_com_quantidade_fracionaria_legado`, a renomear para `test_promocao_por_grupo_cobra_a_fracao_restante` | [`pytest_antes_ubuntu.log`](evidencias/pytest_antes_ubuntu.log); log pós-correção em `evidencias/testes/` (Fases 4.7 e 4.8) |
| RF06 | A definir (Fase 5) | A definir (Fase 6) | — |
| RF07 | A definir (Fase 5) | A definir (Fase 6) | — |
| RF08 | A definir (Fase 5) | A definir (Fase 6) | — |
| RF09 | A definir (Fase 5) | A definir (Fase 6) | — |
| RF10 | A definir (Fase 5) | A definir (Fase 6) | — |
| RF11 | A definir (Fase 5) | Ofertas preexistentes: `test_ofertas_de_produtos_diferentes_coexistem_na_mesma_compra`, `test_novo_cadastro_para_o_mesmo_produto_substitui_a_oferta_anterior`. Combos: a definir (Fase 6) | [`pytest_antes_ubuntu.log`](evidencias/pytest_antes_ubuntu.log) |
| RF12 | `receipt.py` (`Receipt`); `tests/receipt_printer.py` (`ReceiptPrinter`) | `test_recibo_impresso_com_todas_as_modalidades_de_oferta`, `test_recibo_impresso_de_carrinho_vazio`, `test_recibo_impresso_so_detalha_quantidade_diferente_de_um` | [`pytest_antes_ubuntu.log`](evidencias/pytest_antes_ubuntu.log) |
| RF13 | `receipt.py` (`Receipt`); `model_objects.py` (`Discount`) | `test_recibo_impresso_com_todas_as_modalidades_de_oferta`, `test_descontos_seguem_a_ordem_da_primeira_inclusao_do_produto`. Combos: a definir (Fase 6) | [`pytest_antes_ubuntu.log`](evidencias/pytest_antes_ubuntu.log) |
| RF14 | `receipt.py` (`Receipt.total_price`) | `test_total_do_recibo_e_soma_dos_itens_mais_descontos_negativos`, `test_carrinho_vazio_gera_recibo_zerado`, `test_mesmo_carrinho_e_mesmas_ofertas_geram_o_mesmo_recibo`, `test_alterar_o_carrinho_depois_de_um_checkout_recalcula_as_ofertas`, `test_receipt_soma_itens_e_descontos_e_devolve_copias_das_listas` | [`pytest_antes_ubuntu.log`](evidencias/pytest_antes_ubuntu.log) |
| RF16 | A definir (Fase 5) | A definir (Fase 6) | — |
| RNF01 | `teller.py`, `catalog.py` | `test_product_with_name_devolve_o_produto_ou_none`. **DEF-03:** `test_product_with_name_funciona_com_qualquer_catalogo_do_contrato` (Fase 4.9) | Log pós-correção em `evidencias/testes/` (Fase 4.9) |
| RNF03 | Interfaces públicas de `teller.py`, `shopping_cart.py`, `receipt.py` e `model_objects.py` | `test_tipos_de_oferta_existentes_e_seus_valores`, `test_add_special_offer_guarda_um_offer_com_tipo_produto_e_argumento`, `test_handle_offers_continua_aceitando_receipt_ofertas_e_catalogo`, `test_carrinho_expoe_itens_como_product_quantity`, mais a suíte completa de caracterização | [`pytest_antes_ubuntu.log`](evidencias/pytest_antes_ubuntu.log) e logs de cada fase |
| RNF09 | Todo o `src/` | `test_recibo_impresso_com_todas_as_modalidades_de_oferta` (preços com centavos para fixar o arredondamento) | [`pytest_antes_ubuntu.log`](evidencias/pytest_antes_ubuntu.log) |

---

## 6. Histórico de alterações

Alterações feitas na Fase 1 do [plano de execução](plano_de_execucao.md#12-requisitos), em relação à planilha original:

| ID | Alteração | Motivo |
|---|---|---|
| RF01 | Descrição e critério reescritos | O preço não é registrado no carrinho, e sim consultado no catálogo no checkout |
| RF02, RF03 | Critérios detalhados (soma das inclusões; fração exata) | Fixar comportamentos já cobertos pelos testes de caracterização |
| RF04, RF05 | Separados em percentual (RF04) e ofertas por grupo (RF05), com critérios numéricos por oferta | Os critérios originais não tinham valores verificáveis. Os critérios numéricos foram mantidos dentro dos dois requisitos, sem criar um requisito por oferta |
| RF05 | Critérios [DEF-01] e [DEF-02] incluídos | Correções decididas no registro de defeitos. Couberam no RF05, sem criar requisitos novos |
| RF07 | Percentual configurável, com padrão de 10% | Decisão D5 do plano |
| RF11 | Critério reescrito: um desconto por unidade, carrinho sem combo inalterado, disputa entre combos pela ordem de cadastro | Decisões D1 e D6. A regra de disputa entre combos coube no RF11, sem criar requisito novo |
| RF13 | Descrição `combo 10% off` incluída | Decisão D4 |
| RF14 | Critério de recálculo a cada checkout incluído | Absorve o RF17 |
| RF16 | Reescrito como a política de "melhor dos dois, com empate para as ofertas" | Decisão D1. Substitui a seleção da "melhor combinação" |
| RF15, RF18 | Movidos para "Fora de escopo" | Ver a seção 4 |
| RF17 | Removido | Incorporado ao RF14 |
| RNF01 | Critério [DEF-03] incluído | A correção do DEF-03 coube no RNF01, sem criar requisito novo. Verificação passa a incluir *Teste* |
| RNF03 | Critério ressalva os casos alterados por DEF-01 e DEF-02 | Sem essa ressalva, as correções decididas violariam o próprio requisito |
| RNF09 | Critério registra a manutenção do `float` como risco remanescente | Diagnóstico técnico, seção 3.11 |
