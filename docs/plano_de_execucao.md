# Plano de Execução: Modernização do Supermarket Receipt

Este documento organiza o trabalho que falta para cumprir o edital. Ele está dividido em 10 fases, em ordem. Cada fase indica o passo do edital que cumpre, o que fazer, quais arquivos mudam e como saber que terminou.

> **Situação atual.** As etapas 1 a 3 (leitura, execução inicial e diagnóstico), os requisitos, o plano de contingência e os testes de caracterização já foram feitos. A caracterização tem 57 testes passando e 99% de cobertura de ramos no `src/`. Ainda faltam ajustes nos documentos, a refatoração, os combos, a segurança "depois", o README e a apresentação.

---

## Sumário

- [Regras gerais](#regras-gerais)
- [Política de convivência entre combos e ofertas](#política-de-convivência-entre-combos-e-ofertas)
- [Decisões registradas (D1 a D6)](#decisões-registradas-d1-a-d6)
- [Fase 0: Organização do repositório](#fase-0-organização-do-repositório)
- [Fase 1: Corrigir os documentos de análise](#fase-1-corrigir-os-documentos-de-análise)
- [Fase 2: Refazer a linha de base de segurança](#fase-2-refazer-a-linha-de-base-de-segurança)
- [Fase 3: Complementar os testes de caracterização](#fase-3-complementar-os-testes-de-caracterização)
- [Fase 4: Refatoração com padrões de projeto](#fase-4-refatoração-com-padrões-de-projeto)
- [Fase 5: Implementar os combos](#fase-5-implementar-os-combos)
- [Fase 6: Testes de regressão e dos combos](#fase-6-testes-de-regressão-e-dos-combos)
- [Fase 7: Reavaliação de segurança](#fase-7-reavaliação-de-segurança)
- [Fase 8: Documento de padrões de projeto](#fase-8-documento-de-padrões-de-projeto)
- [Fase 9: README e evidências finais](#fase-9-readme-e-evidências-finais)
- [Fase 10: Apresentação e entrega](#fase-10-apresentação-e-entrega)
- [Checklist anti-penalidade](#checklist-anti-penalidade)
- [O que pode andar em paralelo](#o-que-pode-andar-em-paralelo)

---

## Regras gerais

- **Uma branch por fase**, saindo da `develop`, com PR de volta para a `develop`.
- **Commits pequenos**, no padrão Conventional Commits (`feat:`, `fix:`, `refactor:`, `test:`, `docs:`, `chore:`).
- **A suíte inteira precisa passar a cada commit.** Um teste só pode mudar de valor esperado em uma correção de defeito intencional e documentada (ver D2, D3 e o [registro de defeitos](registro_de_defeitos.md)).
- **O código novo do `src/` fica em inglês**, para manter a consistência com o legado (RNF06). Os testes e a documentação continuam em português.

---

## Política de convivência entre combos e ofertas

### A regra dos combos

Quando o carrinho tem todos os produtos de um combo, aplica-se 10% de desconto **só sobre as unidades que formam combos completos**.

```
nº de combos = menor valor de ⌊disponível[p] / exigido[p]⌋ entre os produtos do combo
desconto     = 10% × nº de combos × (soma dos preços dos produtos do combo)
```

Exemplos com escova a R$ 10,00 e creme dental a R$ 15,00:

| Carrinho | Combos | Desconto | Total |
|---|---|---|---|
| 1 escova + 1 creme | 1 | R$ 2,50 | R$ 22,50 |
| 2 escovas + 1 creme | 1 | R$ 2,50 | R$ 32,50 (uma escova a preço cheio) |
| 2 escovas + 2 cremes | 2 | R$ 5,00 | R$ 45,00 |
| Só 1 escova | 0 | — | R$ 10,00 |

### O problema

Um produto pode ter uma oferta individual (por exemplo, "3 por 2") e também fazer parte de um combo. O edital proíbe desconto duplicado na mesma unidade e exige manter o comportamento anterior nos carrinhos com promoções preexistentes.

Dar sempre prioridade ao combo **encarece** alguns carrinhos. Exemplo, com a escova em "3 por 2":

- **Carrinho:** 3 escovas + 1 creme.
- **Legado:** R$ 35,00.
- **Combo primeiro:** o combo leva 1 escova e 1 creme (R$ 22,50) e as 2 escovas restantes não ativam o "3 por 2" (R$ 20,00). Total: **R$ 42,50**.

Comparar só os percentuais também engana. O combo dá 10% nos **dois** produtos, inclusive no parceiro que não tinha desconto. Por isso ele pode vencer até ofertas de 20% ou 33%, dependendo dos preços.

### A política adotada: o melhor dos dois, com empate para as ofertas

1. Cada oferta individual informa quantas unidades **consumiu**:

   | Oferta | Unidades consumidas |
   |---|---|
   | 3 por 2 | `(qtd_inteira // 3) × 3`, se ativou |
   | 2 por valor | `(qtd_inteira // 2) × 2`, se ativou |
   | 5 por valor | `(qtd_inteira // 5) × 5`, se ativou |
   | Percentual | **Todas** as unidades |
   | Oferta que não ativou | 0 |

2. O carrinho é calculado **das duas formas**:
   - **Ordem C (ofertas → combos):** as ofertas individuais são aplicadas primeiro, exatamente como seriam sem combos, e os combos usam só as unidades que sobraram.
   - **Ordem B (combos → ofertas):** os combos são aplicados primeiro e as ofertas individuais usam só as unidades que sobraram.

3. Fica a ordem de **menor total**. **Em empate, vence a ordem C** (as ofertas individuais são mantidas). O combo só substitui uma oferta quando é **estritamente melhor** para o cliente.
4. A comparação dos totais é feita **em centavos** (arredondando para 2 casas), para que diferenças de ponto flutuante não decidam o desempate.

### O que essa política garante

| Propriedade | Por quê |
|---|---|
| Cadastrar um combo nunca encarece o carrinho | O resultado é ≤ ordem C, e a ordem C é ≤ o mesmo carrinho sem combos, porque só acrescenta descontos |
| O recibo só muda quando o combo traz ganho real | Em empate, mantém-se a ordem C, que preserva as linhas de desconto das ofertas preexistentes |
| Não há desconto duplicado | Cada unidade é consumida por uma única política |
| Carrinho sem combo completo fica idêntico ao cálculo sem combos | Nas duas ordens nenhum combo se forma |

> A comparação é sempre com **o mesmo carrinho sem combos, no sistema já corrigido** (ver D2). Nos cenários dos defeitos DEF-01 e DEF-02, o total muda em relação ao legado por causa das correções, não por causa dos combos.
| É determinística e testável | São sempre exatamente dois cálculos |

**Limitação documentada:** a escolha é feita para o carrinho inteiro. Distribuições mistas não são testadas, como "este combo primeiro, aquela oferta primeiro". Isso exigiria testar todas as combinações, o que aumentaria a complexidade e não é exigido pelo edital.

### Quem vence: exemplos

| Cenário | Ordem C | Ordem B | Escolhida |
|---|---|---|---|
| Escova R$ 10 com "3 por 2" + creme R$ 15; carrinho 3 + 1 | **R$ 35,00** | R$ 42,50 | C (oferta) |
| Escova R$ 10 com 20% + creme R$ 15; carrinho 1 + 1 | R$ 23,00 | **R$ 22,50** | B (combo) |
| Escova R$ 10 com "5 por 40" + creme R$ 15; carrinho 5 + 5 | R$ 115,00 | **R$ 112,50** | B (combo) |
| Escova R$ 10 com "2 por 15" + creme R$ 15; carrinho 2 + 2 | R$ 45,00 | R$ 45,00 | C (empate → ofertas) |

---

## Decisões registradas (D1 a D6)

Estas decisões devem constar no plano de contingência e no README.

| ID | Decisão | Escolha | Justificativa |
|---|---|---|---|
| D1 | Convivência entre combo e oferta | O melhor dos dois, com empate para as ofertas (ordem C), comparando em centavos | Nunca encarece, sem duplicidade, e mantém as ofertas preexistentes quando o combo não traz ganho, em linha com o "manter o comportamento anterior" do edital |
| D2 | Defeitos confirmados do legado: **DEF-01** ("2 por valor" com quantidade ímpar cobra a mais), **DEF-02** (fração gratuita nas ofertas por grupo) e **DEF-03** (`product_with_name` fora do contrato do catálogo) | **Corrigir todos** | DEF-01 cobra do cliente mais do que sem a oferta; DEF-02 concede desconto maior que o anunciado; DEF-03 quebra com qualquer catálogo real. Detalhes no [registro de defeitos](registro_de_defeitos.md) |
| D3 | Momento e forma das correções | **Depois da refatoração estrutural**, um commit `fix:` por defeito, cada um precedido de um teste `xfail(strict=True)` do comportamento correto | Separa mudança de estrutura de mudança de comportamento. Com o Template Method, DEF-01 e DEF-02 são corrigidos em um único lugar, e cada correção pode ser revertida isoladamente |
| D4 | Representação do combo no recibo | Um `Discount` por produto do combo, com a descrição `"combo 10% off"` | Não muda a classe `Discount` nem o `ReceiptPrinter` |
| D5 | API do combo | Nova e aditiva: `teller.add_bundle_offer(products, percent=10.0)` | Não altera o enum `SpecialOfferType`, o `teller.offers` nem o `add_special_offer`, que são protegidos por testes de contrato |
| D6 | Dois combos disputando o mesmo produto | Vence a ordem de cadastro | Simples, determinístico e testável |

**Sobre os 10%:** o edital define a regra dos 10% como obrigatória, e combos incorretos custam −2,0. O percentual pode ser um parâmetro com padrão de 10%, o que demonstra extensibilidade, mas todos os requisitos e testes usam 10%. A validação de "proporcionalidade mínima" (RF18) não deve ser implementada, porque faria o `add_special_offer` rejeitar ofertas que o legado aceita.

---

## Fase 0: Organização do repositório

> **Edital:** base da estratégia de reversão e de "Evidências / README" | **Branch:** `chore/organizacao`

- [ ] **Criar e publicar a tag da linha de base**, que é a versão estável citada na estratégia de reversão:
  ```bash
  git tag legado-baseline 58df732
  git push origin legado-baseline
  ```
- [ ] **Incluir `*:Zone.Identifier` no `.gitignore` da raiz.** São arquivos que o Windows cria ao baixar documentos.
- [ ] **Criar a estrutura de pastas:**
  ```
  docs/
  ├── diagnostico_tecnico.md
  ├── registro_de_defeitos.md
  ├── requisitos.md
  ├── plano_contingencia.md
  ├── padroes_projeto.md
  ├── seguranca_antes_depois.md
  ├── evidencias_testes.md
  └── evidencias/
      ├── testes/
      └── seguranca/
          ├── antes/
          └── depois/
  apresentacao/
  ```
  A sugestão é escrever em Markdown (versionável, com diffs visíveis nos PRs) e exportar para PDF no final. Quem preferir o Word deve exportar o PDF para essas mesmas pastas.
- [ ] **Definir um ambiente de referência** (por exemplo, Python 3.12 em Ubuntu ou WSL) e registrá-lo no README. Hoje há três ambientes diferentes nos registros: 3.13.11 no Windows, 3.12.5 no Windows e 3.12.3 no Ubuntu.
- [ ] **Regenerar as evidências do Passo 2:**
  - [ ] Log da execução **original** (1 teste). Ele é gerado em um *worktree* temporário, uma cópia do commit original em outra pasta, para não mexer na branch atual:
    ```bash
    REPO=$(pwd)
    git worktree add ../kata-inicial 095e831
    cd ../kata-inicial/python_pytest
    "$REPO/python_pytest/.venv/bin/python" -m pytest -v -p no:cacheprovider \
        > "$REPO/docs/evidencias/testes/01_pytest_inicial.log" 2>&1
    cd "$REPO"
    git worktree remove ../kata-inicial
    ```
  - [ ] Log após a caracterização (57 testes): o `docs/evidencias/pytest_antes_ubuntu.log` já está em UTF-8 e com os nomes atuais dos testes, então serve como evidência. Remover o `pytest_antes_windows.log`, que está em UTF-16 e com um nome de teste desatualizado, ou regenerá-lo em UTF-8 no Windows (`python -m pytest -v | Out-File -Encoding utf8 <arquivo>`).

**Pronto quando:** a tag está no remoto, a pasta `docs/` está estruturada e os logs estão em UTF-8.

---

## Fase 1: Corrigir os documentos de análise

> **Edital:** Passos 3, 4 e 5 | **Barema:** 1,0 + 1,0 + 0,5 | Evita as penalidades de −3,0 por diagnóstico ou plano ausentes | **Branch:** `docs/analise`

### 1.1 Diagnóstico técnico

- [x] **Incluir uma seção de Duplicação:** as três fórmulas de oferta por grupo em `shopping_cart.py` (linhas 45, 54–55 e 63–64) seguem o mesmo padrão. Mostrar o **trecho de código**, não só o número das linhas.
- [x] **Incluir uma seção de Defeitos encontrados**, resumindo e apontando para o [registro de defeitos](registro_de_defeitos.md):
  - DEF-01: "2 por valor" com quantidade ímpar (`/` em vez de `//`, linha 45);
  - DEF-02: fração gratuita nas ofertas por grupo (linhas 45, 54–55 e 63–64);
  - DEF-03: `Teller.product_with_name` acessa `catalog.products`, que não faz parte do contrato (linhas 28–29 de `teller.py`).
- [x] **Incluir os smells que faltam:**
  - [x] lógica do "3 por 2" partida em dois blocos (linhas 39–40 e 53–56);
  - [x] nomes pouco expressivos (`x`, `p`, `discount_n`, `number_of_x`);
  - [x] números mágicos (2, 3, 5, 100);
  - [x] Feature Envy no `handle_offers`, que usa mais dados do catálogo, da oferta e do recibo do que do próprio carrinho;
  - [x] `float` para valores monetários;
  - [x] `ShoppingCart.items` expondo a lista interna. A síntese cita esse ponto, mas o corpo do texto não o analisa.
- [x] **Corrigir a numeração** (há duas seções 7) e os erros de digitação ("catélogo", "susbtituição", "foram identificados foram registrados").

### 1.2 Requisitos

> **Feito em [`requisitos.md`](requisitos.md).** Para não criar requisitos desnecessários, os itens marcados como "novo RF/RNF" e a quebra do RF04/RF05 foram atendidos como **critérios de aceitação** dos requisitos existentes: o percentual no RF04, os critérios das ofertas por grupo e de DEF-01/DEF-02 no RF05, D6 no RF11 e DEF-03 no RNF01. A rastreabilidade já tem os testes de caracterização e segue sendo preenchida nas Fases 4 a 6.

- [x] **RF01:** corrigir a descrição. O preço vem do catálogo no momento do checkout e não é registrado no carrinho.
- [x] **Quebrar RF04 e RF05 em um requisito por oferta**, com critérios numéricos:
  - [x] 3 por 2 (por exemplo, 7 unidades a R$ 10 custam R$ 50);
  - [x] desconto percentual (por exemplo, 3 unidades a R$ 10 com 20% dão R$ 6 de desconto);
  - [x] 2 por valor (por exemplo, 4 unidades a R$ 10 em "2 por 15" custam R$ 30);
  - [x] 5 por valor (por exemplo, 12 unidades a R$ 10 em "5 por 40" custam R$ 100).
- [x] **RF07:** manter os 10% sobre os combos completos. O percentual pode ser parâmetro, desde que o padrão seja 10%.
- [x] **RF16:** reescrever como "Se combo e oferta individual disputarem as mesmas unidades, aplicar a ordem (combos primeiro ou ofertas primeiro) que resultar no menor total, comparado em centavos; em empate, prevalecem as ofertas individuais."
- [x] **Novo RF:** combos que disputam o mesmo produto são resolvidos pela ordem de cadastro (D6).
- [x] **Novo RF (DEF-01):** o "2 por valor" com quantidade ímpar cobra a unidade restante a preço cheio. Critério: 3 unidades a R$ 10 em "2 por 15" custam R$ 25.
- [x] **Novo RF (DEF-02):** nas ofertas por grupo, toda a sobra (unidades e fração) é cobrada a preço cheio. Critério: 3,5 kg a R$ 2,00 em "3 por 2" custam R$ 5,00.
- [x] **Novo RNF (DEF-03):** o `Teller` funciona com qualquer catálogo que cumpra o contrato de `SupermarketCatalog`, incluindo a busca de produto por nome.
- [x] **Remover RF15, RF17 e RF18:**
  - RF15 e RF18 vão para uma seção "Fora de escopo", com justificativa.
  - RF17 já é comportamento do legado e vira critério de aceitação do RF14.
- [x] **Incluir a coluna de rastreabilidade:** RF → arquivo ou classe → nome do teste → evidência. Preencher ao longo das Fases 4 a 6.

### 1.3 Plano de contingência

> **Feito em [`plano_contingencia.md`](plano_contingencia.md).** Além dos itens abaixo, a seção 2.1 perdeu a referência à proporcionalidade mínima, os riscos ligados aos requisitos removidos foram ajustados (o R10 passou a tratar das correções de defeitos, e foi incluído o R13, de quebra de comportamento ou de interface na refatoração), e a estratégia de reversão incorporou o fluxo de branches e PRs para a `develop` sugerido nos comentários do documento.

- [x] **Tabela de interfaces:**
  - [x] corrigir `Teller.checkout_articles_from_cart` para **`Teller.checks_out_articles_from`**;
  - [x] incluir `Teller.product_with_name`, `Teller.offers`, `ShoppingCart.items`, `ShoppingCart.product_quantities` e `Receipt.add_product`.
- [x] **Invariantes:**
  - [x] trocar "no máximo um desconto por produto" por **"no máximo um desconto por unidade"**;
  - [x] marcar as invariantes do "2 por valor" e da fração como **comportamento corrigido** (DEF-01 e DEF-02), explicando que até a Fase 4.6 vale o comportamento legado e a partir das Fases 4.7 e 4.8 vale o corrigido;
  - [x] incluir **"cadastrar um combo nunca encarece um carrinho"**.
- [x] **Seção 2.2:** substituir o "algoritmo de otimização" e a "proporcionalidade mínima" pela política descrita neste documento.
- [x] **Preencher a tabela "Ordem das alterações"**, que hoje tem a linha 3 escrita "pensar" e as linhas 4 a 7 vazias, com as Fases 3 a 7 deste plano.
- [x] **Consertar a tabela de critérios de aceitação**, que está com células deslocadas. A "Preservação das funcionalidades" deve apontar para o RNF03.
- [x] **Risco (1):** retirar a mitigação "implementar validações na inclusão de produtos". Ela mudaria o comportamento do legado e não é pedida.
- [x] **Incluir a seção "Decisões registradas"** com D1 a D6.
- [x] **Estratégia de reversão:** citar a tag `legado-baseline` e o fluxo de branches e PRs para a `develop`.

**Pronto quando:** os três documentos estão revisados pelo grupo e estão em `docs/`.

---

## Fase 2: Refazer a linha de base de segurança

> **Edital:** Passo 7 | **Barema:** parte de 1,0 (Bandit, pip-audit e Safety) e parte das Evidências | **Branch:** `chore/seguranca-antes`

**Por que refazer:** o `filelock` vulnerável que aparece na auditoria atual **não é dependência do projeto**. Ao instalar o `requirements.txt` em um ambiente limpo, ele não aparece. Ele veio das ferramentas de auditoria instaladas no mesmo `.venv` do projeto, o que é confirmado pelo motivo `environment-dependency` reportado pelo Safety. Por isso as ferramentas devem rodar em um ambiente separado.

Rodar tudo **sobre a tag `legado-baseline`**, a partir da raiz do repositório:

```bash
git switch --detach legado-baseline

# ambiente das ferramentas FORA de python_pytest
python -m venv ../.venv-audit
../.venv-audit/bin/pip install bandit pip-audit safety

# versões (registrar no documento)
../.venv-audit/bin/bandit --version
../.venv-audit/bin/pip-audit --version
../.venv-audit/bin/safety --version
python --version

D=docs/evidencias/seguranca/antes
mkdir -p $D
../.venv-audit/bin/bandit -r python_pytest/src -f json -o $D/bandit.json
../.venv-audit/bin/bandit -r python_pytest/src -f txt  -o $D/bandit.txt
../.venv-audit/bin/pip-audit -r python_pytest/requirements.txt -f json -o $D/pip-audit.json
../.venv-audit/bin/safety auth login        # registrar se exigiu conta (limitação de acesso)
../.venv-audit/bin/safety scan --target python_pytest --output json > $D/safety.json

git switch develop
```

No Windows, troque `../.venv-audit/bin/` por `..\.venv-audit\Scripts\`.

Registrar em `docs/seguranca_antes_depois.md`:

- [ ] **Ambiente:** versões das ferramentas, sistema operacional, versão do Python e os comandos exatos.
- [ ] **Limitação de acesso:** se o Safety exigiu login ou conta.
- [ ] **Escopo:** por que o Bandit cobriu só o `src/`. Os testes usam `assert`, que gera alertas B101 esperados.
- [ ] **Triagem do `filelock`:** a origem dele e a comparação entre o ambiente contaminado e o ambiente isolado.
- [ ] **Divergência de contagem:** a diferença entre 14 e 13 ocorrências do Safety, ou a constatação de que ela some no ambiente limpo.
- [ ] **Tabela de classificação:**

  | ID | Ferramenta | Pacote ou arquivo | Severidade | Relevância no contexto | Ação | Justificativa |
  |---|---|---|---|---|---|---|

---

## Fase 3: Complementar os testes de caracterização

> **Edital:** Passo 6 | **Branch:** `test/caracterizacao-complemento`

- [ ] **Teste "carrinho sem combos = legado":** fixar o recibo impresso de alguns carrinhos variados (sem ofertas, com cada tipo de oferta, com produto por peso). Esse teste será repetido depois com combos cadastrados para outros produtos. **Não usem** nesses carrinhos os cenários dos defeitos ("2 por valor" com quantidade ímpar e oferta por grupo ativada com quantidade fracionária). Esses cenários já têm testes próprios, e assim o teste continua válido depois das correções.
- [ ] **Revisar o teste de contrato do enum** [`test_tipos_de_oferta_existentes_e_seus_valores`](../python_pytest/tests/test_comportamento_legado.py). Ele compara com `==` e exige exatamente 4 membros. Se a decisão D5 for seguida, pode ficar como está. Para ter margem de segurança, troquem a comparação por "os 4 membros existentes continuam com os mesmos valores".
- [ ] **Decidir sobre o `approvaltests`:**
  - **Opção 1:** usá-lo para um *golden master*, com dezenas de carrinhos gerados e os recibos aprovados antes da refatoração. É uma evidência forte de "antes e depois".
  - **Opção 2:** removê-lo na Fase 7, porque hoje nenhum teste o usa.
- [ ] **Criar os testes do comportamento correto para cada defeito**, marcados com `@pytest.mark.xfail(strict=True, reason="DEF-0X")`. Eles falham enquanto o defeito existe, o que serve de evidência do defeito, e são liberados nos commits `fix:` da Fase 4. Os valores esperados estão na seção 4 do [registro de defeitos](registro_de_defeitos.md).
- [ ] **Gerar a cobertura de baseline:**
  ```bash
  cd python_pytest
  python -m coverage run --branch --source=src -m pytest
  python -m coverage report -m > ../docs/evidencias/testes/02_cobertura_antes.txt
  ```
  Referência atual: 99% de cobertura de ramos. As únicas linhas não cobertas são as do stub de banco em `catalog.py`.

**Pronto quando:** a suíte passa e a cobertura de baseline está salva.

---

## Fase 4: Refatoração com padrões de projeto

> **Edital:** Passo 8 | **Barema:** 2,0 (código refatorado) + 2,0 (padrões, SOLID e Clean Code) | **Branch:** `refactor/politicas-desconto`

### Padrões adotados

Cada padrão resolve um problema apontado no diagnóstico, o que atende ao critério do edital de que "padrões artificiais não serão valorizados".

| Padrão | Classe ou função | Problema do diagnóstico que resolve |
|---|---|---|
| **Strategy** | `DiscountPolicy` (interface), com `PercentageOffer`, `GroupPriceOffer` e `BundleOffer` | Switch statements, violação do OCP, excesso de responsabilidades no `ShoppingCart` (SRP) |
| **Template Method** | `GroupPriceOffer`, com as subclasses `ThreeForTwo`, `TwoForAmount` e `FiveForAmount` | **Duplicação** das três fórmulas de grupo |
| **Factory** | `create_policy(offer)` | Traduzir o `SpecialOfferType` da API pública em estratégia, preservando o `add_special_offer` |

> ⚠️ **Sobre o nome "Factory":** se a fábrica for apenas uma função ou um dicionário, ela é uma *Simple Factory*, não um *Factory Method* do GoF. Chamem pelo nome correto na documentação, ou implementem como uma classe com método de fábrica. A banca pode cobrar essa distinção.

### Estrutura proposta

```python
class PolicyResult(NamedTuple):
    discounts: list[Discount]
    consumed: dict[Product, float]


class DiscountPolicy(ABC):                      # Strategy
    @abstractmethod
    def apply(self, available, catalog) -> PolicyResult: ...


class GroupPriceOffer(DiscountPolicy):          # Template Method
    def apply(self, available, catalog):
        # algoritmo comum: grupos, sobra, desconto e unidades consumidas
        ...

    @abstractmethod
    def group_size(self) -> int: ...

    @abstractmethod
    def group_price(self, unit_price) -> float: ...


class ThreeForTwo(GroupPriceOffer): ...
class TwoForAmount(GroupPriceOffer): ...
class FiveForAmount(GroupPriceOffer): ...
class PercentageOffer(DiscountPolicy): ...      # consome todas as unidades


def create_policy(offer: Offer) -> DiscountPolicy:   # Factory
    ...
```

### Commits, nesta ordem

A suíte deve passar em cada um.

| # | Commit | O que muda | Comportamento |
|---|---|---|---|
| 4.1 | `refactor: renomeia variáveis e extrai métodos em handle_offers` | `x` → `group_size`, `p` → `product` etc.; um método privado por tipo de oferta | Idêntico |
| 4.2 | `refactor: introduz DiscountPolicy (Strategy) e PercentageOffer` | Cria `src/discount_policies.py` com a interface `apply(available, catalog) -> PolicyResult` | Idêntico |
| 4.3 | `refactor: extrai GroupPriceOffer (Template Method)` | Classe base com o algoritmo comum e três subclasses. **Preservar exatamente** as descrições (`"3 for 2"`, `"2 for 15.0"`, `"5 for 40.0"`, `"10.0% off"`) **e os defeitos DEF-01 e DEF-02**, que só são corrigidos nos itens 4.7 e 4.8 | Idêntico |
| 4.4 | `refactor: adiciona fábrica de políticas a partir de SpecialOfferType` | `create_policy(offer)` | Idêntico |
| 4.5 | `refactor: move aplicação de descontos para DiscountEngine` | Cria `src/discount_engine.py`, que aplica as políticas em sequência e vai descontando as unidades consumidas. `ShoppingCart.handle_offers(receipt, offers, catalog)` vira uma **fachada** que delega ao engine e mantém o contrato. O `Teller` passa a usar o engine | Idêntico |
| 4.6 | `refactor: políticas informam unidades consumidas` | Preparação para os combos, ainda sem uso | Idêntico |
| 4.7 | `fix(DEF-01): cobra a sobra do 2 por valor a preço cheio` | Divisão inteira no cálculo de grupos, em `GroupPriceOffer`. Remover o `xfail` do teste correto e substituir `test_dois_por_valor_com_quantidade_impar_defeito_legado` por `..._cobra_sobra_a_preco_cheio`: 3 un. → R$ 25,00; 5 un. → R$ 40,00; 7 un. → R$ 55,00 | **Mudança intencional** (3 casos) |
| 4.8 | `fix(DEF-02): cobra a fração restante nas ofertas por grupo` | Sobra calculada com a quantidade real (`quantity - groups * size`). Atualizar `test_promocao_por_grupo_com_quantidade_fracionaria_legado` (3 casos) e o recibo impresso: `5 for 7.5 (maca) -2.45` e `Total: 20.96` | **Mudança intencional** (4 casos) |
| 4.9 | `fix(DEF-03): busca de produto por nome via contrato do catálogo` | Declarar `product_with_name` em `SupermarketCatalog`, implementar no `FakeCatalog` e fazer o `Teller` delegar ao catálogo. Liberar o teste `test_product_with_name_funciona_com_qualquer_catalogo_do_contrato` | Interface pública inalterada |
| 4.10 | `docs: adiciona docstrings PEP 257` | Docstrings nas classes e métodos novos | Idêntico |

Cada commit `fix:` deve citar na mensagem o ID do defeito e o link para o [registro de defeitos](registro_de_defeitos.md).

**Pronto quando:**
- até o item 4.6, os 57 testes passam **sem nenhuma alteração** (prova de refatoração pura);
- depois dos itens 4.7 a 4.9, só os 7 casos listados no registro de defeitos mudaram de valor esperado, e não restou nenhum `xfail`;
- o `handle_offers` não tem mais nenhum `if` por tipo de oferta.

---

## Fase 5: Implementar os combos

> **Edital:** Passo 9 | **Barema:** 0,5 | Evita a penalidade de −2,0 | **Branch:** `feat/combos`

| # | Commit | O que muda |
|---|---|---|
| 5.1 | `feat: adiciona BundleOffer` | Nova estratégia em `discount_policies.py`. Calcula `n = min(⌊disponível / exigido⌋)` entre os produtos do combo. Gera **um `Discount` por produto**, com a descrição `"combo 10% off"` (D4), e consome `n × exigido` de cada produto |
| 5.2 | `feat: adiciona Teller.add_bundle_offer` | `add_bundle_offer(products, percent=10.0)`. Um produto repetido na lista significa quantidade maior que 1. Os combos ficam em `self.bundles`, **sem tocar** em `self.offers` nem no enum (D5) |
| 5.3 | `feat: DiscountEngine escolhe a melhor ordem entre ofertas e combos` | Calcula a ordem C (ofertas → combos) e a ordem B (combos → ofertas) e fica com o menor total, comparado em centavos; em empate, a C (D1). Produtos com 0 unidades disponíveis são ignorados, para não gerar linhas de `-0.00`. Os descontos individuais aparecem antes dos de combo no recibo |

Esboço do engine:

```python
def _apply_in_order(self, policies, quantities, catalog):
    available = dict(quantities)
    discounts = []
    for policy in policies:
        result = policy.apply(available, catalog)
        discounts += result.discounts
        for product, qty in result.consumed.items():
            available[product] -= qty
    return discounts

def apply(self, receipt, quantities, offers, bundles, catalog):
    individual = [create_policy(offers[p]) for p in quantities if p in offers]
    offers_first = self._apply_in_order([*individual, *bundles], quantities, catalog)
    bundles_first = self._apply_in_order([*bundles, *individual], quantities, catalog)
    # descontos são negativos: soma menor = cliente paga menos
    # compara em centavos; B só vence se for ESTRITAMENTE melhor (empate → C)
    in_cents = lambda discounts: round(sum(d.discount_amount for d in discounts), 2)
    chosen = bundles_first if in_cents(bundles_first) < in_cents(offers_first) else offers_first
    for discount in chosen:
        receipt.add_discount(discount)
```

---

## Fase 6: Testes de regressão e dos combos

> **Edital:** Passo 10 | **Barema:** 1,0 | Evita a penalidade de −3,0 | **Branch:** `test/combos`

Criar `python_pytest/tests/test_combos.py`, usando as fixtures existentes: escova R$ 10, creme dental R$ 15 e fio dental R$ 5.

| # | Cenário | Esperado | Critério |
|---|---|---|---|
| 1 | Combo escova + creme; carrinho 1 + 1 | desconto −2,50; total 22,50 | Combo completo |
| 2 | Carrinho com 2 escovas + 1 creme | −2,50; total 32,50 | Excedente a preço cheio |
| 3 | Carrinho com 2 escovas + 2 cremes | −5,00; total 45,00 | Múltiplos combos |
| 4 | Só 1 escova | sem desconto | Combo incompleto |
| 5 | Combo de 2 escovas + 1 creme; carrinho 5 + 3 | 2 combos | Combo com quantidade maior que 1 |
| 6 | Escova com "3 por 2"; carrinho 3 + 1 | total 35,00, sem linha de combo | Oferta vence |
| 7 | Escova com 20%; carrinho 1 + 1 | total 22,50 | Combo vence |
| 8 | Escova com "5 por 40"; carrinho 5 + 5 | total 112,50 | Combo vence oferta de 20% |
| 9 | Escova com "2 por 15"; carrinho 2 + 2 | total 45,00, com a linha `2 for 15.0 (escova)` e **sem** linhas de combo | **Empate: vencem as ofertas** |
| 10 | Combos escova + creme e escova + fio; carrinho 1 + 1 + 1 | só o primeiro combo | Ordem de cadastro (D6) |
| 11 | Fio dental com "3 por 2" fora do combo | oferta do fio inalterada | Convivência |
| 12 | Parametrizado com vários carrinhos | total com combos ≤ total sem combos | "Nunca encarece" |
| 13 | Recibo impresso com combo | texto exato | RF13 |
| 14 | `add_bundle_offer` | `teller.offers` inalterado | Contrato (D5) |
| 15 | Suíte de caracterização com combos cadastrados para outros produtos | resultados inalterados | Regressão |

Evidências a salvar em `docs/evidencias/testes/`:

- [ ] `03_pytest_depois.log`
- [ ] `04_cobertura_depois.txt`
- [ ] Tabela comparativa em `docs/evidencias_testes.md`:

  | Métrica | Antes | Depois |
  |---|---|---|
  | Testes | 57 | |
  | Aprovados | 57 | |
  | Cobertura de ramos (`src/`) | 99% | |
  | Casos alterados de propósito | — | 7 (DEF-01: 3; DEF-02: 4) |
  | Testes novos de defeitos | — | 1 (DEF-03) |
  | Defeitos corrigidos | 0 de 3 | 3 de 3 |

---

## Fase 7: Reavaliação de segurança

> **Edital:** Passo 11 | **Barema:** restante de 1,0 | **Branch:** `chore/seguranca-depois`

- [ ] **Remover `approvaltests` e `pytest-approvaltests`**, se não foram usados na Fase 3, junto com o `tests/approvaltests_config.json`.
- [ ] **Fixar as versões no `requirements.txt`** (por exemplo, `pytest==9.2.0`, `coverage==7.17.3`), para a instalação ser reproduzível (RNF08).
- [ ] **Rodar os mesmos comandos da Fase 2** sobre a `develop`, com saída em `docs/evidencias/seguranca/depois/`.
- [ ] **Montar o comparativo de antes e depois** em `docs/seguranca_antes_depois.md`:
  - achados que sumiram e o motivo;
  - achados novos, se houver;
  - **riscos remanescentes**: o uso de `float` para dinheiro e as limitações da análise estática e das bases de vulnerabilidades.
- [ ] **Incluir as ressalvas exigidas pelo edital:**
  - a ausência de alertas não prova que o sistema é seguro;
  - nenhuma dependência foi adicionada só para gerar resultado nas ferramentas.

---

## Fase 8: Documento de padrões de projeto

> **Edital:** entregável 5.5 | **Barema:** parte dos 2,0 de padrões | **Arquivo:** `docs/padroes_projeto.md`

- [ ] **Diagramas de classes de antes e depois**, em Mermaid, que o GitHub renderiza:
  ```mermaid
  classDiagram
      class DiscountPolicy {
          <<interface>>
          +apply(available, catalog) PolicyResult
      }
      DiscountPolicy <|.. PercentageOffer
      DiscountPolicy <|.. GroupPriceOffer
      DiscountPolicy <|.. BundleOffer
      GroupPriceOffer <|-- ThreeForTwo
      GroupPriceOffer <|-- TwoForAmount
      GroupPriceOffer <|-- FiveForAmount
      DiscountEngine --> DiscountPolicy
      Teller --> DiscountEngine
      ShoppingCart ..> DiscountEngine : handle_offers (fachada)
  ```
- [ ] **Para cada padrão (Strategy, Template Method e Factory):**
  - o problema que resolve, com referência ao diagnóstico;
  - o trecho de código antes e depois;
  - os testes que o exercitam.
- [ ] **Seção de SOLID:**
  - **SRP:** o carrinho só guarda itens; o cálculo de descontos foi para as políticas e o engine.
  - **OCP:** os combos entraram sem alterar as estratégias existentes.
  - **LSP:** toda `DiscountPolicy` pode ser trocada por outra no engine.
  - **DIP:** o engine depende da abstração `DiscountPolicy`, não dos tipos concretos.
- [ ] **Benefício do Template Method nas correções:** o DEF-02 estava replicado em três blocos do legado, e depois da refatoração foi corrigido em um único método.
- [ ] **Demonstração de extensibilidade:** quantas linhas do código existente foi preciso alterar para adicionar os combos (`git diff --stat` da Fase 5).

---

## Fase 9: README e evidências finais

> **Edital:** entregável 5.9 | **Barema:** 0,5 (Evidências) + 0,5 (README) | **Branch:** `docs/readme`

- [ ] **Reescrever o `README.md` da raiz**, que hoje é o original do kata, em inglês. Ele precisa ter:
  - [ ] objetivo do trabalho e integrantes;
  - [ ] ambiente de referência (sistema operacional, versão do Python);
  - [ ] instalação (`python -m venv .venv`, `pip install -r requirements.txt`);
  - [ ] como rodar os testes e a cobertura;
  - [ ] como rodar o Bandit, o pip-audit e o Safety, com o ambiente separado;
  - [ ] estrutura de pastas;
  - [ ] decisões arquiteturais (D1 a D6) e padrões adotados, com links para a pasta `docs/`;
  - [ ] seção "Defeitos corrigidos", resumindo DEF-01 a DEF-03 e apontando para o registro de defeitos.
- [ ] **Testar o README do zero** em uma máquina ou pasta limpa. É o critério "execução independente" do barema.
- [ ] **Completar a coluna de rastreabilidade** dos requisitos (RF → implementação → teste → evidência).

---

## Fase 10: Apresentação e entrega

> **Edital:** entregável 5.10 | **Arquivo:** `apresentacao/slides.pdf`

Roteiro sugerido para 10 a 15 minutos:

1. **O problema inicial:** um sistema legado com ofertas concentradas no `handle_offers`.
2. **O diagnóstico:** os principais smells e as violações de SOLID, com trechos de código.
3. **O plano e os riscos:** ordem das alterações e estratégia de reversão (tag e branches).
4. **Testes de caracterização e defeitos:** 57 testes, 99% de cobertura, os **3 defeitos encontrados** e como cada um foi corrigido (tabela de antes e depois do registro de defeitos).
5. **Os padrões e o SOLID:** Strategy, Template Method e Factory, com o diagrama de antes e depois.
6. **A regra dos combos e a política de convivência:** a tabela "Quem vence".
7. **Resultados dos testes:** o comparativo de antes e depois.
8. **Segurança:** as três ferramentas, a triagem e o **caso do `filelock`**.
9. **Lições aprendidas e limitações:** separar refatoração de correção, a escolha por carrinho inteiro na política de combos e o uso de `float` como risco remanescente.

Fechamento:

- [ ] Fazer o merge da `develop` na `main` por PR.
- [ ] Criar a tag `v1.0-entrega` e publicá-la no remoto.

---

## Checklist anti-penalidade

| Penalidade do edital | Valor | Coberta na fase |
|---|---|---|
| Diagnóstico técnico ausente | −3,0 | 1.1 |
| Plano de contingência ausente | −3,0 | 1.3 |
| Testes automatizados ausentes | −3,0 | 3 e 6 |
| Código final não executa | nota máxima 5,0 | Suíte passando em todo commit; README testado (9) |
| Quebra injustificada de regras ou interfaces | −2,0 | Testes de contrato preservados; D5; as únicas mudanças de regra são as correções DEF-01 a DEF-03, isoladas em commits próprios e justificadas no registro de defeitos |
| Regra de combos incorreta ou ausente | −2,0 | 5 e 6 |
| Faltar os dois padrões exigidos | zero no critério | 4 e 8 |
| Faltar a análise das três ferramentas | zero no critério | 2 e 7 |

---

## O que pode andar em paralelo

```
Fase 0 ──┬── Fase 1 (documentos) ─────────────┐
         ├── Fase 2 (segurança antes) ────────┤
         └── Fase 3 (testes complementares) ──┴── Fase 4 ── Fase 5 ── Fase 6 ──┬── Fase 7 ──┐
                                                                               └── Fase 8 ──┴── Fase 9 ── Fase 10
```

- **Fases 1, 2 e 3:** podem ser feitas por pessoas diferentes ao mesmo tempo, logo depois da Fase 0.
- **Fases 4 a 6:** são **sequenciais**, porque cada commit depende do anterior. O ideal é uma pessoa conduzindo e outra revisando os PRs.
- **Fases 7 e 8:** podem andar em paralelo assim que a Fase 6 terminar.
