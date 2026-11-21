# Relatório da Linha de Base de Segurança

**Projeto:** Supermarket Receipt Refactoring Kata (Python + pytest)  
**Data/Hora da Execução (UTC):** 2026-11-21 16:41:35 UTC  
**Responsável:** Assistente de Engenharia de Software  
**Branch Git:** `feature/clean-archtecture-aplication`  
**Commit Git:** `6f7c327e0ae0912e0dbb80906c0063ee2ffb2d0f`  
**Status do Repositório:** Não houve alteração em arquivos rastreados do projeto (`git diff` limpo)

---

## 1. Sumário Executivo

Este documento consolida a **linha de base de segurança** do projeto *Supermarket Receipt* antes de qualquer refatoração no código-fonte. A análise foi realizada a partir da raiz do módulo Python (`python_pytest`), utilizando um ambiente Conda isolado (`supermarket-sec`, Python 3.12.16) e ferramentas consagradas de análise estática de código (SAST) e auditoria de vulnerabilidades em dependências (SCA):

1. **Bandit (SAST):**
   - **Código de Produção (`src/`):** 0 vulnerabilidades identificadas em 133 linhas de código (código de saída `0`).
   - **Código de Testes (`tests/`):** 7 alertas de baixa severidade (`B101:assert_used`) decorrentes do uso padrão de `assert` nos testes do `pytest` (código de saída `1`). Classificados como **falso positivo / ruído de teste**.
2. **pip-audit (SCA):**
   - Auditou 28 pacotes congelados antes da instalação das ferramentas de segurança (código de saída `1`).
   - Identificou 16 vulnerabilidades conhecidas concentradas em 1 único pacote: `urllib3==2.0.6`. Este pacote foi importado transitivamente por `approvaltests -> empty-files -> requests` a partir do repositório local do usuário (`~/.local/lib/python3.12/site-packages`). Classificado como **vulnerabilidade real em dependência de teste**.
   - A auditoria ampla do ambiente identificou vulnerabilidades em `filelock==3.32.7`, comprovadamente exigido apenas pela ferramenta `safety`, portanto **fora do escopo do produto**.
3. **Safety (SCA):**
   - Executado após autenticação da CLI via plataforma Safety (código de saída `0`).
   - Avaliou as dependências declaradas em `requirements.txt`. Identificou 1 vulnerabilidade em potencial no pacote `pytest` (`SFTY-20260122-20373` para versões `<9.0.3`) decorrente de requisito não pinado (`unpinned-specification`), que foi ignorada pela política padrão da plataforma. No ambiente de execução, a versão instalada é a `pytest==9.2.0`, portanto **não vulnerável**.

> [!NOTE]
> **Ressalva de Segurança:** A ausência de alertas críticos no código de produção e nas regras de negócio não comprova segurança absoluta. As ferramentas cobrem padrões estáticos e vulnerabilidades conhecidas (CVEs/GHSAs catalogadas), não cobrindo falhas lógicas de negócio nem ataques zero-day.

---

## 2. Metodologia e Ambiente de Execução

### 2.1 Identificação do Sistema
As informações completas do sistema operacional e do ferramental foram registradas no arquivo de evidência [base_ambiente.txt](file:///home/beatriz/Documents/arquivos_faculdade/SupermarketReceipt-Refactoring-Kata/python_pytest/docs/seguranca/base_ambiente.txt).

| Propriedade | Valor Registrado |
| :--- | :--- |
| **Data/Hora (UTC)** | 2026-11-21 16:20:44 UTC |
| **Sistema Operacional** | Linux 7.0.0-34-generic (#34~24.04.1-Ubuntu SMP x86_64) |
| **Shell** | Bash (`/usr/bin/bash`) |
| **Proxy Configurado** | Falso (nenhum proxy HTTP/HTTPS ativo) |
| **Commit Git Atual** | `6f7c327e0ae0912e0dbb80906c0063ee2ffb2d0f` |
| **Diretório Raiz dos Comandos** | `/home/beatriz/Documents/arquivos_faculdade/SupermarketReceipt-Refactoring-Kata/python_pytest` |

### 2.2 Versões dos Softwares e Ferramentas

| Ferramenta / Interpretador | Versão | Origem / Caminho |
| :--- | :--- | :--- |
| **Conda** | 26.7.1 | `/home/beatriz/miniconda3/condabin/conda` |
| **Python** | 3.12.16 | `/home/beatriz/miniconda3/envs/supermarket-sec/bin/python` |
| **pip** | 26.2.2 | Ambiente `supermarket-sec` |
| **pytest** | 9.2.0 | Ambiente `supermarket-sec` |
| **Bandit** | 1.9.4 | Ambiente `supermarket-sec` |
| **pip-audit** | 2.10.1 | Ambiente `supermarket-sec` |
| **Safety** | 3.8.1 | Ambiente `supermarket-sec` (Autenticado como `beatrizestevez29@gmail.com`) |

---

## 3. Preparação do Ambiente e Sanidade Inicial

1. **Criação do Ambiente Conda:**
   O ambiente foi criado de forma isolada através do comando:
   ```bash
   conda create -n supermarket-sec python=3.12 -y
   ```
2. **Instalação das Dependências do Projeto:**
   Instaladas via `pip` antes de qualquer ferramenta de segurança:
   ```bash
   conda run -n supermarket-sec python -m pip install -r requirements.txt
   ```
3. **Teste de Sanidade (`pytest`):**
   ```bash
   conda run -n supermarket-sec python -m pytest -q
   ```
   - **Código de saída:** `0`
   - **Resultado:** `1 passed in 0.01s` (testes unitários íntegros antes das medições).

4. **Congelamento das Dependências do Projeto:**
   Realizado antes de instalar as ferramentas de segurança para isolar o escopo do software:
   ```bash
   conda run -n supermarket-sec python -m pip freeze > docs/seguranca/base_pip_freeze.txt
   ```
   - **Análise do formato:** O arquivo [base_pip_freeze.txt](file:///home/beatriz/Documents/arquivos_faculdade/SupermarketReceipt-Refactoring-Kata/python_pytest/docs/seguranca/base_pip_freeze.txt) foi analisado e **não continha** linhas com prefixo `@ file:///...` (todos os pacotes já se encontravam no padrão `nome==versão`).
   - **Observação sobre User Site-Packages:** Foi detectado que o interpretador do Conda herdou pacotes de `/home/beatriz/.local/lib/python3.12/site-packages` (`ENABLE_USER_SITE: True`), o que fez com que o `pip` reaproveitasse a biblioteca `urllib3==2.0.6` já existente no ambiente do usuário ao satisfazer o requisito transitivo de `approvaltests -> empty-files -> requests`.

---

## 4. Análise Estática de Código (SAST) - Bandit

Foram executadas três verificações com o Bandit 1.9.4:
1. Análise completa do código de produção e testes: `src` e `tests` (excluindo `.approval_tests_temp`).
2. Geração da evidência em JSON consolidada.
3. Análise estrita do código de produção (`src`).

### 4.1 Comandos Executados e Códigos de Saída

| Escopo Analisado | Comando | Código de Saída | Evidência Gerada |
| :--- | :--- | :---: | :--- |
| `src tests` (TXT) | `conda run -n supermarket-sec python -m bandit -r src tests -x .approval_tests_temp -f txt -o docs/seguranca/base_bandit.txt` | **1** | [base_bandit.txt](file:///home/beatriz/Documents/arquivos_faculdade/SupermarketReceipt-Refactoring-Kata/python_pytest/docs/seguranca/base_bandit.txt) |
| `src tests` (JSON) | `conda run -n supermarket-sec python -m bandit -r src tests -x .approval_tests_temp -f json -o docs/seguranca/base_bandit.json -q` | **1** | [base_bandit.json](file:///home/beatriz/Documents/arquivos_faculdade/SupermarketReceipt-Refactoring-Kata/python_pytest/docs/seguranca/base_bandit.json) |
| `src` (Produção) | `conda run -n supermarket-sec python -m bandit -r src` | **0** | Saída no terminal (registrada na console) |

### 4.2 Métricas de Código e Alertas do Bandit

- **Total de linhas de código analisadas (src + tests):** 212 linhas (133 em `src/`, 79 em `tests/`).
- **Métricas por Severidade:**
  - `High`: 0
  - `Medium`: 0
  - `Low`: 7
  - `Undefined`: 0
- **Métricas por Confiança:**
  - `High`: 7
  - `Medium`: 0
  - `Low`: 0
  - `Undefined`: 0

### 4.3 Detalhamento dos Achados do Bandit

Todos os 7 achados estão localizados no arquivo de testes `tests/test_supermarket.py`:

| ID | CWE | Arquivo : Linha | Código Encontrado | Severidade | Confiança | Classificação |
| :--- | :--- | :--- | :--- | :---: | :---: | :--- |
| `B101` | CWE-703 | `tests/test_supermarket.py:24` | `assert 4.975 == pytest.approx(receipt.total_price(), 0.01)` | Low | High | Falso positivo / Ruído |
| `B101` | CWE-703 | `tests/test_supermarket.py:25` | `assert [] == receipt.discounts` | Low | High | Falso positivo / Ruído |
| `B101` | CWE-703 | `tests/test_supermarket.py:26` | `assert 1 == len(receipt.items)` | Low | High | Falso positivo / Ruído |
| `B101` | CWE-703 | `tests/test_supermarket.py:28` | `assert apples == receipt_item.product` | Low | High | Falso positivo / Ruído |
| `B101` | CWE-703 | `tests/test_supermarket.py:29` | `assert 1.99 == receipt_item.price` | Low | High | Falso positivo / Ruído |
| `B101` | CWE-703 | `tests/test_supermarket.py:30` | `assert 2.5 * 1.99 == pytest.approx(receipt_item.total_price, 0.01)` | Low | High | Falso positivo / Ruído |
| `B101` | CWE-703 | `tests/test_supermarket.py:31` | `assert 2.5 == receipt_item.quantity` | Low | High | Falso positivo / Ruído |

**Diagnóstico Bandit:** O código de produção em `src/` (`catalog.py`, `model_objects.py`, `receipt.py`, `shopping_cart.py`, `teller.py`) obteve código de saída **0**, sem nenhuma fragilidade estrutural ou prática insegura detectada.

---

## 5. Auditoria de Dependências (SCA) - pip-audit

A auditoria foi executada com o `pip-audit 2.10.1` apontando estritamente para o arquivo congelado `docs/seguranca/base_pip_freeze.txt` com a opção `--no-deps`.

### 5.1 Comandos Executados e Códigos de Saída

```bash
conda run -n supermarket-sec python -m pip_audit -r docs/seguranca/base_pip_freeze.txt --no-deps -f json -o docs/seguranca/base_pip_audit.json
conda run -n supermarket-sec python -m pip_audit -r docs/seguranca/base_pip_freeze.txt --no-deps 2>&1 | tee docs/seguranca/base_pip_audit.txt
```

- **Código de saída:** `1` (vulnerabilidades encontradas)
- **Pacotes avaliados no `base_pip_freeze.txt`:** 28 pacotes
- **Total de vulnerabilidades encontradas:** 16 vulnerabilidades em 1 pacote (`urllib3==2.0.6`)
- **Evidências:** [base_pip_audit.txt](file:///home/beatriz/Documents/arquivos_faculdade/SupermarketReceipt-Refactoring-Kata/python_pytest/docs/seguranca/base_pip_audit.txt) e [base_pip_audit.json](file:///home/beatriz/Documents/arquivos_faculdade/SupermarketReceipt-Refactoring-Kata/python_pytest/docs/seguranca/base_pip_audit.json)

### 5.2 Vulnerabilidades Identificadas no `urllib3 2.0.6`

| Pacote | Versão Instalada | ID da Vulnerabilidade | Versões Corrigidas | Descrição Resumida |
| :--- | :---: | :--- | :--- | :--- |
| `urllib3` | 2.0.6 | `PYSEC-2023-212` | `1.26.18, 2.0.7` | Não remoção do corpo de requisição em redirecionamentos HTTP (CVE-2023-45803) |
| `urllib3` | 2.0.6 | `PYSEC-2026-141` | `2.7.0` | Vazamento do cabeçalho `Proxy-Authorization` em redirecionamentos cross-origin (CVE-2026-21441) |
| `urllib3` | 2.0.6 | `PYSEC-2026-1994` | `2.6.0` | Injeção de CRLF em URLs e cabeçalhos de requisição (CVE-2026-38140) |
| `urllib3` | 2.0.6 | `PYSEC-2026-1995` | `1.26.19, 2.2.2` | Loop infinito / bomba de descompressão em fluxos zlib/deflate (CVE-2026-38141) |
| `urllib3` | 2.0.6 | `PYSEC-2026-1996` | `2.6.3` | Reutilização indevida de sessão TLS em conexões agrupadas (CVE-2026-38142) |
| `urllib3` | 2.0.6 | `PYSEC-2026-1998` | `2.6.0` | Falha na validação de representação de endereços IPv6 em URLs (CVE-2026-38144) |
| `urllib3` | 2.0.6 | `PYSEC-2026-1999` | `2.5.0` | Esgotamento de pool de conexões sob condições concorrentes (CVE-2026-38145) |
| `urllib3` | 2.0.6 | `PYSEC-2026-4175` | `2.8.0` | Falha de integridade em streaming de chunks HTTP (CVE-2026-4175) |
| `urllib3` | 2.0.6 | `PYSEC-2026-4177` | `2.8.0` | Vazamento de credenciais em proxy tunneling (CVE-2026-4177) |

### 5.3 Análise da Cadeia de Dependências (`urllib3`)
Comprovação via `python -m pip show`:
- `urllib3`: Localizado em `/home/beatriz/.local/lib/python3.12/site-packages`. Declarado como requerido por: `requests`.
- `requests`: Declarado como requerido por: `empty-files`.
- `empty-files`: Declarado como requerido por: `approvaltests`.
- `approvaltests`: Requisito declarado no `requirements.txt` do projeto.

### 5.4 Auditoria Ampla do Ambiente vs. Escopo do Produto
Ao auditar o ambiente inteiro (`python -m pip_audit`), foram identificadas 13 vulnerabilidades adicionais no pacote `filelock==3.32.7` (`CVE-2026-112940`, `CVE-2026-112913`, etc., corrigidas na versão `4.2.1`).
- Comprovação: `python -m pip show filelock` retornou: `Required-by: safety`.
- **Conclusão:** `filelock` pertence exclusivamente à ferramenta `safety`, estando **fora do escopo do produto**.

---

## 6. Auditoria com Safety

O Safety 3.8.1 foi executado após a autenticação da CLI.

### 6.1 Histórico de Autenticação e Tentativa Inicial
- **Tentativa Inicial (Não Autenticado):** O comando `safety auth status` indicou ausência de token de acesso. A tentativa de execução do scan foi interceptada e registrada em [base_safety_tentativa.txt](file:///home/beatriz/Documents/arquivos_faculdade/SupermarketReceipt-Refactoring-Kata/python_pytest/docs/seguranca/base_safety_tentativa.txt) com erro `EOF when reading a line` (código de saída `1`), respeitando a diretriz de parada mandatória.
- **Autenticação Concluída:** O usuário efetuou o login interativo via navegador com a conta `beatrizestevez29@gmail.com`. A confirmação foi registrada com sucesso:
  ```text
  [2026-11-21 13:41:15]: Safety 3.8.1
  Authenticated as beatrizestevez29@gmail.com
  ```

### 6.2 Execução do Scan
```bash
conda run -n supermarket-sec python -m safety scan 2>&1 | tee docs/seguranca/base_safety.txt
conda run -n supermarket-sec python -m safety scan --output json > docs/seguranca/base_safety.json
```

- **Código de saída:** `0` (sem vulnerabilidades bloqueantes segundo a política)
- **Arquivo analisado:** `python_pytest/requirements.txt`
- **Dependências avaliadas:** 4 dependências diretas (`pytest`, `approvaltests`, `pytest-approvaltests`, `coverage`)
- **Evidências:** [base_safety.txt](file:///home/beatriz/Documents/arquivos_faculdade/SupermarketReceipt-Refactoring-Kata/python_pytest/docs/seguranca/base_safety.txt) e [base_safety.json](file:///home/beatriz/Documents/arquivos_faculdade/SupermarketReceipt-Refactoring-Kata/python_pytest/docs/seguranca/base_safety.json)

### 6.3 Detalhamento do Achado do Safety

A saída registrou: `1 vulnerability found, 1 ignored due to policy`.

- **ID do Alerta:** `SFTY-20260122-20373`
- **Pacote Afetado:** `pytest` (declarado no `requirements.txt` de forma aberta, sem versão pinada)
- **Especificação Vulnerável:** `<9.0.3`
- **Motivo da Ignoração pela Política:** Flag `unpinned-specification` ativada por padrão na política do Safety Platform (`ignore-unpinned-requirements: True`).
- **Realidade no Ambiente Local:** A versão efetivamente instalada e resolvida pelo pip no ambiente Conda foi `pytest==9.2.0`, que já é superior a `9.0.3` e, portanto, **não está vulnerável**.

---

## 7. Triagem Consolidada dos Achados

| Ferramenta | Alvo / Pacote | ID / Regra | Classificação | Justificativa de Relevância |
| :--- | :--- | :--- | :---: | :--- |
| **Bandit** | `tests/test_supermarket.py` (linhas 24 a 31) | `B101:assert_used` | **Falso Positivo / Ruído** | O uso de `assert` em suites de teste do `pytest` é o padrão de desenvolvimento e esperado pela arquitetura da ferramenta. Não há ocorrências em código de produção (`src/`). |
| **pip-audit** | `urllib3==2.0.6` | `PYSEC-2023-212`, `PYSEC-2026-141`, `PYSEC-2026-1994..1999`, `PYSEC-2026-4175..4177` | **Vulnerabilidade Real no Ambiente / Dependência Transitiva de Teste** | Embora a lógica da caixa registradora não utilize HTTP nem rede, a biblioteca `urllib3` instalada possui CVEs ativas. Foi puxada pelo `approvaltests` e herdada do cache local do usuário (`~/.local`). Deve ser atualizada para `>=2.8.0`. |
| **pip-audit** | `filelock==3.32.7` | `CVE-2026-112940` e outros 12 CVEs | **Fora do Escopo do Produto** | O pacote não é dependência do software nem de seus testes; é exigido unicamente pelo executável da ferramenta `safety`. |
| **Safety** | `requirements.txt` (`pytest`) | `SFTY-20260122-20373` | **Falso Positivo / Alerta de Requisito Aberto** | O Safety alertou sobre versões `<9.0.3` devido à ausência de versão pinada no arquivo de requisitos. No entanto, o ambiente real utiliza `pytest==9.2.0`, estando protegido contra essa falha. |

---

## 8. Recomendações de Segurança para as Próximas Fases

1. **Fixar Versões Estritas no `requirements.txt`:**
   Atualmente o arquivo declara dependências sem fixar versões:
   ```text
   pytest
   approvaltests
   pytest-approvaltests
   coverage
   ```
   Recomenda-se fixar versões seguras e testadas (ex: `pytest==9.2.0`), garantindo builds reprodutíveis e previsíveis.
2. **Atualizar a Dependência Transitiva `urllib3`:**
   Atualizar o `urllib3` para a versão `>= 2.8.0` dentro do ambiente ou especificar `urllib3>=2.8.0` explicitamente no `requirements.txt` de desenvolvimento.
3. **Isolamento de Pacotes do Usuário (`--no-user-site`):**
   Configurar ambientes Conda de CI/CD e desenvolvimento para ignorar o site-packages do usuário local (definindo `PYTHONNOUSERSITE=1`), prevenindo que bibliotecas antigas instaladas globalmente pelo desenvolvedor contaminem o projeto.

---

## 9. Inventário de Evidências

Todos os arquivos brutos e saídas de comandos foram preservados na pasta `docs/seguranca/`:

- [base_ambiente.txt](file:///home/beatriz/Documents/arquivos_faculdade/SupermarketReceipt-Refactoring-Kata/python_pytest/docs/seguranca/base_ambiente.txt): Dados de ambiente, data/hora, hardware, proxy, commit git e versões.
- [base_pip_freeze.txt](file:///home/beatriz/Documents/arquivos_faculdade/SupermarketReceipt-Refactoring-Kata/python_pytest/docs/seguranca/base_pip_freeze.txt): Lista de pacotes e versões congeladas antes da instalação das ferramentas.
- [base_bandit.txt](file:///home/beatriz/Documents/arquivos_faculdade/SupermarketReceipt-Refactoring-Kata/python_pytest/docs/seguranca/base_bandit.txt): Saída textual detalhada da varredura do Bandit em `src` e `tests`.
- [base_bandit.json](file:///home/beatriz/Documents/arquivos_faculdade/SupermarketReceipt-Refactoring-Kata/python_pytest/docs/seguranca/base_bandit.json): Relatório estruturado do Bandit em JSON contendo métricas por arquivo e totais.
- [base_pip_audit.txt](file:///home/beatriz/Documents/arquivos_faculdade/SupermarketReceipt-Refactoring-Kata/python_pytest/docs/seguranca/base_pip_audit.txt): Tabela das vulnerabilidades identificadas pelo pip-audit sobre o `base_pip_freeze.txt`.
- [base_pip_audit.json](file:///home/beatriz/Documents/arquivos_faculdade/SupermarketReceipt-Refactoring-Kata/python_pytest/docs/seguranca/base_pip_audit.json): Estrutura detalhada em JSON do pip-audit com metadados de cada CVE/PYSEC.
- [base_safety_tentativa.txt](file:///home/beatriz/Documents/arquivos_faculdade/SupermarketReceipt-Refactoring-Kata/python_pytest/docs/seguranca/base_safety_tentativa.txt): Registro da tentativa inicial de scan antes da autenticação interativa.
- [base_safety.txt](file:///home/beatriz/Documents/arquivos_faculdade/SupermarketReceipt-Refactoring-Kata/python_pytest/docs/seguranca/base_safety.txt): Saída do scan de vulnerabilidades executado pelo Safety autenticado.
- [base_safety.json](file:///home/beatriz/Documents/arquivos_faculdade/SupermarketReceipt-Refactoring-Kata/python_pytest/docs/seguranca/base_safety.json): Relatório estruturado do Safety em formato JSON.
- [relatorio_linha_de_base_seguranca.md](file:///home/beatriz/Documents/arquivos_faculdade/SupermarketReceipt-Refactoring-Kata/python_pytest/docs/seguranca/relatorio_linha_de_base_seguranca.md): Este relatório consolidado.
