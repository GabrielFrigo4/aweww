# 🤝 Guia de Contribuição — Aweww

> Diretrizes de desenvolvimento, testes em modo batch e quality gates para a extensão **Aweww**.

---

## 🚀 Setup Inicial da Bancada (Primeiros Passos)

Para clonar e configurar o repositório localmente com todos os ganchos e quality gates ativados:

```sh
# 1. Clonar o repositório
git clone "https://github.com/GabrielFrigo4/aweww.git" "${HOME}/Documents/aweww"
cd "${HOME}/Documents/aweww"

# 2. Configurar ganchos Git e permissões canônicas
make hooks

# 3. Validar compilação batch de todos os módulos
make test

# 4. Executar a suíte de validação local
make ci
```

> [!IMPORTANT]
> O comando `make hooks` configura `core.hooksPath -> .githooks` e aplica permissões canônicas `0755` aos ganchos de pre-commit e commit-msg. Execute-o sempre após um novo clone.

---

## 🛡️ Invariantes de Engenharia no Aweww

1. **Invariante de `lexical-binding` na Linha 1:**
    - TODO e qualquer arquivo `.el` DEVE conter estritamente `;;; -*- lexical-binding: t -*-` na Linha 1.

2. **Invariante de Clonagem "Out-of-the-Box" (Zero-Tweaks Invariant):**
    - Scripts executáveis (`aweww.sh`) devem possuir modo octal `100755` no Git Index.
    - Arquivos Elisp, documentações e configurações devem possuir modo `100644`.

3. **Compilação Limpa (Hermetismo Batch):**
    - A compilação em lote via `make test` (`emacs -Q --batch -L .`) deve passar com 0 erros.

---

## 🪝 Quality Gates & Validação Local

```sh
make test      # Valida compilação batch de todos os arquivos .el
make compile   # Compila bytecode (.elc)
make clean     # Remove arquivos compilados temporários
make ci        # Executa bateria completa de testes locais
```

Ganchos Git em `.githooks/`:

- **`pre-commit`:** Verifica whitespace, modos octais no Git Index (0755 vs 0644), compilação Elisp e formatação Prettier.
- **`commit-msg`:** Valida formato semântico da mensagem de commit.

---

## 📝 Convenção de Commits Semânticos

As mensagens de commit devem seguir o formato:

```text
<tipo>(<escopo>): <descrição objetiva>
```

Tipos permitidos: `feat`, `fix`, `refactor`, `docs`, `style`, `test`, `ci`, `chore`.

---

## 📖 Referências Canônicas

- [README.md](README.md) — Visão geral da extensão Aweww
- [PRINCIPLES.md](PRINCIPLES.md) — Princípios de Engenharia e Clean Code
- [AGENTS.md](AGENTS.md) — Briefing para agentes autônomos de IA
- [TODO.md](TODO.md) — Roadmap operacional do Aweww
