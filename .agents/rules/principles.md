# 🌐 Aweww Engineering Principles & Guidelines

> Regras de engenharia e diretrizes de desenvolvimento para o Aweww (Awesome EWW Web Browser Extension).

---

## 🏛️ Invariantes de Código Elisp

1. **Robustez & Fail-Safe:** Todas as funcionalidades do navegador EWW devem operar de forma resiliente tanto em terminais de texto quanto em ambientes gráficos.
2. **Arquitetura de 3 Camadas de Comentários:**
    - Topo do arquivo: Header Banner com 64 `-` (`;; ----------------------------------------------------------------`).
    - Seções principais: Delimitador com 32 `=` (`;; ================================`).
    - Subseções: Delimitador com 32 `-` (`;; --------------------------------`).
    - Sem comentários narrativos inline.
3. **Escopo de Variáveis & Nomenclatura:** Usar prefixo canônico `aweww-` para todas as funções e variáveis.
4. **Lexical Binding Obrigatório (Linha 1):** Todo arquivo `.el` DEVE começar estritamente com `;;; -*- lexical-binding: t -*-` na Linha 1.
5. **Byte-Compilation Hermética:** O código deve compilar via `make test` sem warnings críticos ou erros de sintaxe.
