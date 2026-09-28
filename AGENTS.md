# 🌐 Aweww — AI Agent Briefing

> Extensão de navegação web Awesome EWW para GNU Emacs, integrada à Suíte de Editores do Universal Environment.

---

## 🧭 Identidade e Papel

O **Aweww** é um pacote Elisp soberano que enriquece a experiência de navegação web no GNU Emacs através do navegador embutido EWW, provendo histórico elegante, renderização otimizada e integração com buffers do editor.

---

## ⚠️ Regras Críticas para Agentes de IA

1. **Repositório Independente:** Commits e branches são deste repositório (`aweww`).
2. **Zero Comentários Narrativos:** Use a arquitetura de comentários em 3 camadas (64 `-` no topo, 32 `=` para seções e 32 `-` para subseções).
3. **Hermetismo de Produção & Invariante `rm -rf .agents`:** Repositório 100% autônomo. Zero dependência de código de produção para `.agents/` ou `skills/`.
4. **Bancada de Desenvolvimento vs. Runtimes:** Em produção no Emacs, o pacote é carregado diretamente pelo `init.el`.
5. **Invariante de Clonagem "Out-of-the-Box" (Zero-Tweaks Git Invariant):** O repositório deve funcionar imediatamente após `git clone`. Modos octais no Git Index DEVEM ser rigorosamente `0755` para scripts executáveis e `0644` para arquivos Elisp e documentação.
6. **Governança de Roadmap (Opção C):** O repositório mantém seu [TODO.md](TODO.md) atualizado com a Matriz de Status e Roadmap.
7. **Invariante de `lexical-binding` na Linha 1:** TODO e qualquer arquivo Emacs Lisp (`.el`) criado ou mantido DEVE conter obrigatoriamente `;;; -*- lexical-binding: t -*-` estritamente na Linha 1.
8. **Refatoração Sem Legado / Soberania Monousuário (Clean-Break / Zero-Cruft Invariant):** O ecossistema é estritamente pessoal, governado e operado por um único desenvolvedor soberano (Gabriel Frigo). É terminantemente proibido manter "sujeira" de retrocompatibilidade, shims temporários, wrappers obsoletos, seções de compatibilidade legada ou aliases de transição ao renomear variáveis, comandos, funções, diretórios ou arquivos, salvo se expressamente ordenado pelo usuário. Toda refatoração deve ser atômica, direta, definitiva e limpa (_clean break_), expurgando o identificador antigo integralmente da base de código.

---

## 🛡️ Regra da Proatividade e Correção Contínua (Boy Scout Rule)

O agente de IA **DEVE SER ATIVAMENTE PROATIVO** na manutenção e aplicação dos padrões canônicos deste repositório.

Se durante a execução de qualquer tarefa o agente identificar qualquer inconformidade, **NÃO DEVE HESITAR NEM IGNORAR**:

1. **Notificar concisamente** o usuário sobre a divergência encontrada.
2. **Corrigir imediatamente a inconformidade**, aplicando o padrão canônico correspondente:
    - **Lexical Binding Obrigatório:** Garantir `;;; -*- lexical-binding: t -*-` rigorosamente na linha 1 de todos os arquivos `.el`.
    - **Comentários Narrativos:** Eliminar imediatamente comentários óbvios que apenas narram código executável.
    - **Banners Estruturais:** Ajustar réguas para exatamente 64 hífens no topo ou 32 caracteres com `### ` no corpo.
    - **Portabilidade POSIX:** Substituir bashismos (`[[ ]]`, `&>`, arrays, `source`) por sintaxe estrita POSIX `/bin/sh`.
    - **Shebang Universal:** Garantir sempre `#!/usr/bin/env sh` ou `#!/usr/bin/env python3`.
    - **Sequências ANSI & Escapes:** Eliminar terminantemente octais (`\033`, `\001`) para caracteres ou bytes. Usar `[ -t 1 ] && echo -n $'\e...'` para sequências de escape, notação hexadecimal (`\x01`, `\x1b`) para bytes/controles e fugir de `printf` desnecessário.
    - **Redirecionamento Seguro:** Envolver destinos em aspas duplas (ex: `> "/dev/null" 2>&1`).
    - **Makefiles:** Assegurar cabeçalho `.POSIX: .SILENT:`, `MAKEFLAGS += --no-print-directory -s`, alinhamento estético de variáveis e zero `@` redundante.
    - **Permissões Canônicas:** Aplicar 4 dígitos octais (`chmod 0755`, `chmod 0644`).
    - **Invariante Out-of-the-Box:** Garantir modos octais corretos no Git Index e compilação batch sem intervenção manual pós-clone.
    - **Curadoria Cognitiva:** Capturar decisões estruturais e regras tácitas em skills locais compactas (`.agents/skills/`).
    - **Refatoração Sem Legado:** Expurgar sumariamente aliases obsoletos, variáveis mortas e shims de compatibilidade deixados para trás em renomeações passadas, mantendo o código puro e direto.

## 📖 Referências Obrigatórias

- **[ENVIRONMENT.md](ENVIRONMENT.md)**: Arquitetura global do ecossistema
- **[PRINCIPLES.md](PRINCIPLES.md)**: Os 22 Princípios de Engenharia UNIX + Clean Code
- **[TODO.md](TODO.md)**: Planejamento estratégico e matriz de status
- **[.agents/rules/principles.md](.agents/rules/principles.md)**: Regras específicas de engenharia Elisp
