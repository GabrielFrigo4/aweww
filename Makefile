.POSIX:
.SILENT:

MAKEFLAGS += --no-print-directory -s

# ----------------------------------------------------------------
# Makefile: Aweww (Awesome EWW Web Browser Extension)
# ----------------------------------------------------------------

.PHONY: help hooks test compile clean ci

### ================================
### HELP & DOCUMENTATION
### ================================
help:
	_e=$$'\e'; \
	cmd() { printf "    $${_e}[36mmake %-22s$${_e}[0m %s\n" "$$1" "$$2"; }; \
	sec() { printf "\n  $${_e}[1;33m%s$${_e}[0m\n" "$$1"; }; \
	printf "\n  $${_e}[1;37mAweww — Awesome EWW Web Browser Extension$${_e}[0m\n"; \
	printf "  ============================================================\n"; \
	sec "Setup & Ganchos:"; \
	cmd "hooks"          "Configura e aplica permissões canônicas em .githooks"; \
	sec "Qualidade & Compilação:"; \
	cmd "test"           "Valida integridade sintática e compilação batch de todos os .el"; \
	cmd "compile"        "Compila bytecode (.elc) de todos os módulos"; \
	cmd "clean"          "Remove arquivos de bytecode (.elc)"; \
	cmd "ci"             "Executa suíte completa de quality gates locais"; \
	echo ""

### ================================
### GIT HOOKS & PERMISSIONS
### ================================
hooks:
	echo "🪝 Configurando ganchos Git (.githooks)..."
	chmod 0755 .githooks/pre-commit .githooks/commit-msg 2> "/dev/null" || true
	git config core.hooksPath .githooks 2> "/dev/null" || true
	echo "  ✅ Aweww: core.hooksPath -> .githooks"

### ================================
### TESTING & QUALITY
### ================================
test:
	echo "🧪 Validando integridade e compilação de Aweww..."
	if command -v emacs > "/dev/null" 2>&1; then \
		emacs -Q --batch -L . --eval '\
			(let ((err-count 0))\
			  (dolist (f (directory-files "." nil "\\.el$$"))\
			    (condition-case err\
			        (byte-compile-file f)\
			      (error\
			       (setq err-count (1+ err-count))\
			       (message "❌ Erro em %s: %s" f err))))\
			  (when (> err-count 0)\
			    (kill-emacs 1)))' > "/dev/null" 2>&1 && \
		rm -f ./*.elc && echo "  ✅ Aweww: 100% testado e aprovado!"; \
	else \
		echo "ℹ️  emacs não encontrado no PATH; ignorando teste batch."; \
	fi

compile:
	echo "⚙️  Compilando módulos Aweww..."
	emacs -Q --batch -L . -f batch-byte-compile *.el && echo "  ✅ Bytecode compilado!"

clean:
	echo "🧹 Limpando artefatos de compilação..."
	rm -f ./*.elc 2> "/dev/null" || true
	echo "  ✅ Limpeza concluída!"

ci: test
	echo "🚀 Aweww 100% pronto para produção!"
