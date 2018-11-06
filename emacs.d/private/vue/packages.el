(defconst vue-packages
  '(
    add-node-modules-path
    company
    editorconfig
    flycheck
    (lsp-vue :requires lsp-mode)
    smartparens
    vue-mode
    ))

(defun vue/post-init-add-node-modules-path ()
  (add-hook 'vue-mode-hook #'add-node-modules-path))

(defun vue/post-init-company ()
  (add-hook 'vue-mode-local-vars-hook
            (lambda ()
              (spacemacs|add-company-backends
                :backends company-lsp
                :modes vue-mode
                :append-hooks nil
                :call-hooks t)
              (company-mode))))

(defun vue/post-init-editorconfig ()
  (add-to-list 'editorconfig-indentation-alist
               '(vue-mode css-indent-offset
                          js-indent-level
                          typescript-indent-level
                          sgml-basic-offset
                          ssass-tab-width
                          )))

(defun vue/post-init-flycheck ()
  (require 'flycheck)
  ;; Vetur, the Vue language server doesn't seem to provide any syntax checking
  ;; capabilities yet, so use the ESLint checker for now.
  (flycheck-add-mode 'javascript-eslint 'vue-mode)
  (flycheck-add-mode 'javascript-eslint 'vue-html-mode)
  (spacemacs/enable-flycheck 'vue-mode)
  )

(defun vue/init-lsp-vue ()
  (use-package lsp-vue
    :defer t
    :commands lsp-vue-mmm-enable
    :init
    (add-hook 'vue-mode-local-vars-hook #'lsp-vue-mmm-enable)))

(defun vue/post-init-smartparens ()
  (add-hook 'vue-mode-hook 'smartparens-mode))

(defun vue/init-vue-mode ()
  (use-package vue-mode
    :defer t))
