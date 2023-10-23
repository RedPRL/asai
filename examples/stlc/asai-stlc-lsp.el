;;; asai-stlc-lsp ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; A small shim for the asai_stlc lsp server.

;;; Code:
(require 'lsp-mode)
(define-derived-mode asai-mode prog-mode "Asai")

;; We set OCAMLRUNPARAM=b so that the *asai::stderr* buffer
;; will show us backtraces.
(setenv "OCAMLRUNPARAM" "b")

(defun lsp-asai--create-connection ()
  (lsp-stdio-connection '("dune" "exec" "--" "asai-examples.stlc" "--server")))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-asai--create-connection)
		  :major-modes '(asai-mode)
		  :server-id 'asai
		  :language-id "asai"))

(add-to-list 'lsp-language-id-configuration '(asai-mode . "asai"))

(provide 'asai-stlc-lsp)
;;; asai-stlc-lsp.el ends here
