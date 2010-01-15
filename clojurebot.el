(require 'clojure-mode)
(require 'swank-clojure)

(defvar clojurebot-dir (file-name-directory
                        (or (buffer-file-name) load-file-name)))

;; (add-to-list 'swank-clojure-extra-vm-args "-Djava.security.manager")
;; (add-to-list 'swank-clojure-extra-vm-args
;;              (format "-Djava.security.policy=\"file://%sexample.policy"
;;                      clojurebot-dir))

(unless (functionp 'clojure-project)
  (defun clojure-project (path)
    "Setup classpaths for a clojure project and starts a new SLIME session.
  Kills existing SLIME session, if any."
    (interactive (list
                  (ido-read-directory-name
                   "Project root: "
                   (locate-dominating-file default-directory "pom.xml"))))
    (when (get-buffer "*inferior-lisp*")
      (kill-buffer "*inferior-lisp*"))
    (defvar swank-clojure-extra-vm-args nil)
    (defvar slime-lisp-implementations nil)
    ;; (add-to-list swank-clojure-extra-vm-args
    ;;              (format "-Dclojure.compile.path=%s"
    ;;                      (expand-file-name "target/classes/" path)))
    (setq swank-clojure-binary nil
          swank-clojure-jar-path (expand-file-name "target/dependency/" path)
          swank-clojure-extra-classpaths
          (append (mapcar (lambda (d) (expand-file-name d path))
                          '("src/" "target/classes/" "test/"))
                  (let ((lib (expand-file-name "lib" path)))
                    (if (file-exists-p lib)
                        (directory-files lib t ".jar$"))))
          slime-lisp-implementations
          (cons `(clojure ,(swank-clojure-cmd) :init swank-clojure-init)
                (remove-if #'(lambda (x) (eq (car x) 'clojure))
                           slime-lisp-implementations)))
    (save-window-excursion
      (slime))))

(clojure-project clojurebot-dir)

(add-hook 'slime-connected-hook
          (lambda ()
            (slime-eval-async '(swank:eval-and-grab-output
                                "(require 'hiredman.clojurebot)"))))