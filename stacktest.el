;;; stacktest.el --- Easy Python test running in Emacs
;;
;; Copyright (C) 2009 Jason Pellerin, Augie Fackler
;; Copyright (C) 2015 Sean Dague
;;
;; Derived from the excellent nose.el at
;; https://bitbucket.org/durin42/nosemacs/overview
;;
;; Licensed under the same terms as Emacs.

;;; Commentary:
;; This provides an inline testing interface when working on OpenStack
;; projects. The OpenStack testing interfaces assumes that tests are
;; run via tox with some standard testing targets. 'tox -e py27' is
;; the defined testing interface for basic unit tests in a project.
;;
;; Tox is typically calling ``testr`` for a top level test run. testr
;; works by discovering all tests, then pruning them based on a
;; regex. If using testr, you can also directly call subunit.run or
;; testtools.run to skip discovery and just use a particular
;; testable. This is more useful for testing just one specific
;; function.
;;
;; Expected use cases:
;;   - run tests for this test function I'm in right now: 'stacktest-one
;;   - run tests for this test function with OS_DEBUG=True
;;   - run tests for this file
;;   - run tests for this file with OS_DEBUG=True
;;   - run all the tests
;;

;;; Installation

;; In your emacs config:
;;
;; (require 'stacktest)

;; ; next line only for people with non-eco non-global test runners
;; ; (add-to-list 'nose-project-names "my/crazy/runner")

;; Note that if your global nose isn't called "nosetests", then you'll want to
;; redefine nose-global-name to be the command that should be used.

;; By default, the root of a project is found by looking for any of the files
;; 'setup.py', '.hg' and '.git'. You can add files to check for to the file
;; list:
;;
;; ; (add-to-list 'nose-project-root-files "something")

;; or you can change the project root test to detect in some other way
;; whether a directory is the project root:
;;
;; ; (setq nose-project-root-test (lambda (dirname) (equal dirname "foo")))

;; If you want dots as output, rather than the verbose output:
;; (defvar nose-use-verbose nil) ; default is t

;; nose.el adds a minor mode called 'nose' that's currently only used to
;; manage keybindings and provide a hook for changing the behaviour of
;; the nose output buffer.

;; This is the recommended way to activate nose keybindings when viewing
;; Python files:

;; (add-hook 'python-mode-hook (lambda () (nose-mode t)))

;; nose-mode is also activated when nose displays the buffer that shows
;; the output of nosetests.

;; Code like that given below can be used to change nose.el's keybindings.
;; The bindings shown are nose.el's default bindings. If you wish to use
;; these bindings, then you don't need to include this code in .emacs

;; (define-key nose-mode-map "\C-ca" 'nosetests-all)
;; (define-key nose-mode-map "\C-cm" 'nosetests-module)
;; (define-key nose-mode-map "\C-c." 'nosetests-one)
;; (define-key nose-mode-map "\C-cc" 'nosetests-again)
;; (define-key nose-mode-map "\C-cpa" 'nosetests-pdb-all)
;; (define-key nose-mode-map "\C-cpm" 'nosetests-pdb-module)
;; (define-key nose-mode-map "\C-cp." 'nosetests-pdb-one)

(require 'cl) ;; for "reduce"

(defvar stacktest-project-names '("eco/bin/test"))

(defvar stacktest-project-root-files '(".tox")
  "A list of file names. A directory with any of the files
present is considered to be a 'project root'.")

(defvar stacktest-toxcmd "tox"
  "The command to run tests at the top level. Typically tox")

(defvar stacktest-testrunner ".tox/py27/bin/python -m subunit.run"
  "What test runner should be run on tests. This defaults to
  using subunit.run directly, which requires use of a filter as
  well. Other options would be to use testtools.run directly,
  which has less useful output")

(defvar stacktest-filter ".tox/py27/bin/subunit-trace"
  "What filter to use. subunit-trace is required if we use
  subunit.run to run the tests.")

(defvar stacktest-project-root-test 'stacktest-project-root
  "The function to use to discover the root of the current
project. The function should return a directory path.")

(defvar stacktest-global-name "echo"
  "The command to be run when executing stacktest.")

(defvar stacktest-use-verbose t
  "If t, then the 'verbose' option is passed to stacktest.")

(defvar stacktest-finish-functions nil
  "Functions to call when stacktest complete. See compilation-finish-functions.")

(defvar stacktest--last-run-params nil
  "Stores the last parameters passed to run-stacktest")

(defvar-local stacktest-local-project-root nil
  "This variable will define the project root when a 'current
file name' is inapplicable to the current buffer. An example is
during display of stacktesttest results (compilation buffer.) It
should be set local to such buffers at the time when they're
created." )

(define-minor-mode stacktest-mode
  "Minor mode enabling stacktest key commands."
  :keymap
  '(("\C-c.a" . stacktest-all)
    ("\C-c.m" . stacktest-module)
    ("\C-c.o" . stacktest-one)
    ("\C-c.c" . stacktest-again)
    ("\C-c.pa" . stacktest-debug-all)
    ("\C-c.pm" . stacktest-debug-module)
    ("\C-c.po" . stacktest-debug-one)) )

(defun stacktest--finish-function-hook (buffer message)
  (if (string= (buffer-name buffer) "*stacktest*")
	  (dolist (func stacktest-finish-functions)
		(funcall func buffer message) )))

(add-to-list 'compilation-finish-functions 'stacktest--finish-function-hook)

(defun run-stacktest (&optional tests debug failed)
  "run stacktest"
  (setq stacktest--last-run-params (list tests debug failed))

  (let* ((stacktest (stacktest-find-test-runner))
         (where (or stacktest-local-project-root (stacktest-find-project-root)))
         (args (concat ;;; disabling pdb (if debug "--pdb" "")
                       " "
                       (if failed "--failed" "")))
         (tnames (if tests tests ""))
         ; for the venv invocation to work we need to be at the right
         ; starting point so the cd is required.
         (testrunner (if tests
                         (format "%s%s" where stacktest-testrunner)
                       stacktest-tox))
         (testfilter (if tests (format "| %s%s" where stacktest-filter) ""))
         )
    (if (not where)
        (error
         (format (concat "abort: stacktest couldn't find a project root, "
                         "looked for any of %S") stacktest-project-root-files)))

    ;; Execute stacktest and display the result in a compilation buffer.
    ;;
    ;; Store the active project root in a buffer-local variable, so that stacktest
    ;; can invoked from it by the user after execution is complete. This is
    ;; necessary because the compilation buffer doesn't have a filename from
    ;; which it could be discovered.
    (funcall (lambda (command)
                  (let ((compilation-error-regexp-alist
                         '(("  File \"\\(.*\\)\", line \\([0-9]+\\), in test_" 1 2))))
                    (save-current-buffer
					  (set-buffer (compilation-start command
													 nil
													 (lambda (mode) (concat "*stacktest*"))))
					  (setq-local stacktest-local-project-root where))))
             (format
              (concat "cd %s && "
                      (if debug "OS_DEBUG=True " "")
                      "%s %s %s %s")
              where testrunner args tnames testfilter)))
  )

(defun stacktest-all (&optional debug failed)
  "run all tests"
  (interactive)
  (run-stacktest nil debug failed))

(defun stacktest-failed (&optional debug)
  "run stacktest with the --failed option"
  (interactive)
  (stacktest-all debug t))

(defun stacktest-debug-all ()
  "run all tests using the python debugger"
  (interactive)
  (stacktest-all t))

(defun stacktest-module (&optional debug)
  "run stacktest (via eggs/bin/test) on current buffer"
  (interactive)
  (run-stacktest buffer-file-name debug))

(defun stacktest-debug-module ()
  "run tests in the current buffer using the Python debugger"
  (interactive)
  (stacktest-module t))

(defun stacktest-one (&optional debug)
  "run stacktest (via eggs/bin/test) on testable thing
 at point in current buffer"
  (interactive)
  (run-stacktest (stacktest-subunit-testable) debug))

(defun stacktest-debug-one ()
  "run stacktest (via eggs/bin/test) on testable thing
 at point in current buffer using the Python debugger"
  (interactive)
  (stacktest-one t))

(defun stacktest-again ()
  "runs the most recently executed 'stacktest' command again"
  (interactive)
  (apply 'run-stacktest stacktest--last-run-params))

(defun stacktest-find-test-runner ()
  (message
   (let ((result
		  (reduce '(lambda (x y) (or x y))
				  (mapcar 'stacktest-find-test-runner-names stacktest-project-names))))
	 (if result
		 result
	   stacktest-global-name))))

(defun stacktest-find-test-runner-names (runner)
  "find eggs/bin/test in a parent dir of current buffer's file"
  (stacktest-find-test-runner-in-dir-named
   (file-name-directory (stacktest--context-path)) runner))

(defun stacktest-find-test-runner-in-dir-named (dn runner)
  (let ((fn (expand-file-name runner dn)))
    (cond ((file-regular-p fn) fn)
      ((equal dn "/") nil)
      (t (stacktest-find-test-runner-in-dir-named
          (file-name-directory (directory-file-name dn))
          runner)))))

(defun stacktest-subunit-testable ()
  (interactive)
  (let* ((testtable (stacktest-py-testable))
         ; where is the root of the project
         (where (or stacktest-local-project-root (stacktest-find-project-root)))
         ; make a match of the rootdir vs the buffer-name
         (tempstr (string-match where (buffer-file-name)))
         ; splice off the testpath by substr the match-end
         (testpath (substring (buffer-file-name) (match-end 0)))
         ; this will be of the format nova.foo.test.py
         (testpath_transform (mapconcat 'identity (split-string testpath "/") "."))
         ; now strip off the final .py
         (testmodule (substring testpath_transform 0 (- (length testpath_transform) 3)))
         )
    (message "%s.%s" testmodule testtable)
    (format "%s.%s" testmodule testtable)
    )
)

(defun stacktest-py-testable ()
  (let* ((inner-obj (inner-testable))
         (outer (outer-testable))
         ;; elisp can't return multiple values
         (outer-def (car outer))
         (outer-obj (cdr outer)))
    (cond ((equal outer-def "def") outer-obj)
          ((equal inner-obj outer-obj) outer-obj)
          (t (format "%s.%s" outer-obj inner-obj)))))

(defun inner-testable ()
  (save-excursion
    (re-search-backward
     "^\\(?: \\{0,4\\}\\|\t\\)\\(class\\|def\\)[ \t]+\\([a-zA-Z0-9_]+\\)" nil t)
    (buffer-substring-no-properties (match-beginning 2) (match-end 2))))

(defun outer-testable ()
  (save-excursion
    (re-search-backward
     "^\\(class\\|def\\)[ \t]+\\([a-zA-Z0-9_]+\\)" nil t)
    (let ((result
            (buffer-substring-no-properties (match-beginning 2) (match-end 2))))

      (cons
       (buffer-substring-no-properties (match-beginning 1) (match-end 1))
       result))))

(defun stacktest--context-path ()
  "Returns the best known file path, given available context,
 from which stacktest.el could use to figure out the location of
important resources such as project roots."
  (or buffer-file-name stacktest-local-project-root))

(defun stacktest-find-project-root (&optional dirname)
  (let ((dn
         (if dirname
             dirname
           (file-name-directory (stacktest--context-path)))))
    (cond ((funcall stacktest-project-root-test dn) (expand-file-name dn))
          ((equal (expand-file-name dn) "/") nil)
        (t (stacktest-find-project-root
             (file-name-directory (directory-file-name dn)))))))

(defun stacktest-project-root (dirname)
  (reduce '(lambda (x y) (or x y))
          (mapcar (lambda (d) (member d (directory-files dirname)))
                  stacktest-project-root-files)))

(provide 'stacktest)

;;; stacktest.el ends here
