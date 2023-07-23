;;; elfeed-curate-tests.el --- Elfeed curate tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'elfeed-curate)

(ert-deftest tag-to-group-name-test ()
  (should (string-equal (elfeed-curate-tag-to-group-name "") ""))
  (should (string-equal (elfeed-curate-tag-to-group-name 4) "4"))
  (should (string-equal (elfeed-curate-tag-to-group-name nil) "Nil"))
  (should (string-equal (elfeed-curate-tag-to-group-name '(one two three)) "(One Two Three)"))
  (should (string-equal (elfeed-curate-tag-to-group-name "singleword") "Singleword"))
  (should (string-equal (elfeed-curate-tag-to-group-name "this-is-four-words") "This Is Four Words")))

(provide 'elfeed-curate-tests)

;;; elfeed-curate-tests.el ends here
