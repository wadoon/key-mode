;;; key-mode-tests.el --- Test cases for key-mode
;;; Commentary:
;;; Code:

(require 'buttercup)
(require 'key-mode)
(require 'ert)

(defmacro key--should-indent (from to)
  "Assert that we indent text FROM producing text TO in `julia-mode'."
  `(with-temp-buffer
     (let ((key-indent-offset 4))
       (key-mode)
       (insert ,from)
       (indent-region (point-min) (point-max))
       (should (equal (buffer-substring-no-properties (point-min) (point-max))
                      ,to)))))

(defmacro key--should-font-lock (text pos face)
  "Assert that TEXT at position POS gets font-locked with FACE in `julia-mode'."
  `(with-temp-buffer
     (key-mode)
     (insert ,text)
     (if (fboundp 'font-lock-ensure)
         (font-lock-ensure (point-min) (point-max))
       (with-no-warnings
         (font-lock-fontify-buffer)))
     (should (eq ,face (get-text-property ,pos 'face)))))


(describe "Identation checks"
  (it "test the setup"
    (key--should-indent "" ""))
  
  (it "declarations"
    (key--should-indent
     "
\sorts {
A; B;
C;
D;
}
\predicates {
                       p(A);
q(B, A);
                mega(A, 
B,
C);

}
"
     "
\sorts {
    A; B;
    C;
    D;
}
\predicates {
    p(A);
    q(B, A);
    mega(A, 
        B,
        C);
}
")))







(provide 'key-mode-tests)
;;; key-mode-tests.el ends here
