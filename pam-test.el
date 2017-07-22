(require 'pamparam)
(require 'ert)

(ert-deftest pamparam-sm2 ()
  ;; first rep
  (should (equal (pamparam-sm2 '(2.5 1) 5)
                 '(2.6 6 1)))
  (should (equal (pamparam-sm2 '(2.5 1) 4)
                 '(2.5 6 1)))
  (should (equal (pamparam-sm2 '(2.5 1) 3)
                 '(2.3600000000000003 6 1)))
  (should (equal (pamparam-sm2 '(2.5 1) 2)
                 '(2.18 1 1)))
  (should (equal (pamparam-sm2 '(2.5 1) 1)
                 '(1.96 1 1)))
  (should (equal (pamparam-sm2 '(2.5 1) 0)
                 '(1.7000000000000002 1 1)))
  ;; second rep
  (should (equal (pamparam-sm2 (pamparam-sm2 '(2.5 1) 5) 5)
                 '(2.7 16 6 1)))
  ;; third rep
  (should (equal (pamparam-sm2 (pamparam-sm2 (pamparam-sm2 '(2.5 1) 5) 5) 5)
                 '(2.8000000000000003 45 16 6 1)))
  ;; fourth rep
  (should (equal (pamparam-sm2 (pamparam-sm2 (pamparam-sm2 (pamparam-sm2 '(2.5 1) 5) 5) 5) 5)
                 '(2.9000000000000004 131 45 16 6 1))))

(ert-deftest pamparam-equal ()
  (should (pamparam-equal "het plezier"
                          "het genoegen\nhet plezier")))
