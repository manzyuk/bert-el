(require 'bindat)
(require 'ert)

(defvar bert-bindat-spec
  '((tag u8)
    (union (tag)
           (97  (integer u8))
           (98  (integer u32))
           (99  (float-string str 31))
           (100 (length u16)
                (atom-name str (length)))
           (104 (arity u8)
                (elements repeat (arity)
                          (struct bert-bindat-spec)))
           (105 (arity u32)
                (elements repeat (arity)
                          (struct bert-bindat-spec)))
           (106)
           (107 (length u16)
                (characters str (length)))
           (108 (length u32)
                (elements repeat (length)
                          (struct bert-bindat-spec))
                (tail struct bert-bindat-spec))
           (109 (length u32)
                (data vec (length)))
           (110 (length u8)
                (sign u8)
                (digits vec (length)))
           (111 (length u32)
                (sign u8)
                (digits vec (length))))))

(defun bert-pack (obj)
  (bindat-pack (cons (list 'magic 'u8) bert-bindat-spec)
               (cons (cons 'magic 131) (bert-encode obj))))

(defadvice bert-pack (around bert-pack-around activate)
  (letf (((symbol-function 'lsh) #'ash))
    ad-do-it))

(defun bert-encode (obj)
  (cond ((integerp     obj) (bert-encode-integer    obj))
        ((floatp       obj) (bert-encode-float      obj))
        ((listp        obj) (bert-encode-list       obj))
        ;; The case of symbol must come after the case of list because
        ;; nil is a symbol but should be packed as an empty list.
        ((symbolp      obj) (bert-encode-symbol     obj))
        ((vectorp      obj) (bert-encode-vector     obj))
        ((stringp      obj) (bert-encode-string     obj))
        ((hash-table-p obj) (bert-encode-hash-table obj))
        (t (error "cannot encode %S" obj))))

(defun bert-encode-integer (integer)
  `((tag . ,(if (and (>= integer 0) (< integer 256)) 97 98))
    (integer . ,integer)))

(defun bert-pad-right (string width char)
  (concat string
          (make-string (max 0 (- width (length string))) char)))

(defun bert-encode-float (float)
  (let ((float-string (bert-pad-right (format "%15.15e" float) 31 ?\000)))
    `((tag . 99)
      (float-string . ,float-string))))

(defun bert-encode-symbol (symbol)
  (let* ((name (symbol-name symbol))
         (len (length name)))
    (assert (< len 256) t "symbol name is too long (>= 256): %S" symbol)
    `((tag . 100)
      (length . ,len)
      (atom-name . ,name))))

(defun bert-encode-vector (data)
  `((tag . ,(if (< (length data) 256) 104 105))
    (arity . ,(length data))
    (elements . ,(mapcar #'bert-encode data))))

(defun bert-encode-list (list)
  (if (null list)
      `((tag . 106))
    `((tag . 108)
      (length . ,(length list))
      (elements . ,(mapcar #'bert-encode list))
      (tail . ((tag . 106))))))

(defun bert-encode-string (string)
  `((tag . 109)
    (length . ,(length string))
    (data . ,(string-to-vector string))))

(defun bert-encode-hash-table (hash-table)
  (let (table)
    (maphash (lambda (key value)
               (push (vector key value) table))
             hash-table)
    (bert-encode (vector 'bert 'dict table))))

(defun bert-unpack (string)
  (let* ((struct
          (bindat-unpack (cons (list 'magic 'u8) bert-bindat-spec) string))
         (magic (bindat-get-field struct 'magic)))
    (assert (= magic 131) t "bad magic: %d" magic)
    (bert-decode struct)))

(defadvice bert-unpack (around bert-unpack-around activate)
  (letf (((symbol-function 'lsh) #'ash))
    ad-do-it))

(defun bert-decode (struct)
  (case (bindat-get-field struct 'tag)
    ((97 98)   (bert-decode-integer       struct))
    (99        (bert-decode-float         struct))
    (100       (bert-decode-atom          struct))
    ((104 105) (bert-decode-complex       struct))
    (106       nil)
    (107       (bert-decode-string        struct))
    (108       (bert-decode-list          struct))
    (109       (bert-decode-binary        struct))
    ((110 111) (error "cannot decode bignums"))))

(defun bert-decode-integer (struct)
  (bindat-get-field struct 'integer))

(defun bert-decode-float (struct)
  (read (bindat-get-field struct 'float-string)))

(defun bert-decode-atom (struct)
  (intern (bindat-get-field struct 'atom-name)))

(defun bert-decode-complex (struct)
  (let ((tuple (bert-decode-tuple struct)))
    (if (and (> (length tuple) 2)
             (eq (aref tuple 0) 'bert))
        (case (aref tuple 1)
          (dict (bert-decode-dict (aref tuple 2)))
          (t tuple))
      tuple)))

(defun bert-decode-tuple (struct)
  (let ((elements (bindat-get-field struct 'elements)))
    (apply #'vector (mapcar #'bert-decode elements))))

(defun bert-decode-dict (table)
  (let ((hash-table (make-hash-table)))
    (dolist (row table hash-table)
      (puthash (aref row 0) (aref row 1) hash-table))))

(defun bert-decode-string (struct)
  (bindat-get-field struct 'characters))

(defun bert-decode-list (struct)
  (let* ((elements (bindat-get-field struct 'elements))
         (tail (bindat-get-field struct 'tail))
         (list (mapcar #'bert-decode elements)))
    (setcdr (last list) (bert-decode tail))
    list))

(defun bert-decode-binary (struct)
  (map 'string #'identity (bindat-get-field struct 'data)))

(ert-deftest bert-pack-integer ()
  "Test packing of integers."
  (should (equal (bert-pack 0)
                 (unibyte-string 131 97 0)))
  (should (equal (bert-pack 42)
                 (unibyte-string 131 97 42)))
  (should (equal (bert-pack 255)
                 (unibyte-string 131 97 255)))
  (should (equal (bert-pack 256)
                 (unibyte-string 131 98 0 0 1 0)))
  (should (equal (bert-pack 12345)
                 (unibyte-string 131 98 0 0 48 57)))
  (should (equal (bert-pack -42)
                 (unibyte-string 131 98 255 255 255 214))))

(ert-deftest bert-pack-float ()
  "Test packing of floats."
  (should (equal (bert-pack 3.1415926)
                 (unibyte-string
                  131 99 51 46 49 52 49 53 57 50 54 48 48 48 48 48 48 48 48
                  101 43 48 48 0 0 0 0 0 0 0 0 0 0)))
  (should (equal (bert-pack 9.10938291e-31)
                 (unibyte-string
                  131 99 57 46 49 48 57 51 56 50 57 49 48 48 48 48 48 48 48
                  101 45 51 49 0 0 0 0 0 0 0 0 0 0)))
  (should (equal (bert-pack -6.62606957e-34)
                 (unibyte-string
                  131 99 45 54 46 54 50 54 48 54 57 53 55 48 48 48 48 48 48
                  48 101 45 51 52 0 0 0 0 0 0 0 0 0))))

(ert-deftest bert-pack-symbol ()
  "Test packing of symbols."
  (should (equal (bert-pack (intern ""))
                 (unibyte-string 131 100 0 0)))
  (should (equal (bert-pack (intern "foo"))
                 (unibyte-string 131 100 0 3 102 111 111)))
  (should (equal (bert-pack (intern "foo bar"))
                 (unibyte-string 131 100 0 7 102 111 111 32 98 97 114))))

(ert-deftest bert-pack-list ()
  "Test packing of lists."
  (should (equal (bert-pack nil)
                 (unibyte-string 131 106)))
  (should (equal (bert-pack (list 1 2 3))
                 (unibyte-string 131 108 0 0 0 3 97 1 97 2 97 3 106)))
  (should (equal (bert-pack (list 1 (list 2 3)))
                 (unibyte-string
                  131 108 0 0 0 2 97 1 108 0 0 0 2 97 2 97 3 106 106)))
  (should (equal (bert-pack (list 1 2.718 'foo))
                 (unibyte-string
                  131 108 0 0 0 3 97 1 99 50 46 55 49 56 48 48 48 48 48 48
                  48 48 48 48 48 48 101 43 48 48 0 0 0 0 0 0 0 0 0 0 100 0
                  3 102 111 111 106))))

(ert-deftest bert-pack-vector ()
  "Test packing of vectors."
  (should (equal (bert-pack (vector))
                 (unibyte-string 131 104 0)))
  (should (equal (bert-pack (vector 1 2 3))
                 (unibyte-string 131 104 3 97 1 97 2 97 3)))
  (should (equal (bert-pack (vector 1 (vector 2 3)))
                 (unibyte-string 131 104 2 97 1 104 2 97 2 97 3)))
  (should (equal (bert-pack (vector 1 2.718 'foo))
                 (unibyte-string
                  131 104 3 97 1 99 50 46 55 49 56 48 48 48 48 48 48 48 48
                  48 48 48 48 101 43 48 48 0 0 0 0 0 0 0 0 0 0 100 0 3 102
                  111 111))))

(ert-deftest bert-pack-string ()
  "Test packing of strings."
  (should (equal (bert-pack "")
                 (unibyte-string 131 109 0 0 0 0)))
  (should (equal (bert-pack "foo")
                 (unibyte-string 131 109 0 0 0 3 102 111 111))))

(ert-deftest bert-pack-hash-table ()
  "Test packing of hash-tables."
  (should (equal (bert-pack (make-hash-table))
                 (unibyte-string
                  131 104 3 100 0 4 98 101 114 116 100 0 4 100 105 99 116
                  106)))
  (should (equal (bert-pack
                  (let ((hash-table (make-hash-table)))
                    (puthash 1 "a" hash-table)
                    hash-table))
                 (unibyte-string
                  131 104 3 100 0 4 98 101 114 116 100 0 4 100 105 99 116
                  108 0 0 0 1 104 2 97 1 109 0 0 0 1 97 106))))

(ert-deftest bert-unpack-integer ()
  "Test unpacking of integers."
  (should (equal (bert-unpack (bert-pack     0))     0))
  (should (equal (bert-unpack (bert-pack    42))    42))
  (should (equal (bert-unpack (bert-pack   255))   255))
  (should (equal (bert-unpack (bert-pack   256))   256))
  (should (equal (bert-unpack (bert-pack 12345)) 12345))
  (should (equal (bert-unpack (bert-pack   -42))  -42)))

(ert-deftest bert-unpack-float ()
  "Test unpacking of floats."
  (should (equal (bert-unpack (bert-pack       3.1415926))       3.1415926))
  (should (equal (bert-unpack (bert-pack  9.10938291e-31))  9.10938291e-31))
  (should (equal (bert-unpack (bert-pack -6.62606957e-34)) -6.62606957e-34)))

(ert-deftest bert-unpack-symbol ()
  "Test unpacking of symbols."
  (should (equal (bert-unpack (bert-pack (intern ""))) (intern "")))
  (should (equal (bert-unpack (bert-pack (intern "foo"))) (intern "foo")))
  (should (equal (bert-unpack (bert-pack (intern "foo bar")))
                 (intern "foo bar"))))

(ert-deftest bert-unpack-list ()
  "Test unpacking of lists."
  (should (equal (bert-unpack (bert-pack nil)) nil))
  (should (equal (bert-unpack (bert-pack (list 1 2 3))) (list 1 2 3)))
  (should (equal (bert-unpack (bert-pack (list 1 (list 2 3))))
                 (list 1 (list 2 3))))
  (should (equal (bert-unpack (bert-pack (list 1 2.718 'foo)))
                 (list 1 2.718 'foo))))

(ert-deftest bert-unpack-vector ()
  "Test unpacking of vectors."
  (should (equal (bert-unpack (bert-pack (vector))) (vector)))
  (should (equal (bert-unpack (bert-pack (vector 1 2 3))) (vector 1 2 3)))
  (should (equal (bert-unpack (bert-pack (vector 1 (vector 2 3))))
                 (vector 1 (vector 2 3))))
  (should (equal (bert-unpack (bert-pack (vector 1 2.718 'foo)))
                 (vector 1 2.718 'foo)))
  (should (let ((obj (bert-unpack
                      (bert-pack
                       (vector 'bert
                               'dict
                               (list (vector 1 "a")
                                     (vector 2 "b")))))))
            (and (hash-table-p obj)
                 (hash-table-count obj)
                 (equal (gethash 1 obj) "a")
                 (equal (gethash 2 obj) "b"))))
  (should (equal (bert-unpack (bert-pack (vector 'bert 'true)))
                 (vector 'bert 'true))))

(ert-deftest bert-unpack-string ()
  "Test unpacking of strings."
  (should (equal (bert-unpack (bert-pack "")) ""))
  (should (equal (bert-unpack (bert-pack "foo")) "foo")))

(ert-deftest bert-unpack-hash-table ()
  "Test unpacking of hash-tables."
  (should (let ((obj (bert-unpack (bert-pack (make-hash-table)))))
            (and (hash-table-p obj)
                 (= (hash-table-count obj) 0))))
  (should (let ((obj (bert-unpack
                      (bert-pack
                       (let ((hash-table (make-hash-table)))
                         (puthash 1 "a" hash-table)
                         hash-table)))))
            (and (hash-table-p obj)
                 (= (hash-table-count obj) 1)
                 (equal (gethash 1 obj) "a"))))
  (should (let ((obj (bert-unpack
                      (bert-pack
                       (let ((hash-table (make-hash-table)))
                         (puthash 1 "a" hash-table)
                         (puthash 2 "b" hash-table)
                         hash-table)))))
            (and (hash-table-p obj)
                 (= (hash-table-count obj) 2)
                 (equal (gethash 1 obj) "a")
                 (equal (gethash 2 obj) "b")))))
