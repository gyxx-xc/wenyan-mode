;;; wenyan.el --- Wenyan major mode

;; Copyright (C) 2001  Free Software Foundation, Inc.

;; Author: StefanMonnier
;; Keywords: extensions

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Wenyan major mode

;;; Code:

(defvar wenyan-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for `wenyan-mode'.")

(defvar wenyan-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\u3001 ".") ;; 、
    (modify-syntax-entry ?\u3002 ".") ;; 。
    (modify-syntax-entry ?\u300C "(w<\" 12") ;; 「
    (modify-syntax-entry ?\u300D ")w>\" 34") ;; 」
    (modify-syntax-entry ?\u300E "\"") ;; 『
    (modify-syntax-entry ?\u300F "\"") ;; 』
    st)
  "Syntax table for `wenyan-mode'.
It is almost not used since is designed for c-like language.")

(defvar wenyan-comment-start-list
  '("注曰" "疏曰")
  "List of comment start keywords.")

(defvar wenyan-doc-comment-start-list
  '("批曰")
  "List of doc comment start keyword.")

(defvar wenyan-string-warp-list
  '(("「「" "」」") ("『" "』"))
  "List of string warp keywords.")

(defvar wenyan-variable-name-list
  '("曰" "中之")
  "List of variable name keywords.")

(defvar wenyan-function-use-list
  '("施")
  "List of function call bagin keywords.")

(defvar wenyan-variable-warp-list
  '(("「" "」"))
  "List of variable warp keywords.")

(defvar wenyan-keyword-list
  '("不知何禍歟" "不復存矣" "如事不諧" "姑妄行此" "吾嘗觀" "之禍歟" "乃作罷" "名之曰" "書之" "以施" "之禍" "嗚呼" "之義" "昔之" "方悟" "是矣" "今有" "吾有" "之書" "物之" "夫" "中" "今" "取" "噫" "曰" "施" "豈" "有" "蓋謂" "或云" "乃行是術曰" "若其不然者" "乃止是遍" "乃歸空無" "欲行是術" "若其然者" "其物如是" "乃得矣" "恆為是" "之術也" "必先得" "是術曰" "之物也" "云云" "其餘" "中之" "為是" "之長" "乃止" "若非" "或若" "乃得" "是謂" "者" "若" "遍" "之" "充" "銜" "凡" "也")
  "List of keyword.")

(defvar wenyan-operator-list
  '("中有陽乎" "所餘幾何" "中無陰乎" "不等於" "不大於" "不小於" "等於" "大於" "小於" "加" "乘" "除" "變" "以" "於" "減")
  "List of operator.")

(defvar wenyan-const-list
  '("陰" "陽" "其")
  "List of const.")

(defvar wenyan-type-list
  '("元" "物" "爻" "術" "言" "列" "數")
  "List of type word.")

(defvar wenyan-number-list
  '("負" "·" "又" "零" "〇" "一" "二" "三" "四" "五" "六" "七" "八" "九" "十" "百" "千" "萬" "億" "兆" "京" "垓" "秭" "穰" "溝" "澗" "正" "載" "極" "分" "釐" "毫" "絲" "忽" "微" "纖" "沙" "塵" "埃" "渺" "漠")
  "List of number.")

(defvar wenyan-punctuation-list
  '("。" "、")
  "List of punctuation.")

(defun wenyan-warp-select-rx (list)
  "Return a regexp that matches things warp by LIST.
like string or variable."
  `(or ,@(mapcar (lambda (x)
                   `(and ,(nth 0 x) (*? anything) ,(nth 1 x)))
                 list)))

(defun wenyan-punctuation-or-blank-rx ()
  "Return a regexp that matches mutiple punctuation or blank."
  `(*? (or ,@wenyan-punctuation-list blank))
  )

(defvar wenyan-font-lock-keywords
  `(
    (,(rx-to-string
       `(and (or ,@wenyan-comment-start-list)
             ,(wenyan-punctuation-or-blank-rx)
             ,(wenyan-warp-select-rx wenyan-string-warp-list)))
     . 'font-lock-comment-face)

    (,(rx-to-string
       `(and (or ,@wenyan-doc-comment-start-list)
             ,(wenyan-punctuation-or-blank-rx)
             ,(wenyan-warp-select-rx wenyan-string-warp-list)))
     . 'font-lock-doc-face)

    (,(rx-to-string
       (wenyan-warp-select-rx wenyan-string-warp-list))
     . 'font-lock-string-face)

    (,(rx-to-string
       `(and (or ,@wenyan-variable-name-list)
             (submatch ,(wenyan-warp-select-rx wenyan-variable-warp-list))))
     1 'font-lock-variable-name-face)

    (,(rx-to-string
       `(and (or ,@wenyan-function-use-list)
             (submatch ,(wenyan-warp-select-rx wenyan-variable-warp-list))))
     1 'font-lock-function-use-face
     )

    (,(rx-to-string (wenyan-warp-select-rx wenyan-variable-warp-list))
     . 'font-lock-variable-use-face)

    (,(regexp-opt wenyan-keyword-list t)
     . 'font-lock-keyword-face)

    (,(regexp-opt wenyan-operator-list t)
     . 'font-lock-operator-face)

    (,(regexp-opt wenyan-number-list t)
     . 'font-lock-number-face)

    (,(regexp-opt wenyan-const-list t)
     . 'font-lock-constant-face)

    (,(regexp-opt wenyan-type-list t)
     . 'font-lock-type-face)

    (,(regexp-opt wenyan-punctuation-list t)
     . 'font-lock-punctuation-face)
    )
  "Keyword highlighting specification for `wenyan-mode'.")

;; (defvar wenyan-imenu-generic-expression
;;   ...)

;; (defvar wenyan-outline-regexp
;;   ...)

;;;###autoload
(define-derived-mode wenyan-mode prog-mode "Wenyan"
  "A major mode for editing Wenyan codes."
  :syntax-table wenyan-mode-syntax-table
  (setq-local comment-start (regexp-opt '("批曰" "注曰" "疏曰") t))
  (setq-local comment-end "」」")
  (setq-local font-lock-defaults
              '(wenyan-font-lock-keywords))
  (setq-local indent-line-function 'wenyan-indent-line)
  ;; (setq-local imenu-generic-expression
  ;;             wenyan-imenu-generic-expression)
  ;; (setq-local outline-regexp wenyan-outline-regexp)
  )

;;; Indentation

(defun wenyan-indent-line ()
  "Indent current line of Wenyan code."
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
        (indent (condition-case nil (max (wenyan-calculate-indentation) 0)
                  (error 0))))
    (if savep
        (save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun wenyan-backword-line-skip-blank ()
  "Move to the previous line and skip blank lines."
  (forward-line -1)
  (while (and (not (bobp))
              (looking-at-p "^[[:space:]]*$"))
    (forward-line -1))
  (not (bobp)))


;; 是術曰 +1
;; 是謂 IDENTIFIER 之術也 -1
;; 其物如是 +1
;; 是謂 IDENTIFIER 之物也 -1

;; 若 if_expression 者 +1
;; 若非 -1 +1

;; 恆為是 +1
;; 凡 xx 中之 xx +1
;; 為是 xx 遍 +1
;; 云云 | 也 -1
;; I think there are some better way to do it...
(defun wenyan-calculate-indentation-block-modifier (line)
  "Return amount by which LINE modifies the indentation."
  (save-excursion
    (let ((rtn 0))
      ;; in the perivous line now
      (if (re-search-forward
           (rx-to-string `(seq (or
                                "若非"
                                "是術曰"
                                "恆為是"
                                "其物如是"
                                (seq "若" (*? anything) "者")
                                (seq "凡" (*? anything) "中之" (*? anything))
                                (seq "為是" (*? anything) "遍"))
                               (*? ,(wenyan-punctuation-or-blank-rx))
                               line-end
                               ))
           (line-end-position) t)
          (setq rtn 1))
      ;; in the current line now
      (goto-char line)
      (print rtn)
      (if (looking-at-p
           (rx-to-string `(seq (*? blank)
                               (or
                                (seq "是謂" (*? anything) "之物也")
                                (seq "是謂" (*? anything) "之術也")
                                "若非"
                                "云云"
                                "也")
                               (*? anything)
                               )))
          (setq rtn (1- rtn)))
      (* rtn tab-width))
    ))

(defun wenyan-calculate-indentation ()
  "Return the column to which the current line should be indented."
  (save-excursion
    (let ((cur-line-begin-pos (line-beginning-position)))
      (or
       (when (wenyan-backword-line-skip-blank)
         (let* ((modifier
                 (wenyan-calculate-indentation-block-modifier cur-line-begin-pos)))
           (+ (current-indentation) modifier)))
       0))))


(provide 'wenyan)
;;; wenyan.el ends here
