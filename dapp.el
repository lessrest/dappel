;;; dapp.el --- use Ethereum from Emacs

;; Copyright (C) 2018  Mikael Brockman <mikael@brockman.se>

;; Version: 0.5

;; Dappel is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.

;; Bongo is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with Dappel (see the file `COPYING'); if not,
;; write to the Free Software Foundation, 51 Franklin Street,
;; Fifth Floor, Boston, MA 02110-1301, USA.

;;; Code:

(defvar *erc20-tokens*
  '((ETH . nil)
    (MKR . "0x9f8F72aA9304c8B593d555F12eF6589cC3A579A2")
    (DAI . "0x89d24A6b4CcB1B6fAA2625fE562bDD9a23260359")
    (SAI . "0x59aDCF176ED2f6788A41B8eA4c4904518e62B6A4")))

(defvar *ethereum-rpc-url* "https://mainnet.infura.io")
(defvar *ethereum-accounts* nil)

(defun seth-setup ()
  (setenv "ETH_RPC_URL" *ethereum-rpc-url*))

(defun seth-accounts ()
  (or *ethereum-accounts*
      (mapcar #'split-string (split-string (seth "accounts") "\n" t))))

(defun eth-pick-account ()
  (interactive)
  (completing-read "Account address: " (seth-accounts)))

(defun seth (command &rest args)
  (seth-setup)
  (with-temp-buffer
    (apply #'call-process `("seth" nil t nil ,command ,@args))
    (string-trim (buffer-string))))

(defun seth-token-balance (address token)
  (if (eq token 'ETH)
      (seth "balance" address)
    (let ((token-address
           (cdr (assoc token *erc20-tokens*))))
      (seth "call" token-address "balanceOf(address)" address))))

(defun hex-to-dec (hex)
  (seth "--to-dec" hex))

(defun format-wei (wei)
  (if (string-prefix-p "0x" wei)
      (seth "--from-wei" (hex-to-dec wei))
    (seth "--from-wei" wei)))

(defun token-balance (address token)
  (interactive
   (list (eth-pick-account)
         (intern (completing-read "Token: " *erc20-tokens*))))
  (let ((wei (hex-to-dec (seth-token-balance address token))))
    (insert (format-wei wei))))

(defun token-ls ()
  (interactive)
  (dolist (x (seth-accounts))
    (let ((address (car x)))
      (insert address "   " (seth "nonce" address) "  " (cdr x))
      (newline)
      (dolist (token *erc20-tokens*)
        (let ((balance (format-wei (seth-token-balance address (car token)))))
          (when (not (string-equal balance "0"))
            (insert "  " (symbol-name (car token)) "    " balance)
            (newline)))))))
