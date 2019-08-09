;;; ark-minibuffer.el --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defun suppress-messages (old-fun &rest args)
  (cl-flet ((silence (&rest args1) (ignore)))
    (advice-add 'message :around #'silence)
    (unwind-protect
        (apply old-fun args)
      (advice-remove 'message #'silence))))

;; TODO: Legg til flere samt fjern duplisering i kode
;; TODO: Legg til for "mark saved..."
(advice-add 'end-of-buffer :around #'suppress-messages)
(advice-add 'beginning-of-buffer :around #'suppress-messages)
(advice-add 'ivy-posframe-enable :around #'suppress-messages)
(advice-add 'server-execute :around #'suppress-messages)
(advice-add 'save-buffer :around #'suppress-messages)

(defadvice previous-line (around silencer activate)
  (condition-case nil
      ad-do-it
    ((beginning-of-buffer))))

(defadvice next-line (around silencer activate)
  (condition-case nil
      ad-do-it
    ((end-of-buffer))))

(provide 'ark-minibuffer)

;;; ark-minibuffer ends here
