;;; docker-ps.el --- Implements the docker ps command

;; Copyright (C) 2019 Eric Skoglund

;; Author: Eric Skoglund <eric@pagefault.se>
;; Version 0.1.0
;; Keywords: docker

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(defvar docker-ps-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "k" 'docker-ps-kill-container)
    map))

(define-derived-mode docker-ps-mode tabulated-list-mode "Docker ps"
  "Major mode for listing docker ps.
\\{docker-ps-mode-map}"
  (setq tabulated-list-format [("CONTAINER ID" 15 t)
                               ("IMAGE"        15 t)
                               ("COMMAND"      20 t)
                               ("CREATED"      15 t)
                               ("STATUS"       15 t)
                               ("PORTS"        20 t)
                               ("NAMES"        15 t)])
  (setq tabulated-list-sort-key (cons "STATUS" nil))
  (add-hook 'tabulated-list-revert-hook 'docker-ps--refresh nil t))

(defun docker-ps--refresh ()
  "Run docker ps again."
  (setq tabulated-list-entries nil)
  (dolist (line (cdr (process-lines "docker" "ps")))
    (push (list nil (vconcat [] (split-string line "[[:blank:]]\\{2,\\}" t " ")))
          tabulated-list-entries))
  (tabulated-list-init-header))

(defun docker-ps-kill-container ()
  "Kill the container at point."
  (interactive)
  (let ((container-id (aref (tabulated-list-get-entry) 0)))
    (process-lines "docker" "kill" container-id))
  (revert-buffer))

(defun docker-ps (&optional buffer)
  "Displays a list of all running docker containers.
Optinal argument BUFFER specifies a buffer to use instead
of \"*Docker ps*\"."
  (interactive)
  (unless (bufferp buffer)
    (setq buffer (get-buffer-create "*docker ps*")))
  (with-current-buffer buffer
    (docker-ps-mode)
    (docker-ps--refresh)
    (tabulated-list-print))
  (display-buffer buffer)
  nil)

(provide 'docker-ps)

;;; docker-ps.el ends here
