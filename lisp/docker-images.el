;;; docker-images.el --- Implements the docker images command

;; Copyright (C) 2018 Eric Skoglund

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

;; FIXME: Should be defvar
(setq docker-images-mode-map
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map tabulated-list-mode-map)
        (define-key map "r" 'docker-images-run)
        map))

(defconst docker-images--list-format [("REPOSITORY" 15 t)
                                      ("TAG"        15 t)
                                      ("IMAGES ID"  15 t)
                                      ("CREATED"    15 t)
                                      ("SIZE"       15 t)])

(defun docker-images-run ()
  "Run the docker image at point"
  (interactive)
  (let ((image-id (aref (tabulated-list-get-entry) 2)))
    (process-lines "docker" "run" "-d" image-id)))

(defun docker-images--refresh ()
  "Run docker images again."
  (setq tabulated-list-entries nil)
  (dolist (line (cdr (process-lines "docker" "images")))
    (push (list nil (vconcat [] (split-string line "[[:blank:]]\\{2,\\}" t " ")))
          tabulated-list-entries))
  (tabulated-list-init-header))

(defun docker-images (&optional buffer)
  "Displays a list of all docker images.
Optinal argument BUFFER specifies a buffer to use instead
of \"*Docker Images*\"."
  (interactive)
  (unless (bufferp buffer)
    (setq buffer (get-buffer-create "*Docker Images*")))
  (with-current-buffer buffer
    (docker-images-mode)
    (docker-images--refresh)
    (tabulated-list-print))
  (switch-to-buffer buffer)
  nil)

(define-derived-mode docker-images-mode tabulated-list-mode "Docker Images"
  "Major mode for listing docker images."
  (kill-all-local-variables)
  (setq mode-name "Docker Images")
  (setq major-mode 'docker-images-mode)
  (use-local-map docker-images-mode-map)
  (setq tabulated-list-format docker-images--list-format)
  (setq tabulated-list-sort-key (cons "CREATED" nil))
  (add-hook 'tabulated-list-revert-hook 'docker-images--refresh nil t))

(provide 'docker-images)

;;; docker-images.el ends here
