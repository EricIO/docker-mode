;;; docker-mode.el --- Control docker from emacs

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

;; Docker mode is a mode for controlling docker from within Emacs.
;; I was looking for a magit like interface and decided that I should
;; create it myself since I didn't find anything I liked.

;; ## Basic Usage

;; Start with

;;     (require 'docker-mode)

;; ## docker images

;; Command: M-x docker-images RET

;; Show all images that docker knows about

;; ## docker ps

;; Command: M-x docker-ps RET

;; Show all running docker containers.


;;; Code:

(require 'docker-images)
(require 'docker-ps)

(provide 'docker-mode)

;;; docker-mode.el ends here
