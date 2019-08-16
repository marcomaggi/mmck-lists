;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Lists
;;;Contents: version functions
;;;Date: Aug 16, 2019
;;;
;;;Abstract
;;;
;;;	This unit defines version functions.
;;;
;;;Copyright (C) 2019 Marco Maggi <mrc.mgg@gmail.com>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms of the GNU  Lesser General Public License as published  by the Free Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
;;;
;;;You should  have received a  copy of the GNU  Lesser General Public  License along
;;;with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


;;;; units and module header

(declare (unit mmck.lists.version)
	 (emit-import-library mmck.lists.version))

(module (mmck.lists.version)
    (mmck-lists-package-major-version
     mmck-lists-package-minor-version
     mmck-lists-package-patch-level
     mmck-lists-package-prerelease-tag
     mmck-lists-package-build-metadata
     mmck-lists-package-version
     mmck-lists-package-semantic-version)
  (import (scheme)
	  (prefix mmck.lists.config config::)
	  (only (chicken base)
		declare))


;;;; version functions

(declare (type (mmck-lists-package-major-version	(-> fixnum)))
	 (type (mmck-lists-package-minor-version	(-> fixnum)))
	 (type (mmck-lists-package-patch-level		(-> fixnum)))
	 (type (mmck-lists-package-prerelease-tag	(-> string)))
	 (type (mmck-lists-package-build-metadata	(-> string)))
	 (type (mmck-lists-package-version		(-> string)))
	 (type (mmck-lists-package-semantic-version	(-> string))))

(define (mmck-lists-package-major-version)		config::MMUX_PKG_MAJOR_VERSION)
(define (mmck-lists-package-minor-version)		config::MMUX_PKG_MINOR_VERSION)
(define (mmck-lists-package-patch-level)		config::MMUX_PKG_PATCH_LEVEL)
(define (mmck-lists-package-prerelease-tag)		config::MMUX_PKG_PRERELEASE_TAG)
(define (mmck-lists-package-build-metadata)		config::MMUX_PKG_BUILD_METADATA)
(define (mmck-lists-package-version)			config::MMUX_PKG_VERSION)
(define (mmck-lists-package-semantic-version)		config::MMUX_PKG_SEMANTIC_VERSION)


;;;; done

#| end of module |# )

;;; end of file
