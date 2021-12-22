;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; https://github.com/roelj/graphviz-guile/blob/master/graphviz.scm

(define-module (graphviz)
  #:export (;; New graphs
            graph
            digraph
            strictgraph
            strictdigraph
            readstring
            read

            ;; New nodes/edges
            node
            edge

            ;; Setting/getting attribute values
            setv
            getv

            ;; Finding and obtaining names
            nameof
            findsubg
            findnode
            findedge
            findattr

            ;; Graph navigators
            headof
            tailof
            graphof
            rootof

            ;; Obtain handles of proto node/edge for setting attribute values
            protonode
            protoedge

            ;; Iterators
            ok
            firstsubg
            nextsubg
            firstsupg
            nextsupg
            firstedge
            nextedge
            firstout
            nextout
            firsthead
            nexthead
            firstin
            nextin
            firstnode
            nextnode
            firstattr
            nextattr

            ;; Remove graph objects
            rm

            ;; Layout
            layout
            render
            renderresult
            renderchannel
            renderdata
            write))

;; (load-extension "libgv_guile.so" "SWIG_init")

(load-extension "/usr/lib/graphviz/guile/libgv_guile.so" "SWIG_init")
