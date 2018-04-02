;;; libmpdel-test.el --- Tests for libmpdel.el        -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'cl-lib)

(require 'libmpdel)


;;; Global private variables

(ert-deftest libmpel-response-regexp ()
  (should (string-match-p libmpdel--response-regexp "OK\n"))
  (should (string-match-p libmpdel--response-regexp "OK MPD 0.20.0\n"))
  (should (string-match-p libmpdel--response-regexp "ACK [51@10] {} unknown command \"foobar\"\n"))
  (should (string-match-p libmpdel--response-regexp "Artist: A-ha\nOK\n")))


;;; Data structures

(ert-deftest libmpdel-artist ()
  (let* ((artist (libmpdel--artist-create :name "The Artist"))
         (album (libmpdel--album-create :name "The Album" :artist artist))
         (song (libmpdel--song-create :name "The song" :album album)))
    (should (equal artist (libmpdel-artist artist)))
    (should (equal artist (libmpdel-artist album)))
    (should (equal artist (libmpdel-artist song)))))

(ert-deftest libmpdel-artist-name ()
  (let* ((artist (libmpdel--artist-create :name "The Artist"))
         (album (libmpdel--album-create :name "The Album" :artist artist))
         (song (libmpdel--song-create :name "The song" :album album)))
    (should (equal "The Artist" (libmpdel-artist-name artist)))
    (should (equal "The Artist" (libmpdel-artist-name album)))
    (should (equal "The Artist" (libmpdel-artist-name song)))))

(ert-deftest libmpdel-album-name ()
  (let* ((artist (libmpdel--artist-create :name "The Artist"))
         (album (libmpdel--album-create :name "The Album" :artist artist))
         (song (libmpdel--song-create :name "The song" :album album)))
    (should-error (libmpdel-album-name artist))
    (should (equal "The Album" (libmpdel-album-name album)))
    (should (equal "The Album" (libmpdel-album-name song)))))

(ert-deftest libmpdel-album ()
  (let* ((artist (libmpdel--artist-create :name "The Artist"))
         (album (libmpdel--album-create :name "The Album" :artist artist))
         (song (libmpdel--song-create :name "The song" :album album)))
    (should-error (libmpdel-album artist))
    (should (equal album (libmpdel-album album)))
    (should (equal album (libmpdel-album song)))))

(ert-deftest libmpdel-entity-name ()
  (let* ((artist (libmpdel--artist-create :name "The Artist"))
         (album (libmpdel--album-create :name "The Album" :artist artist))
         (song (libmpdel--song-create :name "The song" :album album)))
    (should (equal "The Artist"(libmpdel-entity-name artist)))
    (should (equal "The Album" (libmpdel-entity-name album)))
    (should (equal "The song" (libmpdel-entity-name song)))))

;;; Helper functions

(ert-deftest libmpdel-extract-data ()
  (should (equal '()
                 (libmpdel--extract-data "OK\n")))
  (should (equal '((changed . "playlist"))
                 (libmpdel--extract-data "changed: playlist\nOK\n")))
  (should (equal '((changed . "player") (changed . "mixer"))
                 (libmpdel--extract-data "changed: player\nchanged: mixer\nOK\n")))
  (should (equal '((volume . "100") (repeat . "0"))
                 (libmpdel--extract-data "volume: 100\nrepeat: 0\nOK\n"))))

(ert-deftest libmpdel-string<-ignore-case ()
  (should (libmpdel--string<-ignore-case "a" "b"))
  (should (libmpdel--string<-ignore-case "a" "B"))
  (should (libmpdel--string<-ignore-case "A" "b"))
  (should-not (libmpdel--string<-ignore-case "b" "a"))
  (should-not (libmpdel--string<-ignore-case "B" "a"))
  (should-not (libmpdel--string<-ignore-case "b" "A")))

(ert-deftest libmpdel-time-to-string ()
  (should (string= (libmpdel-time-to-string nil) "0"))
  (should (string= (libmpdel-time-to-string "2.230") "00:02"))
  (should (string= (libmpdel-time-to-string "84.450") "01:24"))
  (should (string= (libmpdel-time-to-string "3623.23") "60:23")))

(ert-deftest libmpdel-getting-and-setting-play-state ()
  (libmpdel--set-play-state "play")
  (should (equal (libmpdel-play-state) 'play)))

(ert-deftest libmpdel-setting-play-state-run-hook ()
  (let* ((fn-executed 0)
         (fn (lambda () (cl-incf fn-executed))))
    (add-hook 'libmpdel-player-changed-hook fn)
    (setq libmpdel--play-state nil)
    (libmpdel--set-play-state "play")
    (libmpdel--set-play-state "pause")
    (should (equal fn-executed 2))))

(ert-deftest libmpdel-setting-play-state-run-hook-only-once ()
  (let* ((fn-executed 0)
         (fn (lambda () (cl-incf fn-executed))))
    (setq libmpdel-player-changed-hook nil)
    (add-hook 'libmpdel-player-changed-hook fn)
    (setq libmpdel--play-state nil)
    (libmpdel--set-play-state "play")
    (libmpdel--set-play-state "play")
    (should (equal fn-executed 1))))

(ert-deftest libmpdel-getting-and-setting-volume ()
  (libmpdel--set-volume "50")
  (should (string= (libmpdel-volume) "50")))


;;; Public functions

(ert-deftest libmpdel-entries ()
  (should (equal '("A" "B")
                 (libmpdel-entries '((Album . "A") (Album . "B")) 'Album)))
  (should (equal '("B" "A")
                 (libmpdel-entries '((Album . "B") (Album . "A")) 'Album)))
  (should (equal '("A" "B")
                 (libmpdel-entries '((Album . "A") (Foo . "Bar") (Album . "B")) 'Album)))
  (should (equal '("Bar")
                 (libmpdel-entries '((Album . "A") (Foo . "Bar") (Album . "B")) 'Foo))))

(ert-deftest libmpdel-sorted-entries ()
  (should (equal '("A" "B")
                 (libmpdel-sorted-entries '((Album . "A") (Album . "B")) 'Album)))
  (should (equal '("A" "B")
                 (libmpdel-sorted-entries '((Album . "B") (Album . "A")) 'Album)))
  (should (equal '("A" "B")
                 (libmpdel-sorted-entries '((Album . "A") (Foo . "Bar") (Album . "B")) 'Album)))
  (should (equal '("Bar")
                 (libmpdel-sorted-entries '((Album . "A") (Foo . "Bar") (Album . "B")) 'Foo))))

(ert-deftest libmpdel-group-data ()
  (should (equal '(((Album . "1 Album") (Title . "1 Title"))
                   ((Album . "2 Album") (Title . "2 Title")))
                 (libmpdel-group-data '((Album . "1 Album")
                                        (Title . "1 Title")
                                        (Album . "2 Album")
                                        (Title . "2 Title"))))))

(ert-deftest libmpdel-group-data-of-nil-is-nil ()
  (should (null (libmpdel-group-data nil))))

(ert-deftest libmpdel-equal ()
  (let* ((artist1 (libmpdel--artist-create :name "artist1"))
         (artist1-bis (libmpdel--artist-create :name "artist1"))
         (artist2 (libmpdel--artist-create :name "artist2"))
         (album1 (libmpdel--album-create :name "album1" :artist artist1))
         (album1-bis (libmpdel--album-create :name "album1" :artist artist1))
         (album2 (libmpdel--album-create :name "album2" :artist artist1))
         (song1 (libmpdel--song-create
                 :name "name"
                 :file "file"
                 :track "3"
                 :album album1
                 :id "1"
                 :pos "1"))
         (song1-bis (libmpdel--song-create
                     :name "name"
                     :file "file"
                     :track "3"
                     :album album1-bis
                     :id "2" ;; change id and pos
                     :pos "2"))
         (song2 (libmpdel--song-create
                 :name "name2"
                 :file "file2"
                 :track "3"
                 :album album2
                 :id "3"
                 :pos "3")))
    (should (libmpdel-equal artist1 artist1))
    (should (libmpdel-equal artist1 artist1-bis))
    (should (libmpdel-equal artist1-bis artist1))
    (should (not (libmpdel-equal artist1 artist2)))
    (should (not (libmpdel-equal artist2 artist1)))

    (should (libmpdel-equal album1 album1))
    (should (libmpdel-equal album1 album1-bis))
    (should (libmpdel-equal album1-bis album1))
    (should (not (libmpdel-equal album1 album2)))
    (should (not (libmpdel-equal album2 album1)))

    (should (libmpdel-equal song1 song1))
    (should (libmpdel-equal song1 song1-bis))
    (should (libmpdel-equal song1-bis song1))
    (should (not (libmpdel-equal song1 song2)))
    (should (not (libmpdel-equal song2 song1)))))


;;; Playlist queries

(provide 'libmpel-test)
;;; libmpdel-test.el ends here
