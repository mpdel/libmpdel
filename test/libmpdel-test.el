;;; libmpdel-test.el --- Tests for libmpdel.el        -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Url: https://gitlab.petton.fr/mpdel/mpdel
;; Package-requires: ((emacs "25.1"))
;; Version: 1.1.2

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

;; Tests for libmpdel.el.

;;; Code:

(require 'ert)
(require 'ert-x)

(require 'cl-lib)

(require 'libmpdel)


;;; Global private variables

(ert-deftest libmpdel-test--response-regexp ()
  (should (string-match-p libmpdel--response-regexp "OK\n"))
  (should (string-match-p libmpdel--response-regexp "OK MPD 0.20.0\n"))
  (should (string-match-p libmpdel--response-regexp "ACK [51@10] {} unknown command \"foobar\"\n"))
  (should (string-match-p libmpdel--response-regexp "Artist: A-ha\nOK\n")))

(ert-deftest libmpdel-test-msgfield-regexp ()
  (save-match-data
    (let ((line "key: value\n"))
      (should (string-match libmpdel--msgfield-regexp line))
      (should (equal "key" (match-string 1 line)))
      (should (equal "value" (match-string 2 line))))))


;;; Data structures

(ert-deftest libmpdel-test-artist ()
  (let* ((artist (libmpdel--artist-create :name "The Artist"))
         (album (libmpdel--album-create :name "The Album" :artist artist))
         (song (libmpdel--song-create :name "The song" :album album)))
    (should (equal artist (libmpdel-artist artist)))
    (should (equal artist (libmpdel-artist album)))
    (should (equal artist (libmpdel-artist song)))))

(ert-deftest libmpdel-test-artist-name ()
  (let* ((artist (libmpdel--artist-create :name "The Artist"))
         (album (libmpdel--album-create :name "The Album" :artist artist))
         (song (libmpdel--song-create :name "The song" :album album)))
    (should (equal "The Artist" (libmpdel-artist-name artist)))
    (should (equal "The Artist" (libmpdel-artist-name album)))
    (should (equal "The Artist" (libmpdel-artist-name song)))))

(ert-deftest libmpdel-test-album-name ()
  (let* ((artist (libmpdel--artist-create :name "The Artist"))
         (album (libmpdel--album-create :name "The Album" :artist artist))
         (song (libmpdel--song-create :name "The song" :album album)))
    (should-error (libmpdel-album-name artist))
    (should (equal "The Album" (libmpdel-album-name album)))
    (should (equal "The Album" (libmpdel-album-name song)))))

(ert-deftest libmpdel-test-album ()
  (let* ((artist (libmpdel--artist-create :name "The Artist"))
         (album (libmpdel--album-create :name "The Album" :artist artist))
         (song (libmpdel--song-create :name "The song" :album album)))
    (should-error (libmpdel-album artist))
    (should (equal album (libmpdel-album album)))
    (should (equal album (libmpdel-album song)))))

(ert-deftest libmpdel-test-entity-name ()
  (let* ((artist (libmpdel--artist-create :name "The Artist"))
         (album (libmpdel--album-create :name "The Album" :artist artist))
         (song (libmpdel--song-create :name "The song" :album album))
         (stored-playlist (libmpdel--stored-playlist-create :name "The playlist")))
    (should (equal "The Artist" (libmpdel-entity-name artist)))
    (should (equal "The Album" (libmpdel-entity-name album)))
    (should (equal "The song" (libmpdel-entity-name song)))
    (should (equal "The playlist" (libmpdel-entity-name stored-playlist)))))

(ert-deftest libmpdel-test-entity-parent ()
  (let* ((artist (libmpdel--artist-create :name "The Artist"))
         (album (libmpdel--album-create :name "The Album" :artist artist))
         (song (libmpdel--song-create :name "The song" :album album))
         (stored-playlist (libmpdel--stored-playlist-create :name "The playlist")))
    (should (equal 'artists (libmpdel-entity-parent artist)))
    (should (equal artist (libmpdel-entity-parent album)))
    (should (equal album (libmpdel-entity-parent song)))
    (should (equal 'stored-playlists (libmpdel-entity-parent stored-playlist)))
    (should-not (libmpdel-entity-parent 'stored-playlists))
    (should-not (libmpdel-entity-parent 'artists))
    (should-not (libmpdel-entity-parent 'current-playlist))))

(ert-deftest libmpdel-test-create-song-from-data ()
  (let ((song (libmpdel--create-song-from-data
               '((Title . "The song")
                 (file . "foo/song.ogg")
                 (Album . "The Album")
                 (Artist . "The Artist")))))
    (should (equal "The song" (libmpdel-entity-name song)))
    (should (equal "foo/song.ogg" (libmpdel-song-file song)))
    (should (equal "The Album" (libmpdel-entity-name (libmpdel-album song))))
    (should (equal "The Artist" (libmpdel-entity-name (libmpdel-artist (libmpdel-album song)))))))

(ert-deftest libmpdel-test-current-playlist-p ()
  (should (libmpdel-current-playlist-p 'current-playlist))
  (should-not (libmpdel-current-playlist-p (libmpdel--stored-playlist-create :name "The Playlist")))
  (should-not (libmpdel-current-playlist-p (libmpdel--artist-create :name "The Artist"))))


;;; Helper functions

(defmacro libmpdel-test--with-connection (&rest body)
  "Execute BODY within a new fake MPD connection."
  `(let ((libmpdel--msghandlers (list))
         (commands (list))
         (log (list)))
     (cl-letf (((symbol-function 'libmpdel--log)
                (lambda (string type-string) (setq log (append log (list (cons string type-string))))))
               ((symbol-function 'libmpdel--raw-send-command)
                (lambda (command) (setq commands (append commands (list command)))))
               ((symbol-function 'libmpdel-connected-p)
                (lambda () t)))
       ,@body)))

(ert-deftest libmpdel-test--raw-send-command-with-handler-with-a-string ()
  (libmpdel-test--with-connection
   (libmpdel--raw-send-command-with-handler "command")
   (should (equal "command" (car commands)))))

(ert-deftest libmpdel-test--raw-send-command-with-handler-with-a-list ()
  (libmpdel-test--with-connection
   (libmpdel--raw-send-command-with-handler '("%s foo %S" "a" "b"))
   (should (equal "a foo \"b\"" (car commands)))))

(ert-deftest libmpdel-test--raw-send-command-with-handler-update-handlers ()
  (libmpdel-test--with-connection
   (ert-with-test-buffer ()
     (libmpdel--raw-send-command-with-handler "command" 'foo)
     (let ((handler (car libmpdel--msghandlers)))
       (should (equal (cl-first handler) "command"))
       (should (eql 'foo (cl-second handler)))
       (should (eql (current-buffer) (cl-third handler)))))))

(ert-deftest libmpdel-test--raw-send-command-with-handler-add-ignore-handler ()
  (libmpdel-test--with-connection
   (libmpdel--raw-send-command-with-handler "command")
   (let ((handler (car libmpdel--msghandlers)))
     (should (eql #'libmpdel--msghandler-ignore (cl-second handler))))))

(ert-deftest libmpdel-test--message-filter-activates-saved-buffer ()
  (let* ((expected-buffer nil)
         (actual-buffer nil)
         (handler (lambda (_) (setq actual-buffer (current-buffer)))))
    (libmpdel-test--with-connection
     (ert-with-test-buffer ()
       (setq expected-buffer (current-buffer))
       (libmpdel--raw-send-command-with-handler "foo" handler)
       (ert-with-test-buffer ()
         (libmpdel--message-filter nil "OK\n")
         (should (bufferp actual-buffer))
         (should (equal expected-buffer actual-buffer)))))))

(ert-deftest libmpdel-test--message-filter-keeps-current-buffer-if-saved-one-died ()
  (let* ((dead-buffer nil)
         (actual-buffer nil)
         (handler (lambda (_) (setq actual-buffer (current-buffer)))))
    (libmpdel-test--with-connection
     (ert-with-test-buffer ()
       (setq dead-buffer (current-buffer))
       (libmpdel--raw-send-command-with-handler "foo" handler))
     (ert-with-test-buffer ()
       (libmpdel--message-filter nil "OK\n")
       (should (bufferp actual-buffer))
       (should-not (equal dead-buffer actual-buffer))
       (should (equal (current-buffer) actual-buffer))))))

(ert-deftest libmpdel-test--msghandler-status-updates-volume ()
  (libmpdel-test--with-connection
   (libmpdel--msghandler-status '((volume . "42")))
   (should (equal (libmpdel-volume) "42"))))

(ert-deftest libmpdel-test-extract-data ()
  (should (equal '()
                 (libmpdel--extract-data "OK\n")))
  (should (equal '((changed . "playlist"))
                 (libmpdel--extract-data "changed: playlist\nOK\n")))
  (should (equal '((changed . "player") (changed . "mixer"))
                 (libmpdel--extract-data "changed: player\nchanged: mixer\nOK\n")))
  (should (equal '((volume . "100") (repeat . "0"))
                 (libmpdel--extract-data "volume: 100\nrepeat: 0\nOK\n"))))

(ert-deftest libmpdel-test-string<-ignore-case ()
  (should (libmpdel--string<-ignore-case "a" "b"))
  (should (libmpdel--string<-ignore-case "a" "B"))
  (should (libmpdel--string<-ignore-case "A" "b"))
  (should-not (libmpdel--string<-ignore-case "b" "a"))
  (should-not (libmpdel--string<-ignore-case "B" "a"))
  (should-not (libmpdel--string<-ignore-case "b" "A")))

(ert-deftest libmpdel-test-time-to-string ()
  (should (string= (libmpdel-time-to-string nil) "0"))
  (should (string= (libmpdel-time-to-string "2.230") "00:02"))
  (should (string= (libmpdel-time-to-string "84.450") "01:24"))
  (should (string= (libmpdel-time-to-string "3623.23") "60:23")))

(ert-deftest libmpdel-test-getting-and-setting-play-state ()
  (libmpdel--set-play-state "play")
  (should (equal (libmpdel-play-state) 'play)))

(ert-deftest libmpdel-test-setting-play-state-run-hook ()
  (let* ((fn-executed 0)
         (fn (lambda () (cl-incf fn-executed))))
    (add-hook 'libmpdel-player-changed-hook fn)
    (setq libmpdel--play-state nil)
    (libmpdel--set-play-state "play")
    (libmpdel--set-play-state "pause")
    (should (equal fn-executed 2))))

(ert-deftest libmpdel-test-setting-play-state-run-hook-only-once ()
  (let* ((fn-executed 0)
         (fn (lambda () (cl-incf fn-executed))))
    (setq libmpdel-player-changed-hook nil)
    (add-hook 'libmpdel-player-changed-hook fn)
    (setq libmpdel--play-state nil)
    (libmpdel--set-play-state "play")
    (libmpdel--set-play-state "play")
    (should (equal fn-executed 1))))

(ert-deftest libmpdel-test-getting-and-setting-volume ()
  (libmpdel--set-volume "50")
  (should (string= (libmpdel-volume) "50")))

(ert-deftest libmpdel-test-playlist-add-sends-addid ()
 (let ((gmusic-album
        (libmpdel--create-song-from-data
         '((Title . "Album: The Belly Of An Architect (Edicion 2007)")
           (file . "gmusic:album:Bbtjr2k5632pgyabagduxcg3p4q")
           (Album . "The Belly Of An Architect (Edicion 2007)")
           (AlbumArtist . "Wim Mertens")
           (Artist . "Wim Mertens")
           (Date . "1983")
           (X-AlbumUri "gmusic:album:Bbtjr2k5632pgyabagduxcg3p4q")))))
   (libmpdel-test--with-connection
    (libmpdel-playlist-add gmusic-album 'current-playlist)
    (should (equal '("addid \"gmusic:album:Bbtjr2k5632pgyabagduxcg3p4q\"")
                   (last commands))))))

(ert-deftest libmpdel-test-playlist-add-sends-findadd ()
 (let ((song (libmpdel--create-song-from-data
               '((Title . "S")
                 (Album . "A")
                 (Artist . "Art")))))
   (libmpdel-test--with-connection
    (libmpdel-playlist-add song 'current-playlist)
    (should (equal '("findadd artist \"Art\" album \"A\" title \"S\"")
                   (last commands))))))


;;; Public functions

(ert-deftest libmpdel-test-entries ()
  (should (equal '("A" "B")
                 (libmpdel-entries '((Album . "A") (Album . "B")) 'Album)))
  (should (equal '("B" "A")
                 (libmpdel-entries '((Album . "B") (Album . "A")) 'Album)))
  (should (equal '("A" "B")
                 (libmpdel-entries '((Album . "A") (Foo . "Bar") (Album . "B")) 'Album)))
  (should (equal '("Bar")
                 (libmpdel-entries '((Album . "A") (Foo . "Bar") (Album . "B")) 'Foo))))

(ert-deftest libmpdel-test-sorted-entries ()
  (should (equal '("A" "B")
                 (libmpdel-sorted-entries '((Album . "A") (Album . "B")) 'Album)))
  (should (equal '("A" "B")
                 (libmpdel-sorted-entries '((Album . "B") (Album . "A")) 'Album)))
  (should (equal '("A" "B")
                 (libmpdel-sorted-entries '((Album . "A") (Foo . "Bar") (Album . "B")) 'Album)))
  (should (equal '("Bar")
                 (libmpdel-sorted-entries '((Album . "A") (Foo . "Bar") (Album . "B")) 'Foo))))

(ert-deftest libmpdel-test-group-data ()
  (should (equal '(((Album . "1 Album") (Title . "1 Title"))
                   ((Album . "2 Album") (Title . "2 Title")))
                 (libmpdel-group-data '((Album . "1 Album")
                                        (Title . "1 Title")
                                        (Album . "2 Album")
                                        (Title . "2 Title"))))))

(ert-deftest libmpdel-test-group-data-of-nil-is-nil ()
  (should (null (libmpdel-group-data nil))))

(ert-deftest libmpdel-test-equal ()
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

(ert-deftest libmpdel-test-playlist-add-artist-to-stored-playlist ()
  (libmpdel-test--with-connection
   (let ((artist (libmpdel--artist-create :name "The Artist"))
         (playlist (libmpdel--stored-playlist-create :name "the playlist")))
     (libmpdel-playlist-add artist playlist)
     (should (equal (car (last commands))
                    "searchaddpl \"the playlist\" artist \"The Artist\"")))))

(provide 'libmpdel-test)
;;; libmpdel-test.el ends here

;; Local Variables:
;; nameless-current-name: "libmpdel-test"
;; End:
