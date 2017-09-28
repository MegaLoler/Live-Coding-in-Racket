#lang racket
(require rtmidi)

; get current millis
(define (now) (current-inexact-milliseconds))

; second unit constant = 1000 millis
(define *second* 1000)

; asynchonously call proc at absolute system time
(define (schedule time callback)
  (thread
    (lambda ()
      (sync (alarm-evt time))
      (callback time))))

; asynchronously call proc after some milliseconds pass
(define (schedule-after time callback)
  (schedule (+ (now) time) callback))
 
; init midi
(define out (make-rtmidi-out))
(define out-ports (rtmidi-ports out))
(match out-ports
  [(cons a-port other-ports) (rtmidi-open-port out 1)]
  [other (printf (current-error-port) "no output ports\n")])

; send a note on message to start playing a note with some velocity
(define (note-on note velocity)
  (rtmidi-send-message out (list 144 note velocity)))

; send a note off message to stop a note
(define (note-off note)
  (rtmidi-send-message out (list 128 note 0)))

; asynchronously schedule a note to play for a given duration
(define (play-note time note velocity duration)
  (schedule time (lambda (time) (note-on note velocity)))
  (schedule (+ time duration) (lambda (time) (note-off note))))

; play more than one note (chord)
(define (play-notes time notes velocity duration)
  (for-each (lambda (note) (play-note time note velocity duration))
	    notes))

; play an absolute note object ((note-name . accidental) . octave)
(define (play-absolute-note time note velocity duration)
  (play-note time (absolute-midi-note note) velocity duration))

; play a list of absolute notes (absolute chord)
(define (play-chord time notes velocity duration)
  (for-each (lambda (note) (play-absolute-note time note velocity duration))
	    notes))

; turn off all notes that may be playing
(define (panic)
  (for-each (lambda (note)
	      (note-off note))
	    (range 1 129)))

; return the diatonic id of a note name or pitch class
(define (diatonic-id note-name)
  (cond ((eqv? note-name 'c-flat) 0)
        ((eqv? note-name 'c) 0)
	((eqv? note-name 'c-sharp) 0)
	((eqv? note-name 'd-flat) 1)
	((eqv? note-name 'd) 1)
	((eqv? note-name 'd-sharp) 1)
	((eqv? note-name 'e-flat) 2)
	((eqv? note-name 'e) 2)
	((eqv? note-name 'e-sharp) 2)
	((eqv? note-name 'f-flat) 3)
	((eqv? note-name 'f) 3)
	((eqv? note-name 'f-sharp) 3)
	((eqv? note-name 'g-flat) 4)
	((eqv? note-name 'g) 4)
	((eqv? note-name 'g-sharp) 4)
	((eqv? note-name 'a-flat) 5)
	((eqv? note-name 'a) 5)
	((eqv? note-name 'a-sharp) 5)
	((eqv? note-name 'b-flat) 6)
	((eqv? note-name 'b) 6)
	((eqv? note-name 'b-sharp) 6)))

; return the note-name from a diatonic id value
(define (note-name-from-diatonic-id diatonic-id)
  (let ((id (modulo diatonic-id 7)))
    (cond ((= id 0) 'c)
          ((= id 1) 'd)
          ((= id 2) 'e)
          ((= id 3) 'f)
          ((= id 4) 'g)
          ((= id 5) 'a)
          ((= id 6) 'b))))

; return the chromatic id of a note name or pitch class
(define (chromatic-id note-name)
  (cond ((eqv? note-name 'c-flat) 11)
        ((eqv? note-name 'c) 0)
	((eqv? note-name 'c-sharp) 1)
	((eqv? note-name 'd-flat) 1)
	((eqv? note-name 'd) 2)
	((eqv? note-name 'd-sharp) 3)
	((eqv? note-name 'e-flat) 3)
	((eqv? note-name 'e) 4)
	((eqv? note-name 'e-sharp) 5)
	((eqv? note-name 'f-flat) 4)
	((eqv? note-name 'f) 5)
	((eqv? note-name 'f-sharp) 6)
	((eqv? note-name 'g-flat) 6)
	((eqv? note-name 'g) 7)
	((eqv? note-name 'g-sharp) 8)
	((eqv? note-name 'a-flat) 8)
	((eqv? note-name 'a) 9)
	((eqv? note-name 'a-sharp) 10)
	((eqv? note-name 'b-flat) 10)
	((eqv? note-name 'b) 11)
	((eqv? note-name 'b-sharp) 0)))

; return the pitch class from a chromatic id value
(define (pitch-class-from-chromatic-id chromatic-id)
  (let ((id (modulo chromatic-id 12)))
    (cond ((= id 0) 'c)
          ((= id 1) 'c-sharp)
          ((= id 2) 'd)
          ((= id 3) 'd-sharp)
          ((= id 4) 'e)
          ((= id 5) 'f)
          ((= id 6) 'f-sharp)
          ((= id 7) 'g)
          ((= id 8) 'g-sharp)
          ((= id 9) 'a)
          ((= id 10) 'a-sharp)
          ((= id 11) 'b))))

; return a note from a chromatic id
(define (note-from-chromatic-id chromatic-id)
  (let ((id (modulo chromatic-id 12)))
    (cond ((= id 0) (note 'c natural))
          ((= id 1) (note 'c sharp))
          ((= id 2) (note 'd natural))
          ((= id 3) (note 'd sharp))
          ((= id 4) (note 'e natural))
          ((= id 5) (note 'f natural))
          ((= id 6) (note 'f sharp))
          ((= id 7) (note 'g natural))
          ((= id 8) (note 'g sharp))
          ((= id 9) (note 'a natural))
          ((= id 10) (note 'a sharp))
          ((= id 11) (note 'b natural)))))

; an accidental is represented as a dotted pair (type, multiplier)
; return the type identifier of an accidental (natural, sharp, flat)
(define (accidental-type accidental) (car accidental))
; return the multiplier of an accidental (singly, doubly, triply, etc)
(define (accidental-multiplier accidental) (cdr accidental))
; predefined accidentals for convenience
(define natural '(natural . 0))
(define sharp '(sharp . 1))
(define flat '(flat . 1))
(define double-sharp '(sharp . 2))
(define double-flat '(flat . 2))

; return the chromatic distance of an accidental
(define (accidental-distance accidental)
  (let ((distance (cond ((eqv? (accidental-type accidental) 'natural) 0)
			((eqv? (accidental-type accidental) 'sharp) 1)
			((eqv? (accidental-type accidental) 'flat) -1))))
    (* distance (accidental-multiplier accidental))))

; retern an accidental from a chromatic distance
(define (accidental-from-distance offset)
  (cond ((positive? offset) (cons 'sharp offset))
	((negative? offset) (cons 'flat (- offset)))
	(else natural)))

; return pitch class given note pair of name and accidential
(define (pitch-class note)
  (pitch-class-from-chromatic-id (+ (chromatic-id (note-name note)) (accidental-distance (accidental-from-note note)))))

; form a note dotted pair (note-name, accidental)
(define (note note-name accidental) (cons note-name accidental))
; get the note-name
(define (note-name note) (car note))
; get the accidental
(define (accidental-from-note note) (cdr note))
; get the chromatic id of a note
(define (chromatic-id-from-note note) (chromatic-id (pitch-class note)))

; predefined notes
(define c-flat (note 'c flat))
(define c-natural (note 'c natural))
(define c-sharp (note 'c sharp))
(define c c-natural)
(define d-flat (note 'd flat))
(define d-natural (note 'd natural))
(define d-sharp (note 'd sharp))
(define d d-natural)
(define e-flat (note 'e flat))
(define e-natural (note 'e natural))
(define e-sharp (note 'e sharp))
(define e e-natural)
(define f-flat (note 'f flat))
(define f-natural (note 'f natural))
(define f-sharp (note 'f sharp))
(define f f-natural)
(define g-flat (note 'g flat))
(define g-natural (note 'g natural))
(define g-sharp (note 'g sharp))
(define g g-natural)
(define a-flat (note 'a flat))
(define a-natural (note 'a natural))
(define a-sharp (note 'a sharp))
(define a a-natural)
(define b-flat (note 'b flat))
(define b-natural (note 'b natural))
(define b-sharp (note 'b sharp))
(define b b-natural)

; return a midi note from a note name, accidental, and octave
(define (midi-note note-name accidental octave)
  (+ (chromatic-id note-name)
     (accidental-distance accidental)
     (* 12 (+ 1 octave))))

; return a midi note from a note (name accidental pair)
(define (quick-note note octave)
  (midi-note (note-name note) (accidental-from-note note) octave))

; return a midi note from an absolute note ((note-name . accidental) . octave))
(define (absolute-midi-note note)
  (midi-note (absolute-note-name note) (absolute-note-accidental note) (absolute-note-octave note)))

; return an absolute note from a midi note
(define (midi-note-to-absolute-note midi-note)
  (absolute-note (note-from-chromatic-id midi-note)
		 (- (quotient midi-note 12) 1)))

; return an interval (distance . quality)
(define (interval distance quality)
  (cons distance quality))
; get the distance of an interval
(define (interval-distance interval) (car interval))
; get the quality of an interval
(define (interval-quality interval) (cdr interval))
; get the diatonic offset of an interval's diatonic distance value
(define (interval-offset interval) (- (interval-distance interval) 1))
; is this interval a unison, fourth, fifth, or any derivatives thereof?
(define (perfect-style-interval? interval)
  (let ((offset (modulo (interval-offset interval) 7)))
    (memq (+ 1 offset) '(1 4 5))))
; get the chromatic distance of the quality of the interval
(define (interval-quality-distance interval)
  (let ((distance (quality-distance (interval-quality interval))))
    (if (and (not (perfect-style-interval? interval))
             (eqv? (quality-type (interval-quality interval)) 'diminished))
      (- distance 1)
      distance)))
; get the chromatic offset of an interval's diatonic distance value
(define (interval-chromatic-offset interval)
  (let ((diatonic-offset (interval-offset interval)))
    (+ (* 12 (quotient diatonic-offset 7))
       (chromatic-id (note-name-from-diatonic-id diatonic-offset))
       (interval-quality-distance interval))))

; an intervalic quality is represented as a dotted pair (type, multiplier)
; return the type identifier of an intervalic quality (major, minor, perfect, augmented, diminished)
(define (quality-type quality) (car quality))
; return the multiplier of an intervalic quality (for augmented and diminished)
(define (quality-multiplier quality) (cdr quality))
; predefined qualities for convenience
(define major '(major . 1))
(define minor '(minor . 1))
(define perfect '(perfect . 1))
(define augmented '(augmented . 1))
(define diminished '(diminished . 1))
(define double-augmented '(augmented . 2))
(define double-diminished '(diminished . 2))
(define triple-augmented '(augmented . 3))
(define triple-diminished '(diminished . 3))

; return the chromatic distance of an intervalic quality
; (type deviate from perfect and major)
(define (quality-distance quality)
  (let ((distance (cond ((eqv? (quality-type quality) 'major) 0)
			((eqv? (quality-type quality) 'minor) -1)
			((eqv? (quality-type quality) 'perfect) 0)
			((eqv? (quality-type quality) 'augmented) 1)
			((eqv? (quality-type quality) 'diminished) -1))))
    (* distance (quality-multiplier quality))))

; some predefined intervals
(define perfect-unison (interval 1 perfect))
(define diminished-unison (interval 1 diminished))
(define augmented-unison (interval 1 augmented))
(define unison perfect-unison)
(define diminshed-second (interval 2 diminished))
(define minor-second (interval 2 minor))
(define major-second (interval 2 major))
(define augmented-second (interval 2 augmented))
(define diminished-third (interval 3 diminished))
(define minor-third (interval 3 minor))
(define major-third (interval 3 major))
(define augmented-third (interval 3 augmented))
(define perfect-fourth (interval 4 perfect))
(define diminished-fourth (interval 4 diminished))
(define augmented-fourth (interval 4 augmented))
(define fourth perfect-fourth)
(define perfect-fifth (interval 5 perfect))
(define diminished-fifth (interval 5 diminished))
(define augmented-fifth (interval 5 augmented))
(define fifth perfect-fifth)
(define diminished-sixth (interval 6 diminished))
(define minor-sixth (interval 6 minor))
(define major-sixth (interval 6 major))
(define augmented-sixth (interval 6 augmented))
(define diminished-seventh (interval 7 diminished))
(define minor-seventh (interval 7 minor))
(define major-seventh (interval 7 major))
(define augmented-seventh (interval 7 augmented))
(define perfect-octave (interval 8 perfect))
(define diminished-octave (interval 8 diminished))
(define augmented-octave (interval 8 augmented))
(define octave perfect-octave)
(define minor-ninth (interval 9 minor))
(define major-ninth (interval 9 major))
(define minor-tenth (interval 10 minor))
(define major-tenth (interval 10 major))
(define perfect-eleventh (interval 11 perfect))
(define diminished-eleventh (interval 11 diminished))
(define augmented-eleventh (interval 11 augmented))
(define eleventh perfect-eleventh)
(define perfect-twelfth (interval 12 perfect))
(define diminished-twelfth (interval 12 diminished))
(define augmented-twelfth (interval 12 augmented))
(define twelfth perfect-twelfth)
(define minor-thirteenth (interval 13 minor))
(define major-thirteenth (interval 13 major))
(define minor-fourteenth (interval 14 minor))
(define major-fourteenth (interval 14 major))
(define perfect-fifteenth (interval 15 perfect))
(define diminished-fifteenth (interval 15 diminished))
(define augmented-fifteenth (interval 15 augmented))
(define fifteenth perfect-fifteenth)

; get the note-name an interval above another note-name
(define (interval-above-note-name note-name interval)
  (note-name-from-diatonic-id (+ (interval-offset interval) (diatonic-id note-name))))
; get the accidental an interval above a note
(define (interval-above-accidental note interval)
  (accidental-from-distance (- (- (+ (interval-chromatic-offset interval)
				      (chromatic-id-from-note note))
				  (* 12
				     (interval-above-octave-displacement note interval)))
			       (chromatic-id (interval-above-note-name
					       (note-name note) interval)))))
; get the note an interval above another note
(define (interval-above n interval)
  (note (interval-above-note-name (note-name n) interval)
	(interval-above-accidental n interval)))
; get the octave displacement of the note an interval above another note
(define (interval-above-octave-displacement note interval)
  (quotient (+ (interval-offset interval) (diatonic-id (note-name note))) 7))
; get the absolute note an interval above another absolute note
(define (interval-above-absolute note interval)
  (absolute-note (interval-above (absolute-note-note note) interval)
		 (+ (interval-above-octave-displacement
		      (absolute-note-note note) interval)
		    (absolute-note-octave note))))

; some predefined scale degrees
(define i 1)
(define ii 2)
(define iii 3)
(define iv 4)
(define v 5)
(define vi 6)
(define vii 7)
(define tonic i)
(define supertonic ii)
(define mediant iii)
(define subdominant iv)
(define dominant v)
(define submediant vi)
(define subtonic vii)

; scales are represented by lists of interval qualities of the intervals from root to each scale member
; some predefined scales
(define ionian (list perfect major major perfect perfect major major))
(define dorian (list perfect major minor perfect perfect major minor))
(define phrygian (list perfect minor major perfect perfect minor minor))
(define lydian (list perfect major major augmented perfect major major))
(define mixolydian (list perfect major major perfect perfect major minor))
(define aeolian (list perfect major minor perfect perfect minor minor))
(define locrian (list perfect minor major perfect diminished minor minor))
(define major-scale ionian)
(define natural-minor aeolian)
(define harmonic-minor (list perfect major minor perfect perfect minor major))
(define melodic-minor (list perfect major minor perfect perfect major major))
(define minor-scale natural-minor)

; get the interval between tonic and some scale member
(define (scale-interval degree scale)
  (interval degree (list-ref scale (modulo (- degree 1) 7))))

; get a key representation object
(define (key tonic scale)
  (cons tonic scale))
; get the tonic of a key
(define (key-tonic key)
  (car key))
; get the scale of a key
(define (key-scale key)
  (cdr key))

; some predefined keys
(define g-flat-major (key g-flat major-scale))
(define d-flat-major (key d-flat major-scale))
(define a-flat-major (key a-flat major-scale))
(define e-flat-major (key e-flat major-scale))
(define b-flat-major (key b-flat major-scale))
(define f-major (key f major-scale))
(define c-major (key c major-scale))
(define g-major (key g major-scale))
(define d-major (key d major-scale))
(define a-major (key a major-scale))
(define e-major (key e major-scale))
(define b-major (key b major-scale))
(define f-sharp-major (key f-sharp major-scale))
(define e-flat-minor (key e-flat minor-scale))
(define b-flat-minor (key b-flat minor-scale))
(define f-minor (key f minor-scale))
(define c-minor (key c minor-scale))
(define g-minor (key g minor-scale))
(define d-minor (key d minor-scale))
(define a-minor (key a minor-scale))
(define e-minor (key e minor-scale))
(define b-minor (key b minor-scale))
(define f-sharp-minor (key f-sharp minor-scale))
(define c-sharp-minor (key c-sharp minor-scale))
(define g-sharp-minor (key g-sharp minor-scale))
(define d-sharp-minor (key d-sharp minor-scale))

; get a member (note) from the scale of some key
(define (scale-member degree key)
  (interval-above (key-tonic key) (scale-interval degree (key-scale key))))

; get an absolute note from the scale of some key with tonic being in specified octave (everything else above that note)
(define (scale-member-absolute degree key octave)
  (interval-above-absolute (absolute-note (key-tonic key) octave) (scale-interval degree (key-scale key))))

; get an absolute note from the scale of some key (put the final note in specified octave directly)
(define (absolute-scale-member degree key octave)
  (absolute-note (scale-member degree key) octave))

; return a note plus octave
(define (absolute-note note octave)
  (cons note octave))
; get the note of an absolute note
(define (absolute-note-note note) (car note))
; get the note name of an absolute note
(define (absolute-note-name note) (note-name (absolute-note-note note)))
; get the accidental of an absolute note
(define (absolute-note-accidental note) (accidental-from-note (absolute-note-note note)))
; get the octave of an absolute note
(define (absolute-note-octave note) (cdr note))
; get an absolute note with an octave displacement from another absolute note
(define (octave-displacement note octave)
  (absolute-note (absolute-note-note note) (+ octave (absolute-note-octave note))))

; predefined absolute notes
(define c-flat-1 (absolute-note (note 'c flat) 1))
(define c-1 (absolute-note (note 'c natural) 1))
(define c-sharp-1 (absolute-note (note 'c natural) 1))
(define d-flat-1 (absolute-note (note 'd flat) 1))
(define d-1 (absolute-note (note 'd natural) 1))
(define d-sharp-1 (absolute-note (note 'd natural) 1))
(define e-flat-1 (absolute-note (note 'e flat) 1))
(define e-1 (absolute-note (note 'e natural) 1))
(define e-sharp-1 (absolute-note (note 'e natural) 1))
(define f-flat-1 (absolute-note (note 'f flat) 1))
(define f-1 (absolute-note (note 'f natural) 1))
(define f-sharp-1 (absolute-note (note 'f natural) 1))
(define g-flat-1 (absolute-note (note 'g flat) 1))
(define g-1 (absolute-note (note 'g natural) 1))
(define g-sharp-1 (absolute-note (note 'g natural) 1))
(define a-flat-1 (absolute-note (note 'a flat) 1))
(define a-1 (absolute-note (note 'a natural) 1))
(define a-sharp-1 (absolute-note (note 'a natural) 1))
(define b-flat-1 (absolute-note (note 'b flat) 1))
(define b-1 (absolute-note (note 'b natural) 1))
(define b-sharp-1 (absolute-note (note 'b natural) 1))

(define c-flat-2 (absolute-note (note 'c flat) 2))
(define c-2 (absolute-note (note 'c natural) 2))
(define c-sharp-2 (absolute-note (note 'c natural) 2))
(define d-flat-2 (absolute-note (note 'd flat) 2))
(define d-2 (absolute-note (note 'd natural) 2))
(define d-sharp-2 (absolute-note (note 'd natural) 2))
(define e-flat-2 (absolute-note (note 'e flat) 2))
(define e-2 (absolute-note (note 'e natural) 2))
(define e-sharp-2 (absolute-note (note 'e natural) 2))
(define f-flat-2 (absolute-note (note 'f flat) 2))
(define f-2 (absolute-note (note 'f natural) 2))
(define f-sharp-2 (absolute-note (note 'f natural) 2))
(define g-flat-2 (absolute-note (note 'g flat) 2))
(define g-2 (absolute-note (note 'g natural) 2))
(define g-sharp-2 (absolute-note (note 'g natural) 2))
(define a-flat-2 (absolute-note (note 'a flat) 2))
(define a-2 (absolute-note (note 'a natural) 2))
(define a-sharp-2 (absolute-note (note 'a natural) 2))
(define b-flat-2 (absolute-note (note 'b flat) 2))
(define b-2 (absolute-note (note 'b natural) 2))
(define b-sharp-2 (absolute-note (note 'b natural) 2))

(define c-flat-3 (absolute-note (note 'c flat) 3))
(define c-3 (absolute-note (note 'c natural) 3))
(define c-sharp-3 (absolute-note (note 'c natural) 3))
(define d-flat-3 (absolute-note (note 'd flat) 3))
(define d-3 (absolute-note (note 'd natural) 3))
(define d-sharp-3 (absolute-note (note 'd natural) 3))
(define e-flat-3 (absolute-note (note 'e flat) 3))
(define e-3 (absolute-note (note 'e natural) 3))
(define e-sharp-3 (absolute-note (note 'e natural) 3))
(define f-flat-3 (absolute-note (note 'f flat) 3))
(define f-3 (absolute-note (note 'f natural) 3))
(define f-sharp-3 (absolute-note (note 'f natural) 3))
(define g-flat-3 (absolute-note (note 'g flat) 3))
(define g-3 (absolute-note (note 'g natural) 3))
(define g-sharp-3 (absolute-note (note 'g natural) 3))
(define a-flat-3 (absolute-note (note 'a flat) 3))
(define a-3 (absolute-note (note 'a natural) 3))
(define a-sharp-3 (absolute-note (note 'a natural) 3))
(define b-flat-3 (absolute-note (note 'b flat) 3))
(define b-3 (absolute-note (note 'b natural) 3))
(define b-sharp-3 (absolute-note (note 'b natural) 3))

(define c-flat-4 (absolute-note (note 'c flat) 4))
(define c-4 (absolute-note (note 'c natural) 4))
(define c-sharp-4 (absolute-note (note 'c natural) 4))
(define d-flat-4 (absolute-note (note 'd flat) 4))
(define d-4 (absolute-note (note 'd natural) 4))
(define d-sharp-4 (absolute-note (note 'd natural) 4))
(define e-flat-4 (absolute-note (note 'e flat) 4))
(define e-4 (absolute-note (note 'e natural) 4))
(define e-sharp-4 (absolute-note (note 'e natural) 4))
(define f-flat-4 (absolute-note (note 'f flat) 4))
(define f-4 (absolute-note (note 'f natural) 4))
(define f-sharp-4 (absolute-note (note 'f natural) 4))
(define g-flat-4 (absolute-note (note 'g flat) 4))
(define g-4 (absolute-note (note 'g natural) 4))
(define g-sharp-4 (absolute-note (note 'g natural) 4))
(define a-flat-4 (absolute-note (note 'a flat) 4))
(define a-4 (absolute-note (note 'a natural) 4))
(define a-sharp-4 (absolute-note (note 'a natural) 4))
(define b-flat-4 (absolute-note (note 'b flat) 4))
(define b-4 (absolute-note (note 'b natural) 4))
(define b-sharp-4 (absolute-note (note 'b natural) 4))

(define c-flat-5 (absolute-note (note 'c flat) 5))
(define c-5 (absolute-note (note 'c natural) 5))
(define c-sharp-5 (absolute-note (note 'c natural) 5))
(define d-flat-5 (absolute-note (note 'd flat) 5))
(define d-5 (absolute-note (note 'd natural) 5))
(define d-sharp-5 (absolute-note (note 'd natural) 5))
(define e-flat-5 (absolute-note (note 'e flat) 5))
(define e-5 (absolute-note (note 'e natural) 5))
(define e-sharp-5 (absolute-note (note 'e natural) 5))
(define f-flat-5 (absolute-note (note 'f flat) 5))
(define f-5 (absolute-note (note 'f natural) 5))
(define f-sharp-5 (absolute-note (note 'f natural) 5))
(define g-flat-5 (absolute-note (note 'g flat) 5))
(define g-5 (absolute-note (note 'g natural) 5))
(define g-sharp-5 (absolute-note (note 'g natural) 5))
(define a-flat-5 (absolute-note (note 'a flat) 5))
(define a-5 (absolute-note (note 'a natural) 5))
(define a-sharp-5 (absolute-note (note 'a natural) 5))
(define b-flat-5 (absolute-note (note 'b flat) 5))
(define b-5 (absolute-note (note 'b natural) 5))
(define b-sharp-5 (absolute-note (note 'b natural) 5))

(define c-flat-6 (absolute-note (note 'c flat) 6))
(define c-6 (absolute-note (note 'c natural) 6))
(define c-sharp-6 (absolute-note (note 'c natural) 6))
(define d-flat-6 (absolute-note (note 'd flat) 6))
(define d-6 (absolute-note (note 'd natural) 6))
(define d-sharp-6 (absolute-note (note 'd natural) 6))
(define e-flat-6 (absolute-note (note 'e flat) 6))
(define e-6 (absolute-note (note 'e natural) 6))
(define e-sharp-6 (absolute-note (note 'e natural) 6))
(define f-flat-6 (absolute-note (note 'f flat) 6))
(define f-6 (absolute-note (note 'f natural) 6))
(define f-sharp-6 (absolute-note (note 'f natural) 6))
(define g-flat-6 (absolute-note (note 'g flat) 6))
(define g-6 (absolute-note (note 'g natural) 6))
(define g-sharp-6 (absolute-note (note 'g natural) 6))
(define a-flat-6 (absolute-note (note 'a flat) 6))
(define a-6 (absolute-note (note 'a natural) 6))
(define a-sharp-6 (absolute-note (note 'a natural) 6))
(define b-flat-6 (absolute-note (note 'b flat) 6))
(define b-6 (absolute-note (note 'b natural) 6))
(define b-sharp-6 (absolute-note (note 'b natural) 6))

(define middle-c c-4)

; make a list a given length (truncate longer lists and fill shorter lists)
(define (make-length len fill ls)
  (let ((delta (- len (length ls))))
    (cond ((positive? delta) (append ls (make-list delta fill)))
	  ((negative? delta) (drop-right ls (- delta)))
	  (else ls))))

; make chord (list of absolute notes) given absolute root note, intervals (notes above it), and inversions (octave displacements per chord member)
(define (chord root intervals inversions)
  (let ((inversions (make-length (+ (length intervals) 1) 0 inversions)))
    (cons (octave-displacement root (car inversions))
	  (map (lambda (interval inversion)
		 (interval-above-absolute (octave-displacement root inversion) interval))
	       intervals (cdr inversions)))))

; make a harmony (like a chord, except notes aren't absolute (ie, they dont have octaves))
(define (harmony root intervals)
  (cons root (map (lambda (interval)
		    (interval-above root interval))
		  intervals)))

; predefined inversions
(define root-position '(0 0 0))
(define first-inversion '(1 0 0))
(define second-inversion '(1 1 0))
(define third-inversion '(1 1 1))
(define root-position-open '(0 1 0))
(define first-inversion-open '(1 0 1))
(define second-inversion-open '(2 1 0))
(define inversion-5-3 '(0 0))
(define inversion-6-3 '(1 0))
(define inversion-6-4 '(1 1))
(define inversion-7-5-3 '(0 0 0))
(define inversion-6-5-3 '(1 0 0))
(define inversion-6-4-3 '(1 1 0))
(define inversion-6-4-2 '(1 1 1))

; prefedined harmonies (lists of intervals above root)
(define major-harmony (list major-third fifth))
(define minor-harmony (list minor-third fifth))
(define augmented-harmony (list major-third augmented-fifth))
(define diminished-harmony (list minor-third diminished-fifth))
(define half-diminished-harmony (list minor-third diminished-fifth minor-seventh))
(define fully-diminished-harmony (list minor-third diminished-fifth diminished-seventh))
(define dominant-seventh-harmony (list major-third fifth minor-seventh))
(define major-seventh-harmony (list major-third fifth major-seventh))
(define minor-seventh-harmony (list minor-third fifth minor-seventh))
(define dominant-ninth-harmony (list major-third fifth minor-seventh major-ninth))
(define dominant-minor-ninth-harmony (list major-third fifth minor-seventh minor-ninth))
(define major-ninth-harmony (list major-third fifth major-seventh major-ninth))
(define minor-ninth-harmony (list minor-third fifth minor-seventh major-ninth))

; make a naturaally occuring tertian chord on a scale degree in a key
(define (tertian-chord key degree octave note-count inversions)
  (map (lambda (degree-offset inversion)
	 (octave-displacement (scale-member-absolute (+ degree degree-offset) key octave) inversion))
       (range 0 (* 2 note-count) 2) (make-length note-count 0 inversions)))

; make a naturally occuring tertian harmony on a scalle degree in a key
(define (tertian-harmony key degree note-count)
  (map (lambda (degree-offset)
	 (scale-member (+ degree degree-offset) key))
       (range 0 (* 2 note-count) 2)))

; get a list of tertian harmonies
; give a key and a list of degree . note-count pairs
(define (harmonic-progression key degrees-and-note-counts)
  (map (lambda (degree note-count)
	 (tertian-harmony key degree note-count))
       (map car degrees-and-note-counts)
       (map cdr degrees-and-note-counts)))

; get a progression with the same note-count for all harmonies
(define (fixed-count-progression key note-count degrees)
  (map (lambda (degree)
	 (tertian-harmony key degree note-count))
       degrees))

; get a simple triad progression
(define (triad-progression key degrees) (fixed-count-progression key 3 degrees))

; get a simple i-iv-v-i progression in a key
(define (i-iv-v-i key) (triad-progression key (list i iv v i)))

; get an absolute note from a note closest to a reference absolute note
(define (realize-note-by-reference note reference)
  (let ((diatonic-delta (- (diatonic-id (note-name note)) (diatonic-id (absolute-note-name reference)))))
    (absolute-note note (cond ((> diatonic-delta 3) (- (absolute-note-octave reference) 1))
	                      ((< diatonic-delta -3) (+ (absolute-note-octave reference) 1))
	                      (else (absolute-note-octave reference))))))

; realize a harmony (get absolute notes based on the notes in the harmony)
; given harmony and a reference note matrix
; each entry is a harmonic member index (they can be used more than once) and a reference note
; entry looks like this: (0 . c-flat-5) means first note of the harmony realized closest to c-flat-5
(define (realize-harmony harmony members-and-references)
  (map (lambda (harmonic-member reference)
	 (realize-note-by-reference (list-ref harmony harmonic-member) reference))
       (map car members-and-references)
       (map cdr members-and-references)))

; realize all members of harmony once with a common reference note
(define (realize-harmony-simple harmony reference)
  (realize-harmony harmony
		   (map (lambda (i) (cons i reference))
			(range (length harmony)))))

; realize all members of a harmony once with a common reference note but a special reference note for a specified bass note (or any)
(define (realize-harmony-simple-bass harmony reference bass-member bass-reference)
  (realize-harmony harmony
		   (cons (cons bass-member bass-reference)
			 (map (lambda (i) (cons i reference))
			 (remove bass-member (range (length harmony)))))))

; same as above but also add in a doubled note with its own reference
(define (realize-harmony-simple-bass-double harmony reference bass-member bass-reference doubled-member doubled-reference)
  (realize-harmony harmony
		   (cons (cons doubled-member doubled-reference)
              		 (cons (cons bass-member bass-reference)
			       (map (lambda (i) (cons i reference))
		               (remove bass-member (range (length harmony))))))))

; get the absolute diatonic id of an absolute note
(define (absolute-diatonic-id note)
  (+ (* 7 (absolute-note-octave note)) (diatonic-id (absolute-note-name note))))

; get the diatonic distance of two absolute notes
(define (diatonic-distance a b)
  (abs (- (absolute-diatonic-id a) (absolute-diatonic-id b))))

; gets which note is diatonically closest to a reference (absolute)
; returns true if a is closer
(define (diatonic-closest a b reference) (< (diatonic-distance a reference) (diatonic-distance b reference)))

; get the memember of a harmony that is nearest to the reference note (returns absolute note)
; actually sorts them in order of nearest
(define (nearest-harmonic-member harmony reference)
  (sort (map (lambda (note)
               (realize-note-by-reference note reference))
             harmony)
	(lambda (a b) (diatonic-closest a b reference))))

; gets the prefered harmonic member for a voice
; discourages reusing notes
; encourages using notes closest to reference (previous note in voice)
(define (prefered-harmonic-member harmony reference counter-harmony)
  (let ((nearest-members (nearest-harmonic-member harmony reference)))
    (append (filter (lambda (item) (not (member (absolute-note-note item) (map absolute-note-note counter-harmony)))) nearest-members)
            (filter (lambda (item) (member (absolute-note-note item) (map absolute-note-note counter-harmony))) nearest-members))))

; get a series of absolute notes (like a melody) that follow a harmonic progression
; follows the smoothest possible line, starts with the harmonic member closest to given absolute reference note
; avoids notes already used by counter voices
(define (voice progression reference counters)
  (if (null? progression) '()
    (let ((note (first (prefered-harmonic-member (car progression) reference (map car counters)))))
      (cons note (voice (cdr progression) note (map cdr counters))))))

; get a simple chorale (list of voices following a chord progression)
; provide reference notes for the starting notes of however many voices you want
; provide pre-existing voices or none (empty list then)
; also voices get prioritized based on order!!
(define (chorale progression references counters)
  (if (null? references) '()
    (let ((voice (voice progression (car references) counters)))
      (cons voice (chorale progression (cdr references) (cons voice counters))))))

; a simple chorale with all the voices based around one reference note
(define (fast-chorale progression reference voice-count)
  (chorale progression (make-list voice-count reference) '()))

; change a list of series of notes to a series of list of notes
(define (voices-to-chords voices)
  (if (null? (car voices)) '()
    (cons (map car voices) (voices-to-chords (map cdr voices)))))

; init by stopping any lingering notes that might exist
(panic)

; play some stuff
(define (play time)
  (play-chord time (chord e-3 major-harmony root-position) 80 (* *second* 1/5))
  (schedule (+ *second* time) play))

; play accending chords starting on a midi note
(define (play-ascending-chords time note)
  (let ((c (chord (midi-note-to-absolute-note note) augmented-harmony root-position)))
    (play-chord time c 80 (* *second* 1/5))
    (schedule (+ (* 1/3 *second*) time) (lambda (time) (play-ascending-chords time (modulo (+ 1 note) 108))))))

; play some rising scale 7th chords yo
(define (play-rising-7ths time degree)
  (play-chord (now) (realize-harmony-simple-bass-double (tertian-harmony c-major degree 4) f-4 0 c-3 1 g-5) 80 (* *second* 1/5))
    (schedule (+ (* 1/2 *second*) time) (lambda (time) (play-rising-7ths time (+ 1 degree)))))

; play some chords in succession (list of lists of absolute notes)
(define (play-chords time chords velocity duration spacing)
  (if (null? chords) 0
    (begin
      (play-chord time (car chords) velocity duration)
      (schedule (+ spacing time) (lambda (time) (play-chords time (cdr chords) velocity duration spacing))))))

; play a chorale
(define (play-voices time voices velocity duration spacing)
  (play-chords time (voices-to-chords voices) velocity duration spacing))

; TODO:
; redo chorale, make it PRIORITY BASED
;    let a voice see ALL POSSIBLE NOTES
;      prioritize based on 1) unused notes first 2) stay within voices 3) closest to prior note 4) no parallel 5ths!
