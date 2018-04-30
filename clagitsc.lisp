;;; ekzerco 3,5
(defun half (number)
  "Eligu la duonon de la enigo."
  (/ n 2.0))

(defun cube (number)
  "Eligu la tripotencon de la enigo."
  (* n n n))

(defun onemorep (number-1 number-2)
  "Kontrolu, se la unua enigo pligrandas ol la dua, nur per unu."
  (= number-1 (1+ number-2)))

;;; ekzerco 3,6
(defun pythag (x y)
  "Eligu la efekton de apliki la Pitagoran teoremon al la du enigoj."
  (sqrt (+ (expt x 2) (expt y 2))))

;;; ekzerco 3,7
(defun miles-per-gallon (initial-odometer-reading final-odometer-reading gallons-consumed)
  "Kalkulu la nombron de mejlo per gallon"
  (/ (- final-odometer-reading initial-odometer-reading) gallons-consumed))

;;; ekzerco 3,11
(defun longer-than (list-1 list-2)
  "Certigu se la unua enigo pligrandas ol la dua enigo"
  (> (length list-1) (length list-2)))

;;; ekzerco 3,12
(defun addlength (list)
  "Eligu la saman liston kiel la enigo, tamen, nun kun la longeco de la listo en la antaŭo."
  (cons (length list)
        list))

;;; ekzerco 3,22 c
(defun myfun (first second)
  "Eligu liston kiu havas la unuan enigon en lista formo dum la dua enigo ne ŝanĝitas."
  (list (list first) second))

;;; ekzerco 3,22 d
(defun firstp (symbol list)
  "Kontrolu se la valoro de la unua enigo ekvivalentas al la unua elemento de la dua enigo."
  (equal symbol (first list)))

;;; ekzerco 3,22 e
(defun mid-add1 (list)
  "Aldonu unu al la dua elemento de la eniga listo."
  (list (first list) (+ 1 (second list)) (third list)))

;;; ekzerco 3,22 f
(defun f-to-c (f-temperature)
  "Konvertu temperaturon el la farenhejta formo al la celsia formo."
  (/ (* 5 (- f-temperature 32)) 9.0))

;;; ekzerco 4,1
(defun make-even (number)
  "Eligu parnombron se la enigo malparas per aldoni unu al la enigo (aŭ nur eligu la enigon se la
  numero jam paras)."
  (if (evenp number)
      number
      (+ 1 number)))

;;; ekzerco 4,2
(defun further (number)
  "Aldonu unu aŭ subtrahu unu, se la eniga numero pozitivas aŭ ne, respektive."
  (cond ((> 0 number) (1+ number))
        ((< 0 number) (1- number))
        (t 0)))

;;; ekzerco 4,3
(defun my-not (constant)
  "Renversu la enigon per T aŭ NIL."
  (if constant
      nil
      t))

;;; ekzerco 4,4
(defun ordered (number-1 number-2)
  "Kreu liston el la enigoj kiu ordigitas ascende."
  (if (> number-1 number-2)
      (list number-2 number-1)
      (list number-1 number-2)))

;;; ekzerco 4,6
(defun my-abs (number)
  "Enigu la absolutan valoron de la enigo."
  (cond ((>= number 0) number)
        (t (- number))))

;;; ekzerco 4,8
(defun emphasize3 (x)
  "Emfazu la enigan frazon per ŝanĝi la unuan vorton de la enigo aŭ per aldoni la vorton ‘very’ al
  la komenco."
  (cond ((equal (first x) 'good) (cons 'great (rest x)))
        ((equal (first x) 'bad) (cons 'awful (rest x)))
        (t (cons 'very x))))

;;; ekzerco 4,9
(defun make-odd (x)
  "Kreu malparan nombron per aldoni unu al la enigo se la enigo paras."
  (if (not (oddp x))
      (+ x 1)))

;;; ekzerco 4,10
(defun constrain (x max min)
  "Eligu je min, se x plimalgrandas ol min; max, se x pligrandas ol max; aŭ x se la antaŭaj kondiĉoj
  ne sufiĉas."
  (if (< x min)
      min
      (if (> x max)
          max
          x)))

;;; ekzerco 4,11
(defun firstzero (list)
  "Eligu la pozicion de nulo en la eniga listo."
  (cond ((zerop (first list)) 'first)
        ((zerop (second list)) 'second)
        (t 'third)))

;;; ekzerco 4,12
(defun cycle (number)
  "Aldonu unu al la enigo, se plimalgrandas ol naŭdek naŭ."
  (if (< number 99)
      (1+ number)
      1))

;;; ekzerco 4,13
(defun howcompute (number-1 number-2 number-3)
  "Eligu simbolon de kiel kalkuli la trian numeron el la unua du enigoj."
  (cond ((= number-3 (+ number-1 number-2)) 'sum-of)
        ((= number-3 (* number-1 number-2)) 'product-of)
        (t '(beats me))))

;;; ekzerco 4,15
(defun geq (first-input second-input)
  "Eligu la simbolon T, se la unua enigo pligrandas aŭ ekvivalentas al la dua enigo."
  (if (>= first-input second-input)
      t))

;;; ekzerco 4,16
(defun odd-manipulate (number)
  "Eligu la kvadratan radikon de la enigo, se malparas kaj pozitivas; la doublon, se malparas kaj
  malpozitivas; aŭ la eligon kiam oni dividas la enigon per du."
  (cond ((and (oddp number) (> number 0)) (sqrt number))
        ((and (oddp number) (< number 0)) (* 2 number))
        (t (/ number 2))))

;;; ekzerco 4,17
(defun categoryp (input-1 input-2)
  "Kontrulu, se la unua enigo koheras al la dua enigo per ĉi tio: boy/girl al child aŭ man/woman al
  adult, respektive."
  (cond ((and (or (equal input-1 'boy)
                  (equal input-1 'girl))
              (equal input-2 'child)) t)
        ((and (or (equal input-1 'man)
                  (equal input-1 'woman))
              (equal input-2 'adult)) t)))

;;; ekzerco 4,18
(defun play (player-1 player-2)
  "Simulu la ludon de ‘Ŝtonego, Papero, kaj Tondilo’."
  (cond ((equal player-1 player-2) 'tie)
        ((and (equal player-1 'rock)
              (equal player-2 'paper)) 'second-wins)
        ((and (equal player-1 'rock)
              (equal player-2 'scissors)) 'first-wins)
        
        ((and (equal player-1 'paper)
              (equal player-2 'rock)) 'first-wins)
        ((and (equal player-1 'paper)
              (equal player-2 'scissors)) 'second-wins)
        
        ((and (equal player-1 'scissors)
              (equal player-2 'paper)) 'first-wins)
        ((and (equal player-1 'scissors)
              (equal player-2 'rock)) 'second-wins)))

;;; ekzerco 4,19
(defun and-cond (a b c d)
  "Simulu la efekton de uzi je ‘and’ per ‘cond’."
  (cond (a
         (cond (b
                (cond (c
                       (cond (d t)))))))))

(defun and-if (a b c d)
  "Simulu la efekton de uzi je ‘and’ per ‘if’."
  (if a
      (if b
          (if c
              (if d
                  t)))))

;;; ekzerco 4,20
(defun compare-if (x y)
  "Komparu la du enigojn, tiam eligu la verdikton, per uzi je ‘if’."
  (if (equal x y)
      'numbers-are-the-same
      (if (< x y)
          'first-is-smaller
          (if (> x y)
              'first-is-bigger))))

(defun compare-and-or (x y)
  "Komparu la du enigojn, tiam eligu la verdikton, per uzi je ‘and’ kaj ‘or’."
  (or (and (equal x y) 'numbers-are-the-same)
      (and (< x y) 'first-is-smaller)
      (and (> x y) 'first-is-bigger)))

;;; ekzerco 4,21
(defun gtest-if (x y)
  "Kontrolu, se la unua enigo pligrandas ol la dua, aŭ se io ajn de la enigoj estas nulo, per uzi je
  ‘if’."
  (if (> x y)
      t
      (if (zerop x)
          t
          (if (zerop y)
              t))))

(defun gtest-cond (x y)
  "Kontrolu, se la unua enigo pligrandas ol la dua, aŭ se io ajn de la enigoj estas nulo, per uzi je
  ‘cond’."
  (cond ((> x y) t)
        ((zerop x) t)
        ((zerop y) t)))

;;; ekzerco 4,22
(defun boilingp-cond (temp scale)
  "Kontrolu, se temp superas la bolgradon de akvo, per uzi je ‘cond’."
  (cond ((equal scale 'celsius) (cond ((>= temp 100) t)
                                      (t nil)))
        ((equal scale 'fahrenheit) (cond ((>= temp 212) t)
                                         (t nil)))))

(defun boilingp-if (temp scale)
  "Kontrolu, se temp superas la bolgradon de akvo, per uzi je ‘if’."
  (if (equal scale 'celsius)
      (>= temp 100)
      (if (equal scale 'fahrenheit)
          (>= temp 212))))

(defun boilingp-and/or (temp scale)
  "Kontrolu, se temp superas la bolgradon de akvo, per uzi je ‘and’ kaj/aŭ ‘or’."
  (or (and (equal scale 'celsius)
           (>= temp 100))
      (and (equal scale 'fahrenheit)
           (>= temp 212))))

;;; ekzerco 4,29
(defun logical-and-if (x y)
  "Simulu la operacion de la ‘logical and’ operaciilo, per uzi je ‘if’."
  (if x
      (if y
          t)))

(defun logical-and-cond (x y)
  "Simulu la operacion de la ‘logical and’ operaciilo, per uzi je ‘cond’."
  (cond (x (cond (y t)))))

;;; ekzerco 4,30
(defun logical-or (x y)
  "Simulu la operacion de la ‘logical or’ operaciilo, per uzi je ‘if’."
  (if x
      t
      (if y
          t)))

;;; ekzerco 5,1
(defun good-style (p)
  "Eligu liston kiu aldonas kvin al la enigo."
  (let ((q (+ 5 p)))
    (list 'result 'is q)))

;;; ekzerco 5,6 a
(defun throw-die ()
  "Eligu numeron inter unu kaj ses."
  (+ 1 (random 6)))

;;; ekzerco 5,6 b
(defun throw-dice ()
  "Eligu du numerojn inter unu kaj ses."
  (let ((throw-1 (+ 1 (random 6)))
        (throw-2 (+ 1 (random 6))))
    (list throw-1 throw-2)))

;;; ekzerco 5,6 c
(defun snake-eyes-p (x)
  "Kontrolu, se la enigo ekvivalentas al '(1 1)."
  (if (equal x '(1 1))
      t))

(defun boxcars-p (x)
  "Kontrolu, se la enigo ekvivalentas al '(6 6)."
  (if (equal x '(6 6))
      t))

;;; ekzerco 5,6 d
(defun instant-win-p (x)
  "Kontrolu, se la sumo de la ludkuborulo estas sep aŭ dek unu."
  (if (or (equal 7 (+ (car x)
                      (car (cdr x))))
          (equal 11 (+ (car x)
                       (car (cdr x)))))
      t))

(defun instant-lose-p (x)
  "Kontrolu, se la sumo de la ludkuborulo estas du, tri, aŭ dek du."
  (if (or (equal 2 (+ (car x)
                      (car (cdr x))))
          (equal 3 (+ (car x)
                      (car (cdr x))))
          (equal 12 (+ (car x)
                       (car (cdr x)))))
      t))

;;; ekzerco 5,6 e
(defun say-throw (x)
  "Eligu la sumon de la ludkuborulo, aŭ la simbolojn ‘snake-eyes’ aŭ ‘boxcars’, se la sumo estas du
  aŭ dek du, respektive."
  (let ((sum (+ (car x)
                (car (cdr x)))))
    (if (equal sum 2)
        'snake-eyes
        (if (equal sum 12)
            'boxcars
            sum))))

;;; ekzerco 5,6 f
(defun craps (x)
  "Simulu la rulumon de ludkuboj, tiam, eligu la respektivan respondon al tio."
  (let ((one (say-throw x)))
    (list 'throw
          (car x)
          'and
          (car (cdr x))
          '--
          (if (equal one 7)
              (list one '-- 'you 'win)
              (if (equal one 's-e)
                  '(snake-eyes -- you lose)
                  (list 'your 'point 'is one))))))

;;; ekzerco 5,6 g
(defun try-for-point (x)
  "Provu ĵeti daŭre la ludkubojn ĝis oni sukcesas aŭ malsukcesas per akiri sep."
  (let* ((throw-1 (+ 1 (random 6)))
         (throw-2 (+ 1 (random 6)))
         (sum (+ throw-1 throw-2)))
    (list 'throw
          throw-1
          'and
          throw-2
          '--
          (if (equal x sum)
              (list sum '-- 'you 'win)
              (list 'your 'point 'is sum)))))

;;; ekzerco 6,6
(defun last-element (x)
  "Eligu la lastan elementon de la eniga listo anstataŭ la lasta ‘cons cell’."
  (car (last x)))

(defun last-element-reverse (x)
  "Eligu la lastan elementon de la eniga listo per uzi je ‘reverse’."
  (car (reverse x)))

(defun last-element-nth/length (x)
  "Eligu la lastan elementon de la eniga listo per uzi je ‘nth’ kaj ‘length’."
  (nth (- (length x) 1) x))

;;; ekzerco 6,7
(defun next-to-last (x)
  "Eligu la duan element el la fino de la listo per uzi je ‘reverse’."
  (car (cdr (reverse x))))

(defun next-to-last-nth (x)
  "Eligu la duan element el la fino de la listo per uzi je ‘nth’."
  (nth (- (length x) 2) x))

;;; ekzerco 6,8
(defun my-butlast (x)
  "Eligu la liston sen la lasta elemento."
  (reverse (nthcdr 1 (reverse x))))

;;; ekzerco 6,10
(defun palindromep (x)
  "Kontrolu, se la enigo estas palindromo."
  (let ((r (reverse x)))
    (if (equal x r)
        t)))

;;; ekzerco 6,11
(defun make-palindrom (x)
  "Kreu palindromon el la eniga listo."
  (append x (reverse x)))

;;; ekzerco 6,15
(defun contains-article-p-intersection (x)
  "Kontrolu, se ekzistas la frazaj artikoloj en la eniga listo, per uzi je ‘intersection’."
  (if (or (intersection '(the) x)
          (intersection '(a) x)
          (intersection '(an) x))
      t))

(defun contains-article-p-member/or (x)
  "Kontrolu, se ekzistas la frazaj artikoloj en la eniga listo, per uzi je ‘member’ kaj ‘or’."
  (if (or (member 'a x)
          (member 'an x)
          (member 'the x))
      t))

;;; ekzerco 6,18
(defun add-vowels (x)
  "Aldonu la vokalojn al la eniga listo."
  (union '(a e i o u) x))

;;; ekzerco 6,21
(defun my-subsetp (x y)
  "Kontrolu, se la unua enigo estas subaro de la dua enigo."
  (if (equal nil (set-difference x y))
      t))

;;; ekzerco 6,24
(defun set-equal (x y)
  "Kontrolu, se la du enigaj listoj, ekvivalentas kiel aroj."
  (if (and (subsetp x y) (subsetp y x) (equal (length x) (length y)))
      t))

;;; ekzerco 6,25
(defun proper-subsetp (x y)
  "Kontrolu, se la unua enigo estas ĝusta subaro de sia dua enigo."
  (if (and (subsetp x y) (< (length x) (length y)))
      t))

;;; ekzerco 6,26 a
(defun right-side (x)
  "Eligu ĉion da eblo al la dekstra de la -vs- simbolo (de la eniga listo)."
  (set-difference (member '-vs- x) '(-vs-)))

;;; ekzerco 6,26 b
(defun left-side (x)
  "Eligu ĉion da eblo al la maldekstra de la -vs- simbolo (de la eniga listo)."
  (set-difference (member '-vs- (reverse x)) '(-vs-)))

;;; ekzerco 6,26 c
(defun count-common (x)
  "Eligu la nombrojn de la similaj ebloj de la maldekstra kaj dekstra flanko (de la eniga listo)."
  (length (intersection (right-side x) (left-side x))))

;;; ekzerco 6,26 d
(defun compare (x)
  "Eligu la nombrojn de la similaj ebloj de la kontraŭa enhavo de la eniga listo."
  (cons (count-common x) '(common features)))

;;; ekzerco 6,31
(setf books
      '((war-and-peace tolstoy)))

(defun who-wrote (book)
  "Eligu la aŭtoron de la eniga nomo de libro."
  (second (assoc book books)))

;;; ekzerco 6,35 a
(setf nerd-states
      '((sleeping eating)
        (eating waiting-for-a-computer)
        (waiting-for-a-computer programming)
        (programming debugging)
        (debugging sleeping)))

;;; ekzerco 6,35 b
(defun nerdus (state)
  "Determinu la sekvan staton depende al la eniga stato."
  (second (assoc state nerd-states)))

;;; ekzerco 6,35 d
(defun sleepless-nerd (state)
  "Determinu la sekvan staton simile al ‘nerdus’ krom la ‘sleeping’ stato."
  (let ((p-state (nerdus state)))
    (if (equal 'sleeping p-state)
        'eating
        p-state)))

;;; ekzerco 6,35 e
(defun nerd-on-caffeine (state)
  "Determinu la sekvan staton simile al ‘nerdus’, tamen, kun akcelitan paŝon."
  (nerdus (nerdus state)))

;;; ekzerco 6,36
(defun swap-first-last (x)
  "Interŝanĝu la unuan kaj la lastan elementon de la eniga listo."
  (let* ((unu (remove (first x) x))
         (du (remove (car (last unu)) unu)))
    (append (append (list (car (last x))) du) (list (first x)))))

;;; ekzerco 6,37
(defun rotate-left (x)
  "Turnu la enigan liston maldekstre."
  (append (rest x) (list (first x))))

(defun rotate-right (x)
  "Turnu la enigan liston dekstre."
  (let ((unu (remove (car (last x)) x)))
    (append (last x) unu)))

;;; ekzerco 6,41 a
(setf rooms
      '((living-room
         (north front-stairs)
         (south dining-room)
         (east kitchen))

        (upstairs-bedroom
         (west library)
         (south front-stairs))

        (dining-room
         (north living-room)
         (east pantry)
         (west downstairs-bedroom))

        (kitchen
         (west living-room)
         (south pantry))

        (pantry
         (north kitchen)
         (west dining-room))

        (downstairs-bedroom
         (north back-stairs)
         (east dining-room))

        (back-stairs
         (south downstairs-bedroom)
         (north library))

        (front-stairs
         (north upstairs-bedroom)
         (south living-room))

        (library
         (east upstairs-bedroom)
         (south back-stairs))))

(defun choices (room)
  "Eligu la validajn ĉambrojn ke oni povas navigi per la eniga ĉambro."
  (rest (assoc room rooms)))

;;; ekzerco 6,41 b
(defun look (direction room)
  "Eligu la ĉambron ke oni eniros, kiam oni iras al ‘direction’."
  (let ((choices (choices room)))
    (car (cdr (assoc direction choices)))))

;;; ekzerco 6,41 d
(defun how-many-choices ()
  "Eligu la nombron de ĉambraj elektoj por eniri (per la nuna valoro de ‘loc’)."
  (length (choices loc)))

;;; ekzerco 6,41 e
(defun upstairsp (room)
  "Kontrolu, se la enigo estas supra ĉambro."
  (or (equal room 'upstairs-bedroom)
      (equal room 'library)))

(defun onstairsp (room)
  "Kontrolu, se la enigo estas front-stairs aŭ back-stairs."
  (or (equal room 'front-stairs)
      (equal room 'back-stairs)))

;;; ekzerco 6,41 f
(defun where ()
  "Eligu la nunan lokon de Robbie."
  (list 'robbie 'is (if (upstairsp loc)
                        '(upstairs in)
                        (if (onstairsp loc)
                            'on
                            '(downstairs in))) 'the loc))

;;; ekzerco 6,41 g
(defun set-robbie-location (place)
  "Movu je Robbie al place per agordi la variablon loc."
  (setf loc place))

(defun move (direction)
  "Movu je Robbie al la eniga direkto."
  (let* ((place (look direction loc))
         (loc (set-robbie-location place)))
    (if place
        (where)
        '(ouch! robbie hit a wall))))

;;; ekzerco 6,42
(defun royal-we (list)
  "Interŝanĝu la simbolon ‘i’ al ‘we’ en la eniga listo."
  (subst 'we 'i list))

;;; ekzerco 7,7
(defun flip (list)
  "Renversu la elementojn de la eniga listo per la simboloj ‘up’ kaj ‘down’."
  (mapcar #'(lambda (d) (if (equal d 'up)
                            'down
                            'up)) list))

;;; ekzerco 7,8
(defun roughly-equal (x k)
  "Eligu la unuan numeron de la unua eniga listo, kiu aproksime ekvivalentas(+/- 10) al la dua
  enigo."
  (find-if #'(lambda (n) (and (not (< n (- k 10)))
                              (not (> n (+ k 10)))))
           x))

;;; ekzerco 7,9
(defun find-nested (list)
  "Trovu la unuan elementon de la eniga listo kiu estas nenula listo."
  (find-if #'(lambda (e) (if (listp e)
                             (if (not (equal nil e))
                                 e))) list))

;;; ekzerco 7,10 b
(setf note-table
      '((c 1)
        (c-sharp 2)
        (d 3)
        (d-sharp 4)
        (e 5)
        (f 6)
        (f-sharp 7)
        (g 8)
        (g-sharp 9)
        (a 10)
        (a-sharp 11)
        (b 12)))

(defun numbers (list)
  "Transmetu la elementojn de la eniga listo al la respektivaj listo de numeroj."
  (mapcar #'(lambda (n) (car (cdr (assoc n note-table)))) list))

;;; ekzerco 7,10 c
(defun notes (list)
  "Transmetu la elementojn de la eniga listo al la respektivaj listo de tonoj."
  (let ((table-note (mapcar #'(lambda (n) (reverse n)) note-table)))
    (mapcar #'(lambda (n)
                (car (cdr (assoc n table-note)))) list)))

;;; ekzerco 7,10 e
(defun raise (n list)
  "Kresku la valoron de la elementoj de la dua enigo per la unua enigo."
  (mapcar #'(lambda (a) (+ a n)) list))

;;; ekzerco 7,10 f, kun riparo pri la '-1' demando de ekzerco 7,10 g
(defun normalize (list)
  "Normaligu la elementojn de la eniga listo per certiĝi, ke la valoroj estas inter unu kaj dek du."
  (mapcar #'(lambda (n)
              (if (> n 12)
                  (- n 12)
                  (if (< n 1)
                      (+ n 12)
                      n))) list))

;;; ekzerco 7,10 g
(defun transpose (n song)
  "Eligu la transponitan liston de la eniga kanto per la valoro de la unua enigo."
  (let* ((numbered (numbers song))
         (raised (raise n numbered))
         (normalized (normalize raised))
         (noted (notes normalized)))
    noted))

;;; ekzerco 7,11
(defun greater-1-less-5 (list)
  "Eligu la enigan liston krom la elementoj kiu pligrandas ol kvin kaj plimalgrandas ol unu."
  (remove-if-not #'(lambda (n) (and (> n 1) (< n 5))) list))

;;; ekzerco 7,12
(defun the-counter (word-list)
  "Nombru la nombron de la vorto ‘the’ en la eniga listo."
  (length (remove-if-not #'(lambda (n) (equal n 'the)) word-list)))

;;; ekzerco 7,13
(defun length-two (list-of-list)
  "Elektu la liston kiu ekzakte havas du elementojn."
  (remove-if-not #'(lambda (n) (equal 2 (length n))) list-of-list))

;;; ekzerco 7,15 a
(defun rank (card)
  "Eligu la rangon de la eniga karto."
  (car card))

(defun suit (card)
  "Eligu la emblemon de la eniga karto."
  (cadr card))

;;; ekzerco 7,15 b
(setf my-hand
      '((3 hearts)
        (5 clubs)
        (2 diamonds)
        (4 diamonds)
        (ace spades)))

(defun count-suit (suit hand-of-cards)
  "Eligu la respektivan numeron de la eniga emblemo en la dua enigo."
  (length (remove-if-not #'(lambda (n) (equal (suit n) suit)) hand-of-cards)))

;;; ekzerco 7,15 c
(setf colors
      '((clubs black)
        (diamonds red)
        (hearts red)
        (spades black)))

(defun color-of (card)
  "Eligu la respektivan koloron de la eniga karto."
  (cadr (assoc (suit card) colors)))

;;; ekzerco 7,15 d
(defun first-red (hand)
  "Eligu la unuan karton de la enigo kiu havas ruĝan emblemon, aŭ nil, se ne ekzistas."
  (if (equal nil (remove-if-not #'(lambda (n) (equal 'red (color-of n))) hand))
      nil
      (car (remove-if-not #'(lambda (n) (equal 'red (color-of n))) hand))))

;;; ekzerco 7,15 e
(defun black-cards (hand)
  "Eligu liston kiu nur havas la nigrajn kartojn el la enigo."
  (remove-if-not #'(lambda (n) (equal 'black (color-of n))) hand))

;;; ekzerco 7,15 f
(defun what-ranks (suit hand)
  "Eligu la rangon de ĉio da kartoj kiu apartenas al la eniga emblemo."
  (mapcar #'(lambda (n) (rank n)) (remove-if-not #'(lambda (n) (equal (suit n) suit)) hand)))

;;; ekzerco 7,15 g
(setf all-ranks
      '(2 3 4 5 6 7 8 9 10 jack queen king ace))

(defun beforep (x y l)
  "Eligu la simbolon t, se x aperas antaŭ y en l."
  (member y (member x l)))

(defun higher-rank-p (card-1 card-2)
  "Kontrolu, se la unua eniga karto plialtnivelas ol la dua eniga karto."
  (if (beforep (rank card-1) (rank card-2) all-ranks)
      nil
      t))

;;; ekzerco 7,15 h
(defun high-card (list)
  "Trovu karton kiu havas la plej altnivelan rangon en la eniga listo."
  (first (first (sort list #'higher-rank-p))))

;;; ekzerco 7,17
(defun total-lists (list-of-list)
  "Eligu la tutan longecon de la eniga listo."
  (length (reduce #'append list-of-list)))

;;; ekzerco 7,19
(defun all-odd (list)
  "Eligu je T, se ĉio da elemento de la eniga listo malparas."
  (every #'oddp list))

;;; ekzerco 7,20
(defun none-odd (list)
  "Eligu je T, se ĉio da elemento de la eniga listo paras."
  (every #'evenp list))

;;; ekzerco 7,21
(defun not-all-odd (list)
  "Eligu je T, se ekzistas para numero."
  (not (every #'oddp list)))

;;; ekzerco 7,22
(defun not-none-odd (list)
  "Eligu je T, se ĉio da elemento de la eniga listo malparas."
  (not (every #'evenp list)))

;;; ekzerco 7,26
(defun my-find-if (x y)
  "Simulu la efekton de la ‘find-if’ funkcio per uzi je ‘remove-if-not’."
  (car (remove-if-not x y)))

;;; ekzerco 7,27
(defun my-every (x y)
  "Simulu la efekton de la ‘every’ funkcio per uzi je ‘remove-if’."
  (if (equal (length y) (length (remove-if x y)))
      t))

;;; ekzerco 7,29 a
(defun match-element (symbol-1 symbol-2)
  "Eligu je t, se la du enigoj ekvivalentas, aŭ se la dua enigo ekvivalentas al la simbolo ‘?’."
  (if (or (equal symbol-1 symbol-2) (equal '? symbol-2))
      t))

;;; ekzerco 7,29 b
(defun all-t (list)
  "Eligu je t, se ĉio da elemento de la eniga listo ekvivalentas al t."
  (every #'(lambda (n) (equal n 't)) list))

(defun match-triple (assertion pattern)
  "Eligu je t, se assertion akordas al pattern (per la pozicio de la ‘?’ simbolo)."
  (all-t (mapcar #'(lambda (a b) (match-element a b)) assertion pattern)))

;;; ekzerco 7,29 c
(setf database
      (b2 shape brick)
      (b2 color red)
      (b2 size small)
      (b2 supports b1)
      (b2 left-of b3))

(defun fetch (pattern)
  "Eligu ĉio da ‘assertion’ (el ‘database’)kiu akordas je ‘pattern’."
  (remove-if-not #'(lambda (n) (match-triple n pattern)) database))

;;; ekzerco 7,29 e
(defun block-color (block-name)
  "Eligu je ‘pattern’ el la ‘block-name’ enigo."
  (append (list block-name) (list 'color '?)))

;;; ekzerco 7,29 f
(defun supporters (block-name)
  "Eligu listojn de ‘blocks’ kiu povas subteni je ‘block-name’."
  (mapcar #'first (fetch (list '? 'supports block-name))))

;;; ekzerco 7,29 g
(defun supp-cube (block-name)
  "Eligu je t, se la enigo subtenitas per kubo."
  (if (equal 'cube (car (last (car (fetch (list (car (supporters block-name)) 'shape '? ))))))
      t))

;;; ekzerco 7,29 h
(defun desc1 (block-name)
  "Eligu ĉion da ‘assertion’ de la donita ‘block-name’."
  (fetch (list block-name '? '? )))

;;; ekzerco 7,29 i
(defun desc2 (block-name)
  "Forprenu je ‘block-name’ el la rezulto de voki je ‘desc1’."
  (mapcar #'(lambda (n) (remove block-name n)) (desc1 block-name)))

;;; ekzerco 7,29 j
(defun description (block-name)
  "Eligu la priskribon de la donita ‘block-name’."
  (reduce #'append (desc2 block-name)))

;;; ekzerco 8,2
(defun anyoddp (list)
  "Kontrolu, se ekzistas malpara numero en la eniga listo."
  (if (equal nil list)
      nil
      (if (oddp (car list))
          t
          (anyoddp (rest list)))))

;;; ekzerco 8,4
(defun laugh (n)
  "Eligu multe da simbolo ‘ha’, depende je la eniga numero."
  (cond ((equal nil 0) nil)
        (t (cons ’ha (laugh (- n 1))))))

;;; ekzerco 8,5
(defun add-up (list)
  "Aldonu ĉion da numero en la eniga listo."
  (if (equal nil list)
      0
      (+ (car list) (add-up (rest list)))))

;;; ekzerco 8,6
(defun alloddp (list)
  "Kontrolu, se ĉio da numero en la eniga listo malparas."
  (if (equal nil list)
      t
      (and (oddp (car list)) (alloddp (rest list)))))

;;; ekzerco 8,7
(defun rec-member (item list)
  "Simulu la efekton de la ‘member’ funkcio, per realigi la rikuran teĥnikon."
  (if (or (equal '() item) (equal '() list))
      nil
      (if (equal item (car list))
          t
          (rec-member item (rest list)))))

;;; ekzerco 8,8
(defun rec-assoc (item alist)
  "Simulu la efekton de la ‘assoc’ funkcio, per realigi la rikuran teĥnikon."
  (let ((first-sort (mapcar #'first alist)))
    (cond ((equal alist nil) nil)
          ((equal item (car first-sort)) (car alist))
          (t (rec-assoc item (rest alist))))))

;;; ekzerco 8,9
(defun rec-nth (n list)
  "Simulu la efekton de la ‘nth’ funkcio, per realigi la rikuran teĥnikon."
  (if (equal n 0)
      (car list)
      (rec-nth (- n 1) (rest list))))

;;; ekzerco 8,10
(defun add1 (n)
  "Aldonu unu al la eniga numero."
  (1+ n))

(defun sub1 (n)
  "Subtrahu unu el la eniga numero."
  (1- n))

(defun rec-plus (x y)
  "Aldonu du numerojn per malkreski la duan enigon ĝis tio ekvivalentas al nul, tiam, eligu la unuan
  enigon."
  (cond ((zerop y) x)
        (t (rec-plus (add1 x) (sub1 y)))))

;;; ekzerco 8,17
(defun find-first-odd (list)
  "Eligu la unuan malparan numeron en la eniga listo, ‘nil’ se tio ne ekzistas."
  (let ((1st (first list)))
    (cond ((null list) nil)
          ((oddp 1st) 1st)
          (t (find-first-odd (rest list))))))

;;; ekzerco 8,18
(defun last-element (list)
  "Eligu la lastan elementon de la eniga listo."
  (if (equal nil (cdr list))
      (car list)
      (last-element (rest list))))

;;; ekzerco 8,21
(defun add-nums (n)
  "Sumu ĉion da numero el la eniga numero al nulo."
  (if (equal n 0)
      0
      (+ n (add-nums (- n 1)))))

;;; ekzerco 8,22
(defun all-equal (list)
  "Eligu je t, se ĉio da elemento de la eniga listo similas."
  (cond ((< (length list) 2) t)
        (t (and (equal (car list) (cadr list)) (all-equal (rest list))))))

;;; ekzerco 8,24
(defun count-down (n)
  "Kreu liston kiu havas la dekalkulitajn elementojn el la eniga numero, ĝis unu."
  (if (equal n 0)
      nil
      (cons n (count-down (- n 1)))))

;;; ekzerco 8,26
(defun count-down-0 (n)
  "Kreu liston kiu havas la dekalkulitajn elementojn el la eniga numero, ĝis nulo."
  (if (equal n 0)
      (cons 0 nil)
      (cons n (count-down (- n 1)))))

;;; ekzerco 8,27
(defun sqr (n)
  "Eligu la kvadratigon de la eniga numero."
  (* n n))

(defun square-list (list)
  "Kreu liston kiu enhavas la kvadratigitajn numerojn el la eniga listo."
  (if (equal list nil)
      nil
      (cons (sqr (car nil)) (square-list (rest nil)))))

;;; ekzerco 8,28
(defun my-nth (n list)
  "Simulu la efekton de la ‘nth’ funkcio, tamen, la procezo nun ĉesas post la lasta elemento de la
  eniga listo."
  (if (or (equal nil (rest list)) (equal n 0))
      (car y)
      (rec-nth (- n 1) (rest list))))

;;; ekzerco 8,29
(defun my-member (item list)
  "Simulu la efekton de la ‘member’ funkcio, per uzi la rikuran teĥnikon."
  (cond ((equal nil list) nil)
        ((equal item (car list)) t)
        (t (my-member item (rest list)))))

;;; ekzerco 8,30
(defun my-assoc (item alist)
  "Simulu la efekton de la ‘assoc’ funkcio, per uzi la rikuran teĥnikon."
  (cond ((null alist) nil)
        ((equal item (first (first alist))) (first alist))
        (t (my-assoc item (rest alist)))))

;;; ekzerco 8,31
(defun compare-lengths (list-1 list-2)
  "Komparu la du enigajn listojn, tiam, eligu la respektivan respondon pri la stato."
  (cond ((and (equal nil (rest list-1)) (equal nil (rest list-2))) 'same-length)
        ((and (not (equal nil (rest list-1))) (equal nil (rest list-2))) 'first-is-longer)
        ((and (not (equal nil (rest y))) (equal nil (rest list-1))) 'second-is-longer)
        (t (compare-lengths (rest list-1) (rest list-2)))))

;;; ekzerco 8,32
(defun sum-numeric-elements (list)
  "Sumo ĉion da numero en la eniga listo."
  (let ((1st (car list)))
    (cond ((equal nil list) 0)
          ((numberp 1st) (+ 1st
                            (sum-numeric-elements (rest list))))
          (t (sum-numeric-elements (rest list))))))

;;; ekzerco 8,33
(defun my-remove (item sequence)
  "Simulu la efekton de la ‘remove’ funkcio, per uzi la rikuran teĥnikon."
  (cond ((equal item (car sequence)) (my-remove item (rest sequence)))
        ((equal sequence nil) nil)
        (t (cons (car sequence) (my-remove item (rest sequence))))))

;;; ekzerco 8,34
(defun my-intersection (list-1 list-2)
  "Simulu la efekton de la ‘intersection’ funckio, per uzi la rikuran teĥnikon."
  (cond ((or (null list-1)
             (null list-2)) nil)
        
        ((not (member (first list-1) list-2)) (my-intersection (rest list-1)
                                                               list-2))

        ((member (first list-1) list-2) (cons (first list-1)
                                              (my-intersection (rest list-1)
                                                               list-2)))))

;;; ekzerco 8,35
(defun my-set-difference (list-1 list-2)
  "Simulu la efekton de la ‘set-difference’ funkcio, per uzi la rikuran teĥnikon."
  (cond ((equal nil list-2) list-1)
        ((member (car list-2) list-1) (my-set-difference (remove (car list-2) list-1) (rest list-2)))
        (t (my-set-difference list-1 (rest list-2)))))

;;; ekzerco 8,36
(defun count-odd (list)
  "Nombru la nombrojn de la malparaj numeroj en la eniga listo."
  (cond ((equal nil list) 0)
        ((oddp (car list)) (1+ (count-odd (rest list))))
        (t (count-odd (rest list)))))

;;; ekzerco 8,39
(defun count-atoms (list)
  "Eligu la nombron de ‘atoms’ en la eniga listo."
  (cond ((equal '() list) 1)
        ((atom (car list)) (1+ (count-atoms (cdr list))))
        (t (+ (count-atoms (car list))
              (count-atoms (rest list))))))

;;; ekzerco 8,40
(defun count-cons (list)
  "Eligu la nombron de ‘cons cells’ en la eniga listo."
  (cond ((not (consp list)) 0)
        ((consp list) (+ 1
                      (count-cons (car list))
                      (count-cons (cdr list))))
        (t (+ (count-cons (car list))
              (count-cons (cdr list))))))

;;; ekzerco 8,41
(defun sum-tree (list)
  "Sumu ĉion da ekzistita numero en la eniga listo."
  (cond ((or (not (listp list)) (null list)) 0)
        ((numberp (car list)) (+ (car list)
                              (sum-tree (cdr list))))
        (t (+ (sum-tree (cdr list))
              (sum-tree (car list))))))

;;; ekzerco 8,42
(defun my-subst (new old list)
  "Simulu la efekton de la ‘subst’ funkcio, per uzi la rikuran teĥnikon."
  (cond ((null list) nil)
        ((not (rec-member old list)) list)
        ((equal old (car list)) (cons new
                                 (my-subst new old (cdr list))))
        (t (cons (car list)
                 (my-subst new old (cdr list))))))

;;; ekzerco 8,43
(defun flatten (list)
  "Metu ĉion da elemento el la eniga listo al sola listo."
  (cond ((null list) nil)
        ((atom (car list)) (cons (car list)
                              (flatten (cdr list))))
        (t (append (flatten (car list))
                   (flatten (cdr list))))))

;;; ekzero 8,44
(defun tree-depth (list)
  "Eligu la maksimuman profundecon de la eniga listo."
  (cond ((atom list) 0)
        (t (+ 1
              (max (tree-depth (car list))
                   (tree-depth (cdr list)))))))

;;; ekzerco 8,45
(defun paren-depth (list)
  "Eligu la profundecon de la krampoj."
  (cond ((atom list) 0)
        (t (max (+ 1 (paren-depth (car list)))
                (paren-depth (cdr list))))))

;;; ekzerco 8,46
(defun count-up (n)
  "Nombru kreskante el unu ĝis la eniga numero."
  (cond ((= 0 n) nil)
        (t (append (count-up (- n 1))
                   (list n)))))

;;; ekzerco 8,47
(defun make-loaf (n)
  "Kreu liston kiu havas ‘n’ elementojn."
  (if (equal n 0)
      nil
      (cons 'x
            (make-loaf (- n 1)))))

;;; ekzerco 8,48
(defun bury (item n)
  "Kreu je ‘item’ en ‘n’ profundeco de krampo."
  (cond ((equal n 0) item)
        (t (bury (list item) (- n 1)))))

;;; ekzerco 8,49
(defun pairings (list-1 list-2)
  "Kreu duojn el la du enigaj listoj."
  (cond ((null list-1) nil)
        (t (cons (list (car list-1) (car list-2))
                 (pairings (cdr list-1) (cdr list-2))))))

;;; ekzerco 8,50
(defun sublists (list)
  "Eligu la intersekvan subliston el la eniga listo."
  (cond ((null list) nil)
        (t (cons list
                 (sublists (cdr list))))))

;;; ekzerco 8,51
(defun my-reverse (list)
  "Simulu la efekton de uzi je ‘reverse’, per uzi la rikuran teĥnikon."
  (labels ((m-r (x y)
             (cond ((null x) y)
                   (t (m-r (rest x)
                           (cons (first x)
                                 y))))))
    (m-r list nil)))

;;; ekzerco 8,52
(defun my-union (list-1 list-2)
  "Simulu la efekton de uzi la ‘union‘ funkcion, per uzi la rikuran teĥnikon."
  (cond ((null list-1) list-2)
        ((member (car list-1) list-2) (my-union (cdr list-1) list-2))
        (t (cons (car list-1)
                 (my-union (cdr list-1) list-2)))))

;;; ekzerco 8,53
(defun largest-even (list)
  "Eligu la plej grandan paran numeron en la eniga listo."
  (cond ((null list) 0)
        ((evenp (car list)) (max (car list)
                              (largest-even (cdr list))))
        (t (largest-even (cdr list)))))

;;; ekzerco 8,54
(defun huge (n)
  "Potencigu la enigan numeron al si mem."
  (labels ((my-expt (n e)
             (cond ((= e 0) 1)
                   (t (* n (my-expt n (- e 1)))))))
    (my-expt n n)))

;;; ekzerco 8,56
(defun every-other (list)
  "Eligu ĉiun duan elementon de la eniga listo."
  (cond ((null list) nil)
        (t (cons (car list)
                 (every-other (cdr (cdr list)))))))

;;; ekzerco 8,57 !
(defun left-half (list)
  "Eligu la maldekstran duonon de la eniga listo."
  (labels ((l-h (x y)
    (cond ((null x) nil)
          ((= y (length x)) nil)
          ((> y (length x)) nil)
          (t (cons (car x)
                   (l-h (cdr x)
                        (+ y 1)))))))
    (l-h list 0)))

;;; ekzerco 8,58
(defun merge-lists (list-1 list-2)
  "Kunfandu la enhavojn de la du eniga listo."
  (cond ((null list-1) list-2)
        ((null list-2) list-1)
        ((> (car list-1) (car list-2)) (cons (car list-2)
                                   (merge-lists list-1
                                                (cdr list-2))))
        ((> (car y) (car list-1)) (cons (car list-1)
                                   (merge-lists (cdr list-1)
                                                list-2)))
        (t (cons (car list-1)
                 (cons (car list-2)
                       (merge-lists (cdr list-1)
                                    (cdr list-2)))))))

;;; ekzerco 8,60 a
(setf family
      '((colin nil nil)
        (deirdre nil nil)
        (arthur nil nil)
        (kate nil nil)
        (frank nil nil)
        (linda nil nil)
        (suzanne colin deirdre)
        (bruce arthur kate)
        (charles arthur kate)
        (david arthur kate)
        (ellen arthur kate)
        (george frank linda)
        (hillary frank linda)
        (andre nil nil)
        (tamara bruce suzanne)
        (vincent bruce suzanne)
        (wanda nil nil)
        (ivan george ellen)
        (julie george ellen)
        (marie george ellen)
        (nigel andre hillary)
        (frederick nil tamara)
        (zelda vincent wanda)
        (joshua ivan wanda)
        (quentin nil nil)
        (robert quentin julie)
        (olivia nigel marie)
        (peter nigel marie)
        (erica nil nil)
        (yvette robert zelda)
        (diane peter erica)))

(defun father (person)
  "Eligu la patron de la enigo el la ‘family’ tablo."
  (if person
      (cadr (assoc person family))))

(defun mother (person)
  "Eligu la patrinon de la enigo el la ‘family’ tablo."
  (if person
      (caddr (assoc person family))))

(defun parents (person)
  "Eligu la gepatrojn de la enigo el la ‘family’ tablo."
  (if person
      (if (null (cadr (assoc person family)))
          (cddr (assoc person family))
          (cdr (assoc person family)))))

(defun children (peron)
  "Eligu la idojn de la enigo el la ‘family’ tablo."
  (labels ((c-h (x y)
    (cond ((null x) nil)
          ((null y) nil)
          ((or (equal x (caddar y)) (equal x (cadar y))) (cons (caar y)
                                                              (c-h x (cdr y))))
          (t (c-h x (cdr y))))
    (c-h person family)))))

;;; ekzerco 8,60 b
(defun siblings (person)
  "Eligu la gefratojn de la enigo el la ‘family’ tablo."
  (remove person (children (or (mother person)
                               (father person)))))

;;; ekzerco 8,60 c
(defun mapunion (function list)
  "Eligu la ‘union’ de apliki la ‘function’ enigon al la ‘list’ enigo."
  (reduce #'union (mapcar function list)))

;;; ekzerco 8,60 d 
(defun grandparents (person)
  "Eligu la geavojn de la enigo el la ‘family’ tablo."
  (remove nil (mapunion (lambda (n) (parents n)) (parents person))))

;;; ekzerco 8,60 e
(defun cousins (person)
  "Eligu la unuajn gekuzojn de la enigo el la ‘family’ tablo."
  (mapunion (lambda (n) (children n))
            (mapunion (lambda (n) (siblings n)) (parents person))))

;;; ekzerco 8,60 f
(defun descended-from (person-1 person-2)
  "Kontrolu, se la unua enigo estas postuelo de la dua enigo."
  (cond ((null person-1) nil)
        ((member person-2 (parents person-1)) t)
        (t (or (descended-from (father person-1) person-2)
               (descended-from (mother person-1) person-2)))))

;;; ekzerco 8,60 g
(defun ancestors (person)
  "Eligu la praulojn de la enigo el la ‘family’ tablo."
  (cond ((null person) nil)
        (t (remove-duplicates (remove nil (append (parents person)
                                                  (ancestors (mother person))
                                                  (ancestors (father person))))))))

;;; ekzerco 8,60 h
(defun generation-gap (person ancestor)
  "Eligu la kontraston inter la rekta heredado de la dua enigo."
  (cond ((not (member ancestor (ancestors person))) nil)
        ((member ancestor (parents person)) 1)
        (t (+ 1
              (or (generation-gap (mother person) ancestor)
                  (generation-gap (father person) ancestor))))))

;;; ekzerco 8,61
(defun count-up-i (n)
  "Simulu la efekton de la antaŭa ‘count-up‘ funkcio per vostorikuro."
  (labels ((c-ui (x y)
             (if (equal 0 x)
                 (list y)
                 (c-ui (- x 1) (+ y 1)))))
    (c-u n 1)))

;;; ekzerco 8,62
(defun fact-i (n)
  "Simulu la efekton de la ‘factorial’ funkcio per vostorikuro."
  (labels ((f (x y)
    (if (equal 0 x)
        y
        (f (- x 1) (* x y)))))
    (f n 1)))

;;; ekzerco 8,63
(defun union-i (list-1 list-2)
  "Simulu la efekton de la ‘union’ funkcio per vostorikuro."
  (labels ((ui (x y z)
             (cond ((null x) nil)
                   ((null y) nil)
                   ((and (null x) (null y)) z)
                   ((member (car x) y) (ui (cdr x)
                                           y
                                           z))
                   (t (ui (cdr x)
                          y
                          (cons (car x) z))))))
    (ui list-1 list-2 '())))

(defun intersection-i (list-1 list-2)
  "Simulu la efekton de la ‘intersection’ funkcio per vostorikuro."
  (labels ((ii (x y z)
             (cond ((null x) z)
                   ((null y) z)
                   ((equal (car x) (car y)) (ii (cdr x)
                                                (cdr y)
                                                (cons (car x) z)))
                   ((member (car x) y) (ii (cdr x)
                                           y
                                           (cons (car x) z)))
                   (t (ii (cdr x)
                          (cdr y)
                          z)))))
    (ii list-1 list-2 '())))

(defun set-difference-i (list-1 list-2)
  "Simulu la efekton de la ‘set-difference’ funkcio per vostorikuro."
  (labels ((sdi (x y z)
    (cond ((null y) (append x z))
          ((null x) z)
          ((member (car x) y) (sdi (cdr x)
                                   y
                                   z))
          (t (sdi (cdr x)
                  y
                  (cons (car x) z))))))
    (sdi list-1 list-2 '())))

;;; ekzerco 8,64
(defun tree-find-if (function list)
  "Eligu la unuan elementon de la dua enigo, kiu povas verigi la unuan enigan funkcion."
  (let ((flattened (flatten list)))
    (find-if function flattened)))

;;; ekzerco 8,65
(defun tr-count-slices (loaf)
  "Kalkulu la nombron de elemento de la eniga listo."
  (labels ((tr-cs1 (loaf n)
             (cond ((null loaf) n)
                   (t (tr-cs1 (rest loaf) (+ n 1))))))
    (tr-cs1 loaf 0)))

(defun tr-reverse (list)
  "Simulu la efekton de uzi je ‘reverse’ per vostorikuro."
  (labels ((tr-rev1 (x result)
             (cond ((null x) result)
                   (t (tr-rev1
                       (rest x)
                       (cons (first x) result))))))
    (tr-rev1 list nil)))

;;; ekzerco 8,66
(defun arith-eval (list)
  "Kalkulu la rezulton de taksi la donitan matematikan esprimon."
  (cond ((and (atom (car list)) (atom (caddr list))) (funcall (cadr list) (car list) (caddr list)))
        ((atom (car list)) (funcall (cadr list) (car list) (arith-eval (caddr list))))
        ((atom (caddr list)) (funcall (cadr list) (arith-eval (car list)) (caddr list)))))

;;; ekzerco 8,67
(defun legalp (list)
  "Eligu je t, se la enigo estas valida aritmetika esprimo."
  (cond ((null list) nil)
        ((numberp list) t)
        ((symbolp list) nil)
        ((not (equal 3 (length list))) nil)        
        ((and (numberp (car list)) (or (equal (cadr list) '+)
                                    (equal (cadr list) '-)
                                    (equal (cadr list) '*)
                                    (equal (cadr list) '/))) (and t
                                                               (legalp (caddr list))))
        (t (and (legalp (car list))
                (or (equal (cadr list) '+)
                    (equal (cadr list) '-)
                    (equal (cadr list) '*)
                    (equal (cadr list) '/))
                (legalp (caddr list))))))

;;; ekzerco 8,68
(defun proper-listp (list)
  "Kontrolu, se la enigo listo havas je ‘nil’, kiel sia lasta elemento."
  (cond ((null list) t)
        ((not (listp (cdr list))) nil)
        (t (or (proper-listp (cdr list))))))

;;; ekzerco 8,70
(defun factors-tree (n)
  "Eligu la strukturigadan arbon de la eniga numero."
  (labels ((factors-help-tree (n p)
             (cond ((equal n 1) nil)
                   ((equal n p) n)
                   ((zerop (rem n p))
                    (list n p (factors-help-tree (/ n p) p)))
                   (t (factors-help-tree n (+ p 1))))))
    (factors-help-tree n 2)))

;;; ekzerco 9,1
(defun old-pilots ()
  "Eligu frazon pri aviadistoj."
  (format t "~%There are old pilots,~%and there are bold pilots,~%but there are no old bold
  pilots"))

;;; ekzerco 9,2
(defun draw-line (n)
  "Eligu linion kiu havas je ‘n’ kiel sia longeco."
  (cond ((= 0 n) (format t "~%"))
        (t (format t "*")
           (draw-line (- n 1)))))

;;; ekzerco 9,3
(defun draw-box (length lines)
  "Desegnu keston per voki je ‘draw-line’ multfoje, depende je la dua enigo."
  (cond ((= 0 lines) (format t "~%"))
        (t (draw-line length)
           (draw-box length (- lines 1)))))

;;; ekzerco 9,4
(defun ninety-nine-bottles (n)
  "Eligu la enigan numeron (kun malkreski) kun la lirikoj de specifa kanto."
  (cond ((= 0 n) (format t "No more bottles of beer on the wall"))
        ((= 1 n) (format t "~S bottle of beer on the wall,
  ~S bottle of beer!
  Take one down,
  Pass it around,
  ~S bottle of beer on the wall.~%~%" n n (- n 1)))
        ((= 2 n) (format t "~S bottles of beer on the wall,
  ~S bottles of beer!
  Take one down,
  Pass it around,
  ~S bottle of beer on the wall.~%~%" n n (- n 1))
         (ninety-nine-bottles (- n 1)))
        (t (format t "~S bottles of beer on the wall,
  ~S bottles of beer!
  Take one down,
  Pass it around,
  ~S bottles of beer on the wall.~%~%" n n (- n 1))
           (ninety-nine-bottles (- n 1)))))

;;; ekzerco 9,5
(defun print-board (list)
  "Eligu tabulon de ‘tic-tac-toe’ depende je la eniga listo."
  (labels ((pb (x y)
    (cond ((or (null x) (= 0 y)) (format t "~%")) 
          ((evenp y) (format t "-----------~%")
           (pb x (- y 1)))
          (t (format t " ~A | ~A | ~A ~%" (if (not (null (car x)))
                                            (car x)
                                            " ")
                     (if (not (null (cadr x)))
                                            (cadr x)
                                            " ")
                     (if (not (null (caddr x)))
                                            (caddr x)
                                            " "))
             (pb (cdddr x) (- y 1))))))
    (pb list 5)))

;;; ekzerco 9,6
(defun hourly-wage ()
  "Kalkulu la pagon de laboranto depende je la enigoj."
  (format t "Enter hourly wage (in dollars): ")
  (let ((x (read)))
    (format t "Enter hours worked: ")
    (let ((y (read)))
      (format t "Your gross pay is $~S" (* x y)))))

;;; ekzerco 9,7
(defun cookie-monster ()
  "Iteraciu, krom se, oni donas je ‘cookie’ kiel la enigo de la prompto."
  (format t "Give me cookie!!!~%Cookie? ")
  (let ((x (read)))
    (cond ((equal x 'cookie) (format t "Thank you!...Munch munch munch...BURP"))
          (t (format t "No want ~S...~%~%" x)
             (cookie-monster)))))

;;; ekzerco 9,10 a
(defun space-over (n)
  "Eligu multe da spaceto depende je la eniga numero."
  (cond ((equal 0 n) (format t ""))
        ((< n 0) (format t "Error!"))
        ((= n 0) (format t "~A" ""))
        (t (format t "~A" " ")
           (space-over (- n 1)))))

(defun test (n)
  "Testu la eligon de la ‘space-over’ funkcio."
  (format t "~%>>>")
  (space-over n)
  (format t "<<<"))

;;; ekzerco 9,10 b
(defun plot-one-point (plotting-string y-val)
  "Eligu je ‘plotting-string’ en la kolumno ‘y-val’."
  (cond ((< y-val 0) (format t "Error!"))
        (t (space-over y-val)
           (format t "~A~%" plotting-string))))

;;; ekzerco 9,10 c
(defun plot-points (string y-vals)
  "Eligu je ‘string’ al diversaj linioj depende je la valoro de ‘y-vals’."
  (cond ((= 1 (length y-vals)) (plot-one-point string (car y-vals)))
        (t (plot-one-point string (car y-vals))
           (plot-points string (cdr y-vals)))))

;;; ekzerco 9,10 d
(defun generate (m n)
  "Eligu la numerojn el la unua enigo al la dua enigo."
  (cond ((equal m n) (list n))
        ((> m n) (format t "Error!"))
        (t (cons m
                 (generate (+ m 1)
                           n)))))

;;; ekzerco 9,10 e
(defun square (n)
  "Eligu la kvadratigon de la eniga numero."
  (* n n))

(defun make-graph ()
  "Kreu diagramon depende je la sekvaj enigoj."
  (format t "Function to graph? ")
  (let ((func (read)))
    (format t "Starting x value? ")
    (let ((start (read)))
      (format t "Ending x value? ")
      (let ((end (read)))
        (format t "Plotting string? ")
        (let ((plotting-string (read)))
          (plot-points plotting-string (mapcar func (generate start end))))))))

;;; ekzerco 9,11
(defun dotprin1 (list)
  "Kreu novan liston el la eniga listo per la ‘dotted-list’ formo."
  (cond ((atom list) (format t "~a" list))
        (t (format t "(")
           (dotprin1 (car list))
           (format t ".")
           (dotprin1 (cdr list))
           (format t ")"))))

;;; ekzerco 9,15
(defun hybrid-prin1 (list)
  "Eligu la hibridan notacion de la eniga listo."
  (cond ((atom (cdr list)) (cons (car list) (cdr list)))
        (t (cons (car list)
                 (hybrid-prin1 (cdr list))))))

;;; ekzerco 10,2
(setf *total-glasses* 0)

(defun sell (n)
  "Kresku la nunan valoron de *total-glasses* per la eniga nombro."
  (incf *total-glasses* n)
  (format t
          "~&That makes ~S glasses so far today."
          *total-glasses*))

;;; ekzerco 10,3
(setf *friends* nil)

(setf *dupe-meet* 0)

(defun meet (person)
  "Simulu la renkonti iun, tiam, modifiku la valoron de *dupe-meet*, se ekzistas duplikato, aŭ
  *friends*, se la enigo novas."
  (cond ((equal person (first *friends*))
         (incf *dupe-meet* 1)
         'we-just-met)
        ((member person *friends*)
         (incf *dupe-meet* 1)
         'we-know-each-other)
        (t (push person *friends*)
           'pleased-to-meet-you)))

;;; ekzerco 10,4
(defun forget (person)
  "Forprenu la enigon el la *friends* listo."
  (cond ((not (member person *friends*)) (format t "~S not present in *friends*" person))
        (t (setf *friends* (remove person *friends*)))))

;;; ekzerco 10,5
(defun ugly (x y)
  "Eligu la elcentan maksimumon de la mezo de la pli granda enigo."
  (cond ((> x y) (ugly y x))
        (t (let* ((avg (/ (+ x y) 2.0))
                  (pct (* 100 (/ avg y))))
             (list 'average avg 'is
                   pct 'percent 'of 'max)))))

;;; ekzerco 10,8
(defun make-board ()
  "Kreu novan liston kiu havas nulon kiel siaj elementoj."
  (list 'board 0 0 0 0 0 0 0 0 0))

(defun convert-to-letter (v)
  "Konvertu la enigan numeron al la respektiva litero."
  (cond ((equal v 1) "O")
        ((equal v 10) "X")
        (t " ")))

(defun print-row (x y z)
  "Eligu horizontalon depende je la enigoj."
  (format t "~&  ~A | ~A | ~A"
          (convert-to-letter x)
          (convert-to-letter y)
          (convert-to-letter z)))

(defun print-board (board)
  "Eligu tabulon per uzi je ‘print-row’."
  (format t "~%")
  (print-row
   (nth 1 board) (nth 2 board) (nth 3 board))
  (format t "~& -----------")
  (print-row
   (nth 4 board) (nth 5 board) (nth 6 board))
  (format t "~& -----------")
  (print-row
   (nth 7 board) (nth 8 board) (nth 9 board))
  (format t "~%~%"))

(setf *computer* 10)

(setf *opponent* 1)

(defun make-move (player pos board)
  "Transdonu la nunan movon al la unua enigo."
  (setf (nth pos board) player)
  board)

(setf *triplets*
      '((1 2 3) (4 5 6) (7 8 9)
        (1 4 7) (2 5 8) (3 6 9)
        (1 5 9) (3 5 7)))

(defun sum-triplet (board triplet)
  "Sumu la valorojn de la enhavoj de la dua enigo."
  (+ (nth (first triplet) board)
     (nth (second triplet) board)
     (nth (third triplet) board)))

(defun compute-sums (board)
  "Kalkulu la sumojn de la tuta tabulo."
  (mapcar #'(lambda (triplet)
              (sum-triplet board triplet))
          *triplets*))

(defun winner-p (board)
  "Kontrolu, se jam ekzistas venkinto."
  (let ((sums (compute-sums board)))
    (or (member (* 3 *computer*) sums)
        (member (* 3 *opponent*) sums))))

(defun play-one-game ()
  "Komencludu la ludon de ‘tic-tac-toe’."
  (if (y-or-n-p "Would you like to go first? ")
      (opponent-move (make-board))
      (computer-move (make-board))))

(defun opponent-move (board)
  "Kreu movon, se ne ankoraŭ ekzistas venkinto."
  (let* ((pos (read-a-legal-move board))
         (new-board (make-move
                     *opponent*
                     pos
                     board)))
    (print-board new-board)
    (cond ((winner-p new-board)
           (format t "~&You win!"))
          ((board-full-p new-board)
           (format t "~&Tie game."))
          (t (computer-move new-board)))))

(defun read-a-legal-move (board)
  "Determinu kie oni povas meti validan movon."
  (format t "~&Your move: ")
  (let ((pos (read)))
    (cond ((not (and (integerp pos)
                     (<= 1 pos 9)))
           (format t "~&Invalid input.")
           (read-a-legal-move board))
          ((not (zerop (nth pos board)))
           (format t
                   "~&That space is already occupied.")
           (read-a-legal-move board))
          (t pos))))

(defun board-full-p (board)
  "Kontrolu, se la tabulo jam plenas."
  (not (member 0 board)))

(defun computer-move (board)
  "Elektu movon depende je la eligo de la helpilaj funkcioj."
  (let* ((best-move (choose-best-move board))
         (pos (first best-move))
         (strategy (second best-move))
         (new-board (make-move
                     *computer* pos board)))
    (format t "~&My move: ~S" pos)
    (format t "~&My strategy: ~A~%" strategy)
    (print-board new-board)
    (cond ((winner-p new-board)
           (format t "~&I win!"))
          ((board-full-p new-board)
           (format t "~&Tie game."))
          (t (opponent-move new-board)))))

(defun random-move-strategy (board)
  "Listigu la eligon de la ‘pick-random-empty-position’ helpila funkcio."
  (list (pick-random-empty-position board)
        "random move"))

(defun pick-random-empty-position (board)
  "Elektu hazardan movon depende je la nuna stato de la eniga tabulo."
  (let ((pos (+ 1 (random 9))))
    (if (zerop (nth pos board))
        pos
        (pick-random-empty-position board))))

(defun make-three-in-a-row (board)
  "Eligu la valoron de la ‘win-or-block’ helpila funkcio, kun la uzita strategio."
  (let ((pos (win-or-block board
                           (* 2 *computer*))))
    (and pos (list pos "make three in a row"))))

(defun block-opponent-win (board)
  "Eligu la valoron de la ‘win-or-block’ helpila funkcio, kun la uzita strategio."
  (let ((pos (win-or-block board
                           (* 2 *opponent*))))
    (and pos (list pos "block opponent"))))

(defun win-or-block (board target-sum)
  "Kreu movon kiu povas krei je ‘triplet’ kiu povas venki la ludon, aŭ bloki la venkinton."
  (let ((triplet (find-if
                  #'(lambda (trip)
                      (equal (sum-triplet board
                                          trip)
                             target-sum))
                    *triplets*)))
    (when triplet
      (find-empty-position board triplet))))

(defun find-empty-position (board squares)
  "Trovu pozicion en la unua enigo, kiu ne okupitas."
  (find-if #'(lambda (pos)
               (zerop (nth pos board)))
           squares))

(defun choose-best-move (board)
  "Elektu la plej bonan movon depende je la ordo de la plej bonaj strategioj."
  (or (make-three-in-a-row board)
      (block-opponent-win board)
      (squeeze-play-attempt board)
      (two-on-one-play-attempt board)
      (block-squeeze-play board)
      (block-two-on-one board)
      (random-move-strategy board)))

;;; ekzerco 10,8 a
(setf *corners*
      '(1 3 7 9))

(setf *sides*
      '(2 4 6 8))

;;; ekzerco 10,8 b
(defun block-squeeze-play (board)
  "Kontrolu la diagonalojn, se ekzistas la ‘o-x-o’ ŝablono."
  (let ((pos (or (and (equal (nth 1 board) 1)
                      (equal (nth 5 board) 10)
                      (equal (nth 9 board) 1))
                 (and (equal (nth 3 board) 1)
                      (equal (nth 5 board) 10)
                      (equal (nth 7 board) 1)))))
    (when pos
      (list (find-empty-position board *sides*) "block squeeze play"))))

;;; ekzerco 10,8 c
(defun block-two-on-one (board)
  "Kontrolu la diagonalojn, se ekzistas la ‘o-o-x‘, aŭ la ‘x-o-o’ ŝablonoj."
  (let ((pos (or (and (equal (nth 1 board) 10)
                      (equal (nth 5 board) 1)
                      (equal (nth 9 board) 1))
                 (and (equal (nth 3 board) 1)
                      (equal (nth 5 board) 1)
                      (equal (nth 7 board) 10))
                 (and (equal (nth 1 board) 1)
                      (equal (nth 5 board) 1)
                      (equal (nth 9 board) 10))
                 (and (equal (nth 3 board) 10)
                      (equal (nth 5 board) 1)
                      (equal (nth 7 board) 1)))))
    (when pos
      (list (find-empty-position board *corners*) "block two-on-one"))))

;;; ekzerco 10,8 e
(setf *for-squeeze*
      '((1 9)
        (3 7)
        (7 3)
        (9 1)))

(defun squeeze-play-attempt (board)
  (let ((pos (or (and (equal (nth 1 board) 10)
                      (equal (nth 5 board) 1))
                 (and (equal (nth 3 board) 10)
                      (equal (nth 5 board) 1))
                 (and (equal (nth 7 board) 10)
                      (equal (nth 5 board) 1))
                 (and (equal (nth 9 board) 10)
                      (equal (nth 5 board) 1)))))
    (when pos
      (list (cond ((and (equal (nth 1 board) 10)
                        (find-empty-position board '(9))) 9)
                  ((and (equal (nth 3 board) 10)
                        (find-empty-position board '(7))) 7)
                  ((and (equal (nth 7 board) 10)
                        (find-empty-position board '(3))) 3)
                  ((and (equal (nth 9 board) 10)
                        (find-empty-position board '(1))) 1)) "attempt squeeze play"))))

(defun two-on-one-play-attempt (board)
  (let ((pos (or (and (equal (nth 1 board) 10)
                      (equal (nth 5 board) 10)
                      (equal (nth 2 board) 1)
                      (equal (nth 9 board) 1))

                 (and (equal (nth 1 board) 10)
                      (equal (nth 5 board) 10)
                      (equal (nth 4 board) 1)
                      (equal (nth 9 board) 1))
                 

                 (and (equal (nth 5 board) 10)
                      (equal (nth 9 board) 10)
                      (equal (nth 1 board) 1)
                      (equal (nth 6 board) 1))

                 (and (equal (nth 5 board) 10)
                      (equal (nth 9 board) 10)
                      (equal (nth 1 board) 1)
                      (equal (nth 8 board) 1))

                 
                 (and (equal (nth 5 board) 10)
                      (equal (nth 7 board) 10)
                      (equal (nth 3 board) 1)
                      (equal (nth 4 board) 1))

                 (and (equal (nth 5 board) 10)
                      (equal (nth 7 board) 10)
                      (equal (nth 3 board) 1)
                      (equal (nth 8 board) 1))

                 
                 (and (equal (nth 3 board) 10)
                      (equal (nth 5 board) 10)
                      (equal (nth 6 board) 1)
                      (equal (nth 7 board) 1))
                 
                 (and (equal (nth 3 board) 10)
                      (equal (nth 5 board) 10)
                      (equal (nth 2 board) 1)
                      (equal (nth 7 board) 1)))))
    (when pos
      (list (cond ((and (equal (nth 1 board) 10)
                        (equal (nth 5 board) 10)
                        (equal (nth 2 board) 1)
                        (equal (nth 9 board) 1)
                        (find-empty-position board '(7))) 7)

                  ((and (equal (nth 1 board) 10)
                        (equal (nth 5 board) 10)
                        (equal (nth 4 board) 1)
                        (equal (nth 9 board) 1)
                        (find-empty-position board '(3))) 3)

                  ((and (equal (nth 5 board) 10)
                        (equal (nth 9 board) 10)
                        (equal (nth 1 board) 1)
                        (equal (nth 6 board) 1)
                        (find-empty-position board '(7))) 7)

                  ((and (equal (nth 5 board) 10)
                        (equal (nth 9 board) 10)
                        (equal (nth 1 board) 1)
                        (equal (nth 8 board) 1)
                        (find-empty-position board '(3))) 3)

                  ((and (equal (nth 5 board) 10)
                        (equal (nth 7 board) 10)
                        (equal (nth 3 board) 1)
                        (equal (nth 4 board) 1)
                        (find-empty-position board '(9))) 9)

                  ((and (equal (nth 5 board) 10)
                        (equal (nth 7 board) 10)
                        (equal (nth 3 board) 1)
                        (equal (nth 8 board) 1)
                        (find-empty-position board '(1))) 1)

                  ((and (equal (nth 3 board) 10)
                        (equal (nth 5 board) 10)
                        (equal (nth 6 board) 1)
                        (equal (nth 7 board) 1)
                        (find-empty-position board '(1))) 1)

                  ((and (equal (nth 3 board) 10)
                        (equal (nth 5 board) 10)
                        (equal (nth 2 board) 1)
                        (equal (nth 7 board) 1)
                        (find-empty-position board '(9))) 9)) "attempt two-on-one-play"))))

;;; ekzerco 10,9
(defun chop (list)
  "Mallongigi la enigan liston al listo kiu nur havas unuan elementon."
  (setf (cdr list) nil)
  list)

;;; ekzerco 10,10
(defun ntack (list symbol)
  "Kudru la duan enigon en la unua enigo."
  (nconc list (list symbol)))

;;; ekzerco 11,1
(defun it-member (item list)
  "Simulu la efekton de la ‘member’ funkcio, per uzi la iteracian teĥnikon."
  (dolist (a list nil)
    (format t "Checking ~S~%" a)
    (if (equal a item)
        (return t))))

;;; ekzerco 11,2
(defun it-assoc (item alist)
  "Simulu la efekton de la ‘assoc’ funkcio, per uzi la iteracian teĥnikon."
  (dolist (a alist nil)
    (format t "Checking ~S~%" a)
    (if (equal item (car a))
        (return a))))

;;; ekzerco 11,3
(defun check-all-odd (list)
  "Kontrolu, se ĉio da elemento de la eniga listo malparas."
  (cond ((null list) t)
        ((oddp (car list)) (format t "Checking ~S...~%" (car list))
         (and t
              (check-all-odd (cdr list))))
        (t (format t "Checking ~S...~%" (car list))
           nil)))

;;; ekzerco 11,4
(defun it-length (list)
  "Simulu la efekton de la ‘length’ funkcio, per uzi la iteracian teĥnikon."
  (let ((stack 0))
    (dolist (element list stack)
      (if (not (null element))
          (setf stack (+ stack 1))))))

;;; ekzerco 11,5
(defun it-nth (n list)
  "Simulu la efekton de la ‘nth’ funkcio, per uzi la iteracian teĥnikon."
  (let ((num n))
    (dolist (element list)
      (if (= 0 num)
          (return element)
          (setf num (- num 1))))))

;;; ekzerco 11,6
(defun it-union (list-1 list-2)
  "Simulu la efekton de la ‘union’ funkcio, per uzi la iteracian teĥnikon."
  (let ((stack list-2))
    (dolist (element list-1 stack)
      (if (not (member element list-2))
          (push element stack)))))

;;; ekzerco 11,8
(defun it-reverse (list)
  "Simulu la efekton de la ‘reverse’ funkcio, per uzi la iteracian teĥnikon."
  (let ((listo nil))
    (dolist (element list listo)
      (push element listo))))

;;; ekzerco 11,9
(defun it-check-all-odd (list)
  "Simulu la efekton de la ‘check-all-odd’ funkcio, per uzi la ‘do’ funkcion."
  (do ((stack list (cdr stack)))
      ((if (null (car stack))
           (return t)
           (evenp (car stack))) (format t "Checking ~S...~%" (car stack)))    
    (cond ((null stack) (return t))
          (t (format t "Checking ~S...~%" (car stack))))))

;;; ekzerco 11,10
(defun launch (n)
  "Eligu la numerojn el la eniga numero ĝis nulo, tiam, eligu je ‘Blast off~’."
  (dotimes (i n)
    (format t "~S..." (abs (- i 10))))
  (format t "Blast off!"))

;;; ekzerco 11,11
(defun find-largest (list-of-numbers)
  "Trovu le plej grandan numeron el la eniga listo."
  (do* ((e (rest list-of-numbers) (rest e))
        (ll (first list-of-numbers)))
       ((null e) ll)
    (when (> (car e) ll)
      (setf ll (car e)))))

;;; ekzerco 11,12
(defun power-of-2 (n)
  "Eligu la valoron de potencigi du al la eniga numero."
  (do* ((unu n (- unu 1))
        (result 1 (incf result result)))
       ((zerop unu) result)))

;;; ekzerco 11,13
(defun first-non-integer (list)
  "Eligu la unuan elementon de la eniga listo kiu ne estas entjero."
  (dolist (z list 'none)
    (unless (integerp z)
      (return z))))

;;; ekzerco 11,22 a
(defun complement-base (base)
  "Eligu la komplementon de la enigo."
  (cond ((equal 'a base) 't)
        ((equal 'g base) 'c)
        ((equal 't base) 'a)
        ((equal 'c base) 'g)))

;;; ekzerco 11,22 b
(defun complement-strand (sequence)
  "Eligu la komplementofadenon de la enigo."
  (let ((l nil))
    (dolist (e (reverse sequence) l)
      (push (complement-base e) l))))

;;; ezkerco 11,22 c
(defun make-double (strand)
  "Eligu la komplementofadenon de la enigo per duoj."
  (let ((d nil))
    (dolist (e (reverse strand) d)
      (push (list e (complement-base e)) d))))

;;; ekzerco 11,22 d
(defun count-bases (double-strand)
  "Kreu liston de duoj, kiu kalkulas la nombrojn de la diversaj specoj de fadeno."
  (labels ((mpa (lis)
             (let ((l nil))
               (dolist (e (reverse lis) l)
                 (push (cadr e) l)
                 (push (car e ) l)))))
    (let* ((as 0)
           (gs 0)
           (cs 0)
           (ts 0))
      (dolist (e (if (atom (car double-strand))
                     double-strand
                     (mpa double-strand)) (list (list 'a as)
                                                (list 't ts)
                                                (list 'g gs)
                                                (list 'c cs)))
        (cond ((equal e 'a) (incf as 1))
              ((equal e 'g) (incf gs 1))
              ((equal e 'c) (incf cs 1))
              ((equal e 't) (incf ts 1)))))))

;;; ekzerco 11,22 e
(defun prefixp (strand-1 strand-2)
  "Kontrolu, se la unua enigo estas prefikso de la dua enigo."
  (do ((list-1 strand-1 (cdr list-1))
       (list-2 strand-2 (cdr list-2)))
      ((null list-1) t)
    (if (not (equal (car list-1) (car list-2)))
        (return nil))))

;;; ekzerco 11,22 f
(defun appearsp (strand-1 strand-2)
  "Kontrolu, se la unua enigo aperas en la dua enigo, orde."
  (do ((list-1 strand-1 list-1)
       (list-2 strand-2 (cdr list-2)))
      ((or (null list-1) (null list-2)) nil)
    (if (prefixp list-1 list-2)
        (return t))))

;;; ekzerco 11,22 g
(defun coversp (strand-1 strand-2)
  "Kontrolu, se la unua enigo aperas en la dua enigo multfoje aŭ ne, ekzakte."
  (do ((list-1 strand-2 (nthcdr (length strand-1) list-1)))
      ((null list-1) t)
    (if (not (prefixp strand-1 list-1))
        (return nil))))

;;; ekzerco 11,22 h
(defun prefix (n strand)
  "Eligu la maldekstran parton de la dua enigo, per la unua numera enigo."
  (do ((listo strand (butlast listo)))
      ((equal (length listo) n) (return listo))))

;;; ekzerco 11,22 i
(defun kernel (strand-list)
  "Eligu la minimuman fadenon kiu povas ripeteligi la enigan liston multfoje."
  (do* ((n 1 (+ n 1))
        (test-list (prefix n strand-list) (prefix n strand-list)))
       ((coversp test-list strand-list) (return test-list))))

;;; ekzerco 11,22 j
(defun draw-dna (strand)
  "Desegnu la enigan fadenon kun sia komplementofadeno, simile al la bildo en la ĉapitrokomenco."
  (labels ((skel (n s)
             (format t "~&")
             (dotimes (i n)
               (format t "~A" s)))
           (skel-b (s)
             (format t "~&")
             (dolist (e s)
               (format t "  ~A  " e))))
    (let ((l (length strand)))
      (skel l "-----")
      (skel l "  !  ")
      (skel-b strand)
      (skel l "  .  ")
      (skel l "  .  ")
      (skel-b (complement-strand strand))
      (skel l "  !  ")
      (skel l "-----"))))

;;; ekzerco 12,4 a
(defstruct node
  name
  question
  yes-case
  no-case)

;;; ekzerco 12,4 b
(defun init ()
  "Agordu je *node-list* por enhavi je nil."
  (setf *node-list* nil))

;;; ekzerco 12,4 c
(defun add-node (n q yes no)
  "Aldonu nodon al la *node-list* variablo."
  (push (make-node :name n
                   :question q
                   :yes-case yes
                   :no-case no)
        *node-list*))

;;; ekzerco 12,4 d
(defun find-node (node-name)
  "Eligu la enigan nodon se tio ekzistas en *node-list*, je ‘nil’ se ne."
  (labels ((fn (x y)
             (cond ((null y) nil)
                   ((equal x (node-name (car y))) (car y))
                   (t (fn x (cdr y))))))
    (fn node-name *node-list*)))

;;; ekzerco 12,4 e
(defun process-node (node-name)
  "Trovu la enigan nodon, tiam, demandu la ‘jes’ aŭ ‘ne’ demandojn, aŭ eligu je ‘nil’ se tiu nodo ne
  ankoraŭ ekzistas."
  (labels ((pn (x)
             (cond ((find-node x) (if (yes-or-no-p (node-question (find-node x)))
                                      (node-yes-case (find-node x))
                                      (node-no-case (find-node x)))))))
    (pn node-name)))

;;; ekzerco 12,4 f
(defun run ()
  "Iteraciu per daŭre voki je ‘process-node’, tiam, konservu la eligan valoron en la ‘current-node’
  variablo; Se la konservita valoro estas ĉeno, eligu tion."
  (let* ((current-node 'smart)
         (unu (process-node current-node)))
    (cond ((symbolp unu) (setf current-node unu)
           current-node)
          ((null unu) nil)
          (t nil))))

;;; ekzerco 12,4 g
(defun add-new-node ()
  "Aldonu novan nodon per demandi multe da demando."
  (format t "Input node name: ~&")
  (let ((unu (read)))
    (format t "Input node question: ~&")
    (let ((du (read)))
      (format t "Input yes response: ~&")
      (let ((tri (read)))
        (format t "Input no response: ~&")
        (let ((kvar (read)))
          (add-node unu du tri kvar))))))

;;; ekzerco 13,1
(defun subprop (symbol letter property)
  "Forprenu je ‘letter’ el la ‘property’ kampo de la eniga ‘symbol’."
  (setf (get symbol property) (remove letter (get symbol property))))

;;; ekzerco 13,2
(defun forget-meeting (person-1 person-2)
  "Forgesu la renkontiĝon de la du enigaj homoj per viŝu je ‘property list’ de la du enigoj."
  (subprop person-1 person-2 'has-met)
  (subprop person-2 person-1 'has-met)
  t)

;;; ekzerco 13,3
(defun my-get (symbol indicator)
  "Simulu la efekton de la ‘get’ funkcio, per uzi je ‘symbol-plist’."
  (do ((p (symbol-plist symbol) (cddr p)))
      ((null p) nil)
    (if (equal indicator (car p))
        (return (cadr p)))))

;;; ekzerco 13,4
(defun hasprop (symbol property)
  "Kontrolu, se ‘symbol’ havas je ‘property’ enigon en sia ‘property list’."
  (labels ((hp (s p acc)
             (cond ((null acc) nil)
                   ((equal p (car acc)) t)
                   (t (hp s p (cddr acc))))))
    (hp symbol property (symbol-plist sym))))

;;; ekzerco 13,8 b
(defun new-histogram (bins)
  "Komencvalorizu je ‘*total-points*’ al 0, kaj je *hist-array* al sekvenco kiu enhavas je ‘bins’
  nombrojn kiel la nombro de sia enhavo."
  (setf *total-points* 0)
  (setf *hist-array* (make-array bins)))

;;; ekzerco 13,8 c
(defun record-value (n)
  "Kresku la ‘n’-an valoron de ‘*hist-array*’, se la enigo estas inter nulo kaj dek."
  (if (and (> n -1) (< n 11))
      (incf (aref *hist-array* n))
      (format t "Input is out of range.")))

;;; ekzerco 13,8 d
(defun print-hist-line (n)
  "Eligu la linion de la histogramo, per trovi la enigon en ‘*hist-array*’."
  (labels ((p-a (n)
             (let ((unu ""))
               (cond ((zerop n) unu)
                     (t (concatenate 'string "*" (p-a (- n 1))))))))
    (format t "~2S [ ~3S] ~A~%" n
            (aref *hist-array* n)
            (p-a (aref *hist-array* n)))))

;;; ekzerco 13,8 e
(defun print-histogram ()
  "Eligu diagramon kiu enhavas la ordigitan nombron de la generitaj hazardaj numeroj."
  (dotimes (i 11)
    (print-hist-line i))
  (format t "    200 total"))

;;; ekzerco 13,9 b
(defun make-substitution (character-1 character-2)
  "Konservu la enigojn ene siaj respektivaj tablaj variabloj, por ke, ili kongruas al unu la alia,
  per la deĉifrita valoro."
  (setf (gethash character-1 *decipher-table*)
        character-2)
  (setf (gethash character-2 *encipher-table*)
        character-1))

;;; ekzerco 13,9 c
(defun undo-substitution (letter)
  "Agordu la du tablojn de la enigo, por ke, la valoroj de la tablo fariĝos je ‘nil’."
  (setf (gethash (gethash letter *decipher-table*) *encipher-table*)
        nil)
  (setf (gethash letter *decipher-table*)
        nil))

;;; ekzerco 13,9 d
(defun clear ()
  "Senenhavu la du hakkettabelojn."
  (clrhash *decipher-table*)
  (clrhash *encipher-table*))

;;; ekzerco 13,9 e
(defun decipher-string (string)
  "Deĉifru la enigon per krei novan ĉenon, tiam, se la signo ne ekvivalentas al ‘nil’ per sia valoro
  en la tabloj, enigu la ĉenon en la nova ĉeno."
  (let ((unu (make-string (length string) :initial-element #\Space)))
    (do* ((n 0 (+ n 1))
          (du (aref string n) (aref string n)))
         ((= n (- (length string) 1)) (if (gethash du *decipher-table*)
                                     (setf (aref unu n) (gethash du *decipher-table*))
                                     (setf (aref unu n) #\Space))
          unu)
      (cond ((null (gethash du *decipher-table*)) (setf (aref unu n) #\Space))
            ((gethash du *decipher-table*) (setf (aref unu n) (gethash du *decipher-table*)))))))

;;; ekzerco 13,9 f
(defun show-line ()
  "Eligu unu linion de teksto kun sia deĉifrita egalvaloro malsupre."
  (format t "~A~&" (car crypto-text))
  (format t "~A" (decipher-string (car crypto-text))))

;;; ekzerco 13,9 g
(defun show-text (cryptogram)
  "Eligu la liniojn simile al la ‘show-line’ funkcio, tamen, per multe da linio."
  (cond ((null cryptogram) (format t "~&"))
        (t (format t "~A~&" (car cryptogram))
           (format t "~A~%~%" (decipher-string (car cryptogram)))
           (show-text (rest cryptogram)))))

;;; ekzerco 13,9 h
(defun get-first-char (string)
  "Eligu la unuan signon de la enigo."
  (char-downcase
   (char (format nil "~A" string) 0)))

;;; ekzerco 13,9 i
(defun read-letter ()
  "Eligu la unuan signon de la bezonita enigo, aŭ eligu je ‘'end’ aŭ ‘'undo’ se ili estas la enigo."
  (format t "Input: ")
  (let ((unu (read)))
    (if (or (equal unu 'end)
            (equal unu 'undo))
        unu
        (get-first-char unu))))

;;; ekzerco 13,9 j
(defun sub-letter (character)
  "Substituu la enigon (en la kriptogramo) per la nova signo de la sekva prompto."
  (cond ((gethash character *decipher-table*) (format t "Letter has already been deciphered to ~S."
                                               (gethash character *decipher-table*)))
        (t (format t "What does ~S decipher to? " character)
           (let ((unu (read)))
             (if (characterp unu)
                 (make-substitution character unu)
                 (format t "Input is not of type character.~&"))))))

;;; ekzerco 13,9 k
(defun undo-letter ()
  "Malfaru la efekton de la ‘sub-letter’ funkcio, per forprenu la enigon de la sekva prompto."
  (format t "Undo which letter?: ")
  (let ((unu (read)))
    (if (gethash unu *decipher-table*)
        (undo-substitution unu)
        (format t "Letter has not been deciphered yet.~&"))))

;;; ekzerco 13,9 l
(defun solve (cryptogram)
  "Iteraciu daŭre, per voki je ‘read-letter’, ‘sub-letter’, aŭ ‘undo-letter’, depende je la enigo de
  la sekvaj promptoj."
  (let* ((x (make-string 10 :initial-element #\-))
         (unu (format t "~A~&" x)))
    unu
    (show-text cryptogram)
    unu
    (format t "Substitute which letter? ")
    (let ((unu (read-letter)))
      (cond ((characterp unu) (sub-letter unu)
             (solve cryptogram))
            ((equal unu 'undo) (undo-letter)
             (solve cryptogram))
            ((equal unu 'end) t)
            (t (format t "Feku vin."))))))

;;;  ekzerco 14,3
(defmacro set-nil (var)
  "Makroo kiu agordas la valoron de la enigo al ‘nil’."
  (list 'setf var 'nil))

;;; ekzerco 14,4
(defmacro simple-rotatef (a b)
  "Makroo kiu interŝanĝas la valorojn de la du enigoj."
  (list 'let (list (list 'unu a)
                   (list 'du b))
        (list 'setf a 'du)
        (list 'setf b 'unu)))

;;; ekzerco 14,5
(defmacro set-mutual (var-1 var-2)
  "Makroo kiu agordas la valoron de unu la alia al la simbolo de la nomo de unu la alia."
  (list 'progn
        (list 'setf var-1 (list 'quote var-2))
        (list 'setf var-2 (list 'quote var-1))))

;;; ekzerco 14,6
(defmacro variable-chain (&rest vars)
  "Makroo kiu funkcias simile al ‘set-mutual’, tamen, la valoro de la nuna elemento fariĝas la
  simobolo de la sekva elemento."
  `(progn
     ,@(mapcar #'(lambda (var1 var2)
                   `(setf ,var1 ',var2))
               vars (cdr vars))))

;;; ekzerco 14,7
(defnode start)
(defnode have-5)
(defnode have-10)
(defnode have-15)
(defnode have-20)
(defnode have-25)
(defnode end)

(defarc start nickel have-5 "Clunk!")
(defarc start dime have-10 "Clink!")
(defarc start quarter have-25 "Ker-chunk!")
(defarc start coin-return start "Nothing to return.")

(defarc have-5 nickel have-10 "Clunk!")
(defarc have-5 dime have-15 "Clink!")
(defarc have-5 coin-return start "Returned five cents.")

(defarc have-10 nickel have-15 "Clunk!")
(defarc have-10 dime have-20 "Clink!")
(defarc have-10 coint-return start "Returned ten cents.")

(defarc have-15 nickel have-20 "Clunk!")
(defarc have-15 dime have-25 "Clink.")
(defarc have-15 gum-button end "Deliver gum.")
(defarc have-15 coin-return start "Returned fifteen cents.")

(defarc have-20 nickel have-25 "Clunk.")
(defarc have-20 dime have-25 "Nickel returned.")
(defarc have-20 gum-button end "Deliver gum, nickel change.")
(defarc have-20 mint-button end "Deliver mints.")
(defarc have-20 coin-return start "Returned twenty cents.")

(defarc have-25 nickel have-25 "Nickel returned.")
(defarc have-25 dime have-25 "Dime returned.")
(defarc have-25 gum-button  end "Deliver gum, dime change.")
(defarc have-25 mint-button end "Deliver mints, nickel change.")
(defarc have-25 choco-button end "Deliver chocolate bars.")
(defarc have-25 coin-return start "Returned twenty five cents.")

(defstruct (node (:print-function print-node))
  (name nil)
  (inputs nil)
  (outputs nil))

(defun print-node (node stream depth)
  "Eligu la nomon de la unua enigo kun precizigita formo."
  (format stream "#<Node ~A>"
          (node-name node)))

(defstruct (arc (:print-function print-arc))
  (from nil)
  (to nil)
  (label nil)
  (action nil))

(defun print-arc (arc stream depth)
  "Eligu la la nomojn de la ‘arc-from’ kaj ‘arc-to’ de la eniga ‘arc’ kun precizigita formo."
  (format stream "#<ARC ~A / ~A / ~A>"
          (node-name (arc-from arc))
          (arc-label arc)
          (node-name (arc-to arc))))

(defvar *nodes*)
(defvar *arcs*)
(defvar *current-node*)

(defun initialize ()
  "Agordu je ‘*nodes*’, ‘*arcs*’, kaj ‘*current-node*’, por ke, ili enhavos je ‘nil’."
  (setf *nodes* nil)
  (setf *arcs* nil)
  (setf *current-node* nil))

(defmacro defnode (name)
  "Makroo kiu kreas novan nodon per la eniga nomo, per voki je ‘add-node’."
  `(add-node ',name))

(defun add-node (name)
  "Aldonu novan nodon al la ‘*nodes*’ listo."
  (let ((new-node (make-node :name name)))
    (setf *nodes* (nconc *nodes* (list new-node)))
    new-node))

(defun find-node (name)
  "Eligu la nodon kiu havas je ‘name’ kiel sia nomo, aŭ eligu erarmesaĝon se ne ekzistas."
  (or (find name *nodes* :key #'node-name)
      (error "No node named ~A exists." name)))

(defmacro defarc (from label to &optional action)
  "Makroo kiu funkcias simile al la ‘defnode’ makroo, tamen, por ‘arc’."
  `(add-arc ',from ',label ',to ',action))

(defun add-arc (from-name label to-name action)
  "Aldonu novan ‘arc’ al la ‘*arcs*’ listo, tiam, ankaŭ aldonu ĝin al la ‘node-outputs’ listo de la
  ‘from’ nodo, kaj al la ‘node-inputs’ listo de la ‘to’ nodo."
  (let* ((from (find-node from-name))
         (to (find-node to-name))
         (new-arc (make-arc :from from
                            :label label
                            :to to
                            :action action)))
    (setf *arcs* (nconc *arcs* (list new-arc)))
    (setf (node-outputs from)
          (nconc (node-outputs from)
                 (list new-arc)))
    (setf (node-inputs to)
          (nconc (node-inputs to)
                 (list new-arc)))
    new-arc))

(defun fsm (&optional (starting-point 'start))
  "Iteraciu daŭre per voki je ‘one-transition’, dum agordi je ‘*current-node*’ al la opcionala
  ‘starting-point’ enigo."
  (setf *current-node* (find-node starting-point))
  (do ()
      ((null (node-outputs *current-node*)))
    (one-transition)))

(defun one-transition ()
  "Demandu por enigo, tiam, iru al la respektiva celo depende je la enigo, per ŝanĝi la valoron de
  ‘*current-node*’."
  (format t "~&State ~A. Input: "
          (node-name *current-node*))
  (let* ((ans (read))
         (arc (find ans
                    (node-outputs *current-node*)
                    :key #'arc-label)))
    (unless arc
      (format t "~&No arc from ~A has label ~A.~%"
              (node-name *current-node*) ans)
      (return-from one-transition nil))
    (let ((new (arc-to arc)))
      (format t "~&~A" (arc-action arc))
      (setf *current-node* new))))

;;; ekzerco 14,11 a
(defun start (input-syms
              &aux (this-input (first syms)))
  "Simulu la efekton de kuri la maŝinon per la integritaj enigoj."
  (cond ((null input-syms) 'start)
        ((equal this-input 'nickel)
         (format t "~&~A" "Clunk!")
         (have-5 (rest input-syms)))
        ((equal this-input 'dime)
         (format t "~&~A" "Clink!")
         (have-10 (rest input-syms)))
        ((equal this-input 'coin-return)
         (format t "~&~A" "Nothing to return.")
         (start (rest input-syms)))
        (t (error "No arc from ~A with label ~A."
                  ’start this-input))))

(defun compile-arc (arc)
  "Eligu je ‘cond’ klaŭzon kiu havas la demanditan formon simile al la klaŭzo de la antaŭa ‘start’
  funkcio."
  `((equal this-input ',(arc-label arc))
    (format t "~&~A" ,(arc-action arc))
    (,(node-name (arc-to arc))
      (rest input-syms))))

;;; ekzerco 14,11 b
(defun compile-node (node)
  "Eligu je ‘defun’ esprimon de la eniga nodo, simile al la ‘defun’ esprimo de la antaŭa ‘start’
  funkcio."
  (let ((unu (mapcar #'(lambda (n) (compile-arc n))
                     (node-outputs node))))
    `(defun ,(node-name node) (input-syms &aux (this-input (first input-syms)))
       (cond ((null input-syms) ',(node-name node))
             ,@unu
             (t (error "No arc from ~A with label ~A."
                       ',(node-name node) this-input))))))

;;; ekzerco 14,11 c
(defmacro compile-machine ()
  "Kreu je ‘progn’ esprimon por kuri multe da ‘defun’ esprimo per la enhavaj nodoj de ‘*nodes*’."
  `(progn ,@(mapcar #'(lambda (n) (compile-node n))
                    *nodes*)))

