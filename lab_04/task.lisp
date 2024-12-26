;;; 3. Написать, по крайней мере, два варианта функции, которая возвращает
;;; последний элемент своего списка-аргумента.

(defun my-last-1 (lst)
  (car (reverse lst)))

(defun my-last-2 (lst)
  (cond ((not lst) nil)
        ((not (cdr lst)) (car lst))
        ((my-last-2 (cdr lst)))))

;;; 4. Написать, по крайней мере, два варианта функции, которая возвращает
;;; свой список аргумент без последнего элемента.

(defun no-last-1 (lst)
  (cond ((not lst) nil)
        ((not (cdr lst)) nil)
        ((cons (car lst) (no-last-1 (cdr lst))))))

(defun no-last-2 (lst)
  (reverse (cdr (reverse lst))))

;;; 5. Напишите функцию swap-first-last, которая переставляет в списке-
;;; аргументе первый и последний элементы.

(defun reverse-set-val (lst val)
  (rplaca (nreverse lst) val))

(defun swap-first-last-1 (lst)
  (let ((last-v (car (last lst))))
    (reverse-set-val (reverse-set-val lst (car lst)) last-v)))

(defun set-last (lst val)
  (cond ((not lst) nil)
        ((not (cdr lst)) (let ((tmp (car lst))) (setf (car lst) val) tmp))
        ((set-last (cdr lst) val))))

(defun swap-first-last-2 (lst)
  (rplaca lst (set-last lst (car lst))))

;;; 6. Написать простой вариант игры в кости, в котором бросаются две
;;; правильные кости. Если сумма выпавших очков равна 7 или 11 —
;;; выигрыш, если выпало (1,1) или (6,6) — игрок имеет право снова
;;; бросить кости, во всех остальных случаях ход переходит ко второму
;;; игроку, но запоминается сумма выпавших очков. Если второй игрок не
;;; выигрывает абсолютно, то выигрывает тот игрок, у которого больше
;;; очков. Результат игры и значения выпавших костей выводить на экран с
;;; помощью функции print.

(defun get-random-pair () (list (+ (random 6) 1) (+ (random 6) 1)))

(defun turn ()
  (let* ((pair (get-random-pair)) (sum (+ (car pair) (cadr pair))))
    (print "Выпавшие очки:")
    (print pair)
    (cond ((or (= 1 (first pair) (second pair))
               (= 6 (first pair) (second pair)))
           (print "Перебросить? [y/any]: ")
           (if (eql 'y (progn (finish-output) (read t))) (turn) sum))
          (sum))))

(defun choose-winner (turn-1 turn-2)
  (cond ((or (= 7 turn-1) (= 11 turn-1)) 1)
        ((or (= 7 turn-2) (= 11 turn-2)) 2)
        ((> turn-1 turn-2) 1)
        ((< turn-1 turn-2) 2)
        (0)))

(defun game ()
  (print "ИГРА В КОСТИ")
  (print "------------------------")
  (let ((win (choose-winner
               (progn (print "Игрок 1") (turn))
               (progn (print "------------------------")
                      (print "Игрок 2") (turn)))))
    (print "------------------------")
    (cond ((zerop win) (print "Ничья") nil)
          ((= 1 win) (print "Победитель - игрок 1") nil)
          ((= 2 win) (print "Победитель - игрок 2") nil))))

;;; 7. Написать функцию, которая по своему списку-аргументу lst определяет
;;; является ли он палиндромом (то есть равны ли lst и (reverse lst)).

(defun palindrome (lst) (equal lst (reverse lst)))

;;; 8. Напишите свои необходимые функции, которые обрабатывают таблицу из
;;; 4-х точечных пар: (страна . столица), и возвращают по стране - столицу,
;;; а по столице — страну.

(defun get-capital (table country)
  (cond ((not table) nil)
        ((equal (caar table) country) (cdar table))
        ((get-capital (cdr table) country))))

(defun get-country (table capital)
  (cond ((not table) nil)
        ((equal (cdar table) capital) (caar table))
        ((get-country (cdr table) capital))))

(defun set-capital-inner (table country capital)
  (cond ((equal (caar table) country) (rplacd (car table) capital))
        ((not (cdr table)) (cadr (rplacd table (list (cons country capital)))))
        ((set-capital-inner (cdr table) country capital))))

(defun set-capital (table-name country capital)
  (let ((table (eval table-name)))
    (cond ((not table) (car (setf (symbol-value table-name) (list (cons country capital)))))
          ((set-capital-inner table country capital)))))

(setf table nil)
(set-capital 'table 'Нидерланды 'Амстердам)
(set-capital 'table 'Андорра 'Андорра-ла-Велья)
(set-capital 'table 'Греция 'Афины)
(set-capital 'table 'Сербия 'Белград)
(set-capital 'table 'Германия 'Берлин)
(set-capital 'table 'Швейцария 'Берн)
(set-capital 'table 'Словакия 'Братислава)

;;; 9. Напишите функцию, которая умножает на заданное число-аргумент
;;; первый числовой элемент списка из заданного 3-х элементного списка-
;;; аргумента, когда
;;; a) все элементы списка --- числа,
;;; 6) элементы списка -- любые объекты.

(defun mult-first-num-inner (lst val)
  (cond ((not lst) nil)
        ((numberp (car lst)) (rplaca lst (* (car lst) val)))
        ((mult-first-num (cdr lst) val))))

(defun mult-first-num (lst val)
  (mult-first-num-inner lst val)
  lst)

