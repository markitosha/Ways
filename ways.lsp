;Маркитантова Наталья, 324 группа: поиск путей по карте дорог
;запуск через функцию (main l), где l - список рёбер

;поиск ребер в изначальном списке, соединенных с вершиной vert
(defun f-edge (edge vert)
	(cond
		((eq (car edge) vert) (cdr edge))
		((eq (cadr edge) vert) (cons (car edge) (cddr edge)))
		(T nil)
	)
)

;создание списка из n одинаковых элементов el
(defun m-eq-list (n el)
	(cond
		((= n 0) nil)
		(T (cons el (m-eq-list (- n 1) el)))
	)
)

;поиск вершины vert в графе и добавление ее и ее связей в список
(defun f-vertex (vert graph vertex)
	(cond
		((find vert vertex) nil)
		(T (list vert (remove nil (mapcar #'f-edge graph (m-eq-list (length graph) vert)))))
	)
)

;создание графа из списка ребер
(defun  m-graph (graph vertex)
	(cond
		((null graph) nil)
		(T  (let ((a (caar graph)) (b (cadar graph)) (v vertex) (g graph))
				(append
					(append 
						(f-vertex a g v)
						(f-vertex b g v)
					)
					(m-graph (cdr g) (append (list a b) v))
				)
			)
		)
	)
)

;определение параметров при прохождении пути
(defun ch-par (par el path)
	(list
		(cond
			((eq (nth 2 par) 2) path)
			((eq (nth 2 par) (nth 1 el)) path)
			(T (cons "change" path))
		)
		(+ (nth 0 par) (nth 2 el))
		(cond
			((eq (nth 2 par) 2) 0)
			((eq (nth 2 par) (nth 1 el)) (nth 1 par))
			(T (+ (nth 1 par) 1))
		)
		(nth 1 el)
	)
)

;переход к следующему элементу в пути
(defun next (next path b graph par) 
	(cond
		((null next) nil)
		(T (let ((x (ch-par par (car next) path)))
			(append
				(path (caar next) b graph (car x) (cdr x))
				(next (cdr next) path b graph par)
			)
		   )
		)
	)
)

;вспомогательная рекурсивная функция для создания
;всех возможных путей между городами и подсчетa их параметров
(defun path (this b graph path par)
	(cond
		((eq this b) (list (cons (butlast par 1) (reverse (cons b path)))))
		((member this path) ())
		(T (next (cadr (member this graph)) (cons this path) b graph par))
	)
)

;создание всех возможных путей между городами и подсчет их параметров
(defun way (a b graph)
	(path a b graph () '(0 0 2)) 
)

;сранение по длине пути
(defun s-way-len (a b)
	(cond
		((> (caar a) (caar b)) nil)
		(T)
	)
)

;удаление из списка путей всех путей, превышающих длину минимального более чем в 1,5 раза
(defun min-len (way)
	(remove-if #'(lambda (x) (< (- (* 1.5 (caaar way)) 0) (caar x))) way)
)

;сравнение по пересадкам
(defun s-way-ch (a b)
	(cond
		((>= (cadar a) (cadar b)) nil)
		(T)
	)
)

;выбор оптимальных путей (с пересадками не больше 2 и оптимальных по длине)
;закомментированая строка - выбор самых оптимальных путей
(defun the-best (way)
	(cond
		((eq way nil) way)
		((eq (cdr way) nil) way)
;		((not (s-way-ch (car way) (cadr way))) (cons (car way) (the-best (cdr way))))
		((<= (cadaar way) 2) (cons (car way) (the-best (cdr way))))
		((cons (car way) ()))
	)
)

;сортировка списка путей по оптимальности (по длине и пересадкам), вывод минимального
(defun f-best (way)
	(let ((x (min-len (sort way #'s-way-len))))
		(beauty-print (car x) 'MIN)
		(the-best (sort x #'s-way-ch))
	)
)

;главная функция
(defun main (graph)
	(print-way
		(let ((x (way (read) (read) (m-graph graph ()))))
			(f-best x)
		)
	)
)

;создание списка чисел от n до 1
(defun m-list (n)
	(cond
		((eq n 0) ())
		(T (cons n (m-list (- n 1))))
	)
)

;красивый вывод одного пути, n - идентификатор пути
(defun beauty-print (way n)
	(print 'WAY)
	(princ n)
	(princ ":")
	(print (cdr way))
	(print "	LENGTH:")
	(princ (caar way))
	(print "	CHANGES:")
	(princ (cadar way))
)

;вывод на экран списка путей
(defun print-way (way)
	(mapcar #'beauty-print way (reverse (m-list (length way))))
)

(print '>)
(main (read))
