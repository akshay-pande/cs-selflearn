(defun square(a) ( * a a))

(defun iseven(a) (cond ((= (mod a 2) 0 ) 1) (t 0)))

(defun fast_exp_i(a count b n) (
cond 	
	 ((= count 0) (fast_exp_i (* a b) (+ count 1) b n )) 
	 ((and (= (+ count 1) n) (= (iseven n) 0))  (* b a)) ;special case for second last counter of odd n
     ((and (< count n) (= (iseven (floor n (+ count 1))) 1)) (fast_exp_i (* a b) (+ count 1) b n ))		;main recursive case 1
	 ((and (< count n) (= (iseven (floor n (+ count 1))) 0)) (fast_exp_i (square a) (* count 2) b n ))	 ;main recursive case 2
	 ((or (= count n) (> count n))  a) ;final output
	 
)) 

(defun fast_exp(b n) (fast_exp_i 1 0 b n))