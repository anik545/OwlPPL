let* dice1 = uniform [1;2;3;4;5;6] in
let* dice2 = uniform [1;2;3;4;5;6] in
condition (dice1+dice2 = 4)
	(return dice1)
