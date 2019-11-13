program scantron
implicit none 
	! declare variables
   	integer :: student_count, number_of_questions, score, average, i
   	integer,allocatable,dimension(:) :: answer_key
   	integer,allocatable,dimension(:) :: student_answers
   	integer,allocatable,dimension(:, :) :: student_scores
   	integer,dimension(0:100) :: score_frequency
   	! opens the file with the test data
   	character (len = 100) :: filename
   	print *,"Enter filename: " 
	read *, filename
	open(5, file=filename, status='old', action="read")
	! stores the first value, which is the number of questions 
   	read(5, *) number_of_questions
   	! initilize answer_key array and student_answers array with the size of number_of_questions
   	allocate(answer_key(number_of_questions), student_answers(number_of_questions)) 
   	! stores the answer key
   	call read_data(answer_key, number_of_questions, .true.)
   	! find the number of students
   	student_count=0
10	read(5, *, end=15) 
	student_count=student_count+1
	goto 10
	! resets the file back to the beginning
15	if(student_count == 0) then
		print *, "there are no test to grade"
		stop
	end if
	rewind(5) 	 	
	! initialize student_scores array with the size of students in the class
	! first index is student id, second is their score
	allocate(student_scores(student_count, 2))
	! skips first 2 lines of file
	read(5,*)
	read(5,*)
	! initialize all the index of the score_frequency array to 0
	score_frequency = 0
   	i = 1
   	! saves the the studends id in the first index 
20 read(5,*, end=25) student_scores(i,1)
	! resets the file curser back one line
	backspace(5)
	! stores that students answers to the student_answer array
	call read_data(student_answers, number_of_questions, .false.)
	! call the score function and save the result in the second index 
	student_scores(i,2) = score(answer_key, student_answers, number_of_questions)
	! increment the score frequency
	score_frequency(student_scores(i,2)) = score_frequency(student_scores(i,2)) + 1
	i = i + 1
	! repeat till end of file
	goto 20
	! print results
25	write(*, 35) "Student ID", "Score"
	write(*, 30) "========================="
	do i = 1, student_count
		write(*, 45) student_scores(i, 1), student_scores(i, 2)
	end do  
	write(*, 30) "========================="
	write(*, 40) "test graded = ", student_count
	write(*, 30) "========================="
	write(*, 35)  "score", "frequency"
	write(*, 30) "========================="
	do i = 100, 0, -1
		if(score_frequency(i) > 0) then
			write(*, 50) i, score_frequency(i)
		end if 
	end do 
	write(*, 30) "========================="
	write(*, 40) "class average = ", average(student_scores, student_count)
	write(*, 30) "========================="
	! clean up memory
	deallocate(answer_key)
	deallocate(student_answers)
	deallocate(student_scores)
	! formatting statements
30  format(a20)
35 	format(1x, a, 5x, a)
40 	format(1x, a, i3)
45	format(i6, 8x, i5)
50	format(i4.2, 9x, i3)
	! close the test file
	close(5)
end program scantron


! a function that returns the grade of a student
! answer is the array for the answer key
! response is the array of student answers
! size is the number of questions
function score(answers, response, size) 
implicit none
	integer :: score, size, i
	integer, dimension(size) :: answers
	integer, dimension(size) :: response
	score = 0
	i = 1
	do i = 1, size
		if(answers(i) == response(i)) then
			score = score + 1 
		end if
	end do 
	score = score * 100
	score = score / size
end function score


! reads one line of the file
! arr is the array that is saved
! answer_key is bool that tells the program 
! that there is no student id in the first line 
subroutine read_data(arr, size, answer_key) 
implicit none 
	integer :: size, i, id
	integer, dimension(size) :: arr
	logical :: answer_key
	if(answer_key) then
		read(5, *) (arr(i), i = 1, size)
	else 
		read(5, *) id, (arr(i), i=1, size)
	end if 
end subroutine read_data


! a function that returns the average of the class
function average(arr, count)
	integer :: average, i, total_sum, count
	integer, dimension(count, 2) :: arr
	total_sum = 0
	do i = 1, count
		total_sum = total_sum + arr(i, 2)
	end do 
	average = total_sum/count
end function average