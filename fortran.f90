	function checkInput(in) result(output)!determines if input is betwen 1 and 99 
                if (in > 99) then 
                        write(*,*) 'input greater than 99, ERROR'
                        output = 1
                else if (in < 1) then
                        write(*,*) 'input smaller than 1, ERROR'
                        output = 1
		else 
			output = 0
                end if
        end function checkInput
	
	function countQ(input) result(output) !counts quarters
		integer :: temp = 0
		integer :: quarters = 0
		do while (input >= 25) !while ther is room for quarter subtract 25 and add to quarter count 
			input = input - 25
			quarters = quarters + 1
		end do
		output = quarters !send back quarter count
	end function countQ

	function countD(input) result(output)
		integer :: dimes = 0
		do while (input >= 10)!while theres room for dim subtract 10 and add to dime count
			input = input - 10
			dimes = dimes +1
		end do
		output = dimes !send back dime count
	end function countD

        function countN(input) result(output)
                integer :: nickels = 0
                do while (input >= 5)!while there is room for nickel subtract 5 and add to nickel count
                        input = input - 5
                        nickels = nickels +1
                end do
		output = nickels!return nickel count
        end function countN

        function countP(input) result(output)
                integer :: pennies = 0
                do while (input >= 1)!while room for penny subtract 1 and add to pennies(yes i realize i could just set penny equal to input)
                        input = input - 1
                        pennies = pennies +1
                end do
		output = pennies
        end function countP

	subroutine getDateAndTime()
		IMPLICIT NONE
		character(8) :: date 
		character(10) :: time, otherTime*12
		character(4) :: year, month*2, day*2
		character(2) :: hour, minute, second*6
		call date_and_time(date, time)!get date and time info from system
		year = date(1:4) !parse year out(first 4 digits)
		month = date(5:6)!parse month out(5th and 6th digits)
		day = date(7:8)!parse day out (7th and 8th digit)
		hour = time(1:2)!parse hour (first and second digit from time)
		minute = time(3:4)!parse minute(3rd and 4th digit from time)
!		second = time(5:10)!unused second parse
		write(*,*)month, "-", day, "-", year, ", ", hour, ":", minute!print date & time
	end subroutine getDateAndTime

	Program program1
		IMPLICIT NONE!i did this for safe code 
		real :: checkInput!check func
		real :: countQ!quartersfunc
		real :: countD!dimes func	
		real :: countN!nickels func
		real :: countP!pennies func
		integer, dimension(4)  :: cArray!array that holds each coin [quarter, dime, nickel, penny]
		integer :: input = 0!user input initialized at zero (not valid)
		integer :: check = 1!flag
		call getDateAndTime()!subroutine call		
		cArray = (/0, 0, 0, 0/)!initialize array at all zeros
		do while (check > 0)!loops untill good integer is recieved by read
			write(*,*)'Please enter integer amount between 1 and 99'!
			read(*,*) input
			check = checkInput(input)
		end do
		cArray(1) = countQ(input)!count quarters first
		cArray(2) = countD(input)!count dimes second
		cArray(3) = countN(input)!count nickels third
		cArray(4) = countP(input)!count pennies fourth and last
		if (cArray(1) > 0) then !if ther are quarters print amount
			write(*,"(I1)", ADVANCE = 'NO') cArray(1) !print integer up to length to
			write(*, "(A9)", ADVANCE = 'NO') " quarter " !print 8 chars 
		end if
		if (cArray(2) > 0) then
			write(*,"(I1)", ADVANCE = 'NO') cArray(2)!2 ints
			write(*, "(A6)", ADVANCE = 'NO') " dime "!5 chars
		end if
		if (cArray(3) > 0) then
			write(*,"(I1)", ADVANCE = 'NO') cArray(3)!2 ints
			write(*, "(A8)", ADVANCE = 'NO') " nickel "!7 chars
		end if
		if (cArray(4) > 0) then
			write(*,"(I1)", ADVANCE = 'NO') cArray(4)!2 ints
			write(*, "(A7)", ADVANCE = 'NO') " penny "!print 6 chars
		end if
		write(*,*)!new line to make program look cleaner
!		end if

	end Program program1
